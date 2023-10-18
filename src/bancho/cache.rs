use super::{User, UserId};
use std::collections::{HashMap, VecDeque};

use std::sync::{Arc, RwLock};

/// [`UserCache`] is not designed to be operated over tokio tasks, so we are not
/// using primitives in [`tokio::sync`] here.
type GuardedUser = Arc<RwLock<User>>;

#[derive(Debug)]
pub struct UserCache {
    capacity: usize,
    id_map: HashMap<UserId, GuardedUser>,
    name_map: HashMap<String, GuardedUser>,
    queue: VecDeque<GuardedUser>,
}

fn merge_name(old: impl AsRef<str>, new: impl AsRef<str>) -> String {
    let old_str = old.as_ref();
    let new_str = new.as_ref();
    if old_str.len() != new_str.len() {
        panic!(
            "merged names \"{}\" and \"{}\" must have the same length",
            old_str, new_str
        )
    } else {
        std::iter::zip(old_str.chars(), new_str.chars())
            .enumerate()
            .map(|(i, (o, n))| {
                if n == '_' {
                    o
                } else if o == '_' {
                    n
                } else if n == o {
                    n
                } else if i == 0 && (n.is_ascii_lowercase() && o.is_ascii_uppercase()) {
                    n
                } else {
                    o
                }
            })
            .collect()
    }
}

fn merge_user(old: &mut User, new: &User) {
    if !old.has_id() && new.has_id() {
        old.id = new.id;
    }
    if new.has_name() {
        if !old.has_name() {
            old.name = new.name().to_string();
        } else {
            old.name = merge_name(&old.name, &new.name);
        }
    }
}

/// This cache is used to store user information.
///
impl UserCache {
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity,
            id_map: HashMap::new(),
            name_map: HashMap::new(),
            queue: VecDeque::with_capacity(capacity),
        }
    }
    #[inline]
    fn rearrange_queue(&mut self, user: GuardedUser) -> &GuardedUser {
        self.queue
            .iter()
            .rev()
            .position(|u| Arc::ptr_eq(u, &user))
            .map(|i| {
                let i = self.queue.len() - i - 1;
                self.queue.remove(i);
            });
        self.queue.push_back(user.clone());
        self.queue.back().unwrap()
    }
    fn ensure_capacity(&mut self) {
        if self.queue.len() > self.capacity {
            let eliminated = self.queue.pop_front().unwrap();
            let user = eliminated.read().unwrap();
            if let Some(id) = user.id() {
                self.id_map.remove(&id);
            }
            if user.has_name() {
                self.name_map.remove(&user.normalized_name());
            }
        }
    }
    pub fn put_id(&mut self, id: UserId, user: &User) -> GuardedUser {
        self.id_map
            .entry(id)
            .and_modify(|u| {
                let mut u = u.write().unwrap();
                merge_user(&mut u, &user);
            })
            .or_insert(Arc::new(RwLock::new(user.clone())))
            .clone()
    }
    pub fn put_name(&mut self, key: impl AsRef<str>, user: &User) -> GuardedUser {
        self.name_map
            .entry(key.as_ref().to_string())
            .and_modify(|u| {
                let mut u = u.write().unwrap();
                merge_user(&mut u, &user);
            })
            .or_insert(Arc::new(RwLock::new(user.clone())))
            .clone()
    }
    pub fn put(&mut self, user: User) -> Option<User> {
        let key = user.normalized_name();
        if let Some(id) = user.id() {
            let guarded = if self.id_map.contains_key(&id) {
                self.put_id(id, &user)
            } else if user.has_name() && self.name_map.contains_key(&key) {
                self.put_name(key, &user)
            } else {
                self.put_id(id, &user)
            };

            if !self.id_map.contains_key(&id) {
                self.id_map.insert(id, guarded.clone());
            }

            self.rearrange_queue(guarded.clone());
            self.ensure_capacity();

            guarded.read().map(|u| u.clone()).ok()
        } else if user.has_name() {
            let guarded = self.put_name(key, &user);

            self.rearrange_queue(guarded.clone());
            self.ensure_capacity();

            guarded.read().map(|u| u.clone()).ok()
        } else {
            None
        }
    }
    pub fn find_id(&mut self, id: UserId) -> Option<User> {
        match self.id_map.get(&id) {
            None => None,
            Some(guarded) => Some(
                self.rearrange_queue(guarded.clone())
                    .read()
                    .unwrap()
                    .clone(),
            ),
        }
    }
    pub fn find_name(&mut self, name: impl AsRef<str>) -> Option<User> {
        match self.name_map.get(&User::normalize(name)) {
            None => None,
            Some(guarded) => Some(
                self.rearrange_queue(guarded.clone())
                    .read()
                    .unwrap()
                    .clone(),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn assert_queue_eq<'a>(queue: &VecDeque<GuardedUser>, iter: impl IntoIterator<Item = User>) {
        let users: Vec<_> = queue
            .iter()
            .map(|guarded| guarded.read().unwrap().clone())
            .collect();
        let expected: Vec<_> = iter.into_iter().collect();
        assert_eq!(
            users.iter().eq(expected.iter()),
            true,
            "left: {:#?} right: {:#?}",
            users,
            expected,
        )
    }
    #[test]
    fn user_cache_simple() {
        let mut cache = UserCache::new(4);

        assert_eq!(cache.find_name("abc d"), None);
        assert_eq!(cache.find_id(1234), None);

        cache.put(User::name_only("ABC_D"));
        assert_eq!(cache.queue.len(), 1);
        assert_eq!(cache.find_name("abc d").unwrap().name(), "ABC_D");

        cache.put(User::new(1234, "aBC D"));
        assert_eq!(cache.queue.len(), 1);
        assert_eq!(cache.find_name("abc d").unwrap().name(), "aBC D");
        assert_eq!(cache.find_id(1234).unwrap().name(), "aBC D");

        cache.put(User::name_only("user_1"));
        cache.find_name("abc d");
        cache.put(User::name_only("user_2"));
        cache.put(User::name_only("user_3"));
        assert_eq!(cache.queue.len(), 4);
        cache.put(User::name_only("user_4"));
        assert_eq!(cache.queue.len(), 4);

        assert_queue_eq(
            &cache.queue,
            [
                User::name_only("aBC D"),
                User::name_only("user_2"),
                User::name_only("user_3"),
                User::name_only("user_4"),
            ],
        );
    }
}
