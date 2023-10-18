macro_rules! regexset_enum {
    // `{let _ = &$field;}` is to ensure variables are used.
    (@block_matches $captures:ident $field:ident $(: $T:ty)?, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field));
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_matches $captures:ident) => {};

    (@block_variables @m $captures:ident $field:ident : $T:ty, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field));
        {let _ = &$field;}
        regexset_enum!(@block_variables @m $captures $($t)*)
    };
    (@block_variables @m $captures:ident) => {};
    (@block_variables @mstr $captures:ident $field:ident : $T:ty, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field)).map(|m| m.as_str()).unwrap_or("");
        {let _ = &$field;};
        regexset_enum!(@block_variables @mstr $captures $($t)*)
    };
    (@block_variables @mstr $captures:ident) => {};
    (@block_variables $captures:ident $field:ident : Option<$T:ty>, $($t:tt)*) => {
        let $field: Option<$T> = $captures.name(stringify!($field)).map(|v| v.as_str().parse()).transpose()?;
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_variables $captures:ident $field:ident : bool, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field)).is_some();
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_variables $captures:ident $field:ident : &str, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field)).map(|m| m.as_str()).unwrap_or_default();
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_variables $captures:ident $field:ident : $T:ty, $($t:tt)*) => {
        let $field: $T = $captures.name(stringify!($field)).map(|m| m.as_str().parse()).transpose()?.unwrap_or_default();
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_variables $captures:ident $field:ident, $($t:tt)*) => {
        let $field = $captures.name(stringify!($field));
        {let _ = &$field;}
        regexset_enum!(@block_variables $captures $($t)*)
    };
    (@block_variables $captures:ident) => {};

    (@block $captures:ident [m] ($($t:tt)*)) => {
        regexset_enum!(@block_variables @m $captures $($t)*);
    };
    (@block $captures:ident [m, $($helpers:tt)*] ($($t:tt)*)) => {
        regexset_enum!(@block_variables @m $captures $($t)*);
        regexset_enum!(@block_variables $captures $($helpers)*);
    };
    (@block $captures:ident [mstr] ($($t:tt)*)) => {
        regexset_enum!(@block_variables @mstr $captures $($t)*);
    };
    (@block $captures:ident [mstr, $($helpers:tt)*] ($($t:tt)*)) => {
        regexset_enum!(@block_variables @mstr $captures $($t)*);
        regexset_enum!(@block_variables $captures $($helpers)*);
    };
    (@block $captures:ident [v] ($($t:tt)*)) => {
        regexset_enum!(@block_variables $captures $($t)*);
    };
    (@block $captures:ident [v, $($helpers:tt)*] ($($t:tt)*)) => {
        regexset_enum!(@block_variables $captures $($t)*);
        regexset_enum!(@block_variables $captures $($helpers)*);
    };
    (@block $captures:ident [$($helpers:tt)*] ($($t:tt)*)) => {
        regexset_enum!(@block_variables $captures $($helpers)*);
    };

    (@arms $captures:ident $target:ident @($counter:expr, $Variant:ident { $($t:tt)* } $([ $($helpers:tt)* ])? => {$($block:tt)*}, $($tail:tt)*) $($arms:tt)*) => {
        regexset_enum! {
            @arms
            $captures
            $target
            @(
                $counter + 1,
                $($tail)*
            )
            $($arms)*
            index if index == $counter => {
                regexset_enum!(@block $captures [$($($helpers)*)?] ($($t)*));
                $($block)*
            },
        }
    };
    (@arms $captures:ident $target:ident @($counter:expr, $Variant:ident { $($t:tt)* }, $($tail:tt)*) $($arms:tt)*) => {
        regexset_enum! {
            @arms
            $captures
            $target
            @(
                $counter + 1,
                $($tail)*
            )
            $($arms)*
            index if index == $counter => regexset_enum!(@build $captures @($Variant, $($t)*)),
        }
    };
    (@arms $captures:ident $target:ident @($counter:expr, $Variant:ident, $($tail:tt)*) $($arms:tt)*) => {
        regexset_enum! {
            @arms
            $captures
            $target
            @(
                $counter + 1,
                $($tail)*
            )
            $($arms)*
            index if index == $counter => Self::$Variant,
        }
    };
    (@arms $captures:ident $target:ident  @($counter:expr $(,)?) $($arms:tt)*) => {
        match $target {
            $($arms)*
            _ => unreachable!(""),
        }
    };
    (@build $captures:ident @($Variant:ident, $field:ident : Option<$t:ty>, $($tail:tt)*) $($builder:tt)*) => {
        regexset_enum! {
            @build
            $captures
            @($Variant, $($tail)*)
            $($builder)*
            $field: $captures.name(stringify!($field)).map(|m| m.as_str().parse()).transpose()?,
        }
    };
    (@build $captures:ident @($Variant:ident, $field:ident : bool, $($tail:tt)*) $($builder:tt)*) => {
        regexset_enum! {
            @build
            $captures
            @($Variant, $($tail)*)
            $($builder)*
            $field: $captures.name(stringify!($field)).is_some(),
        }
    };
    (@build $captures:ident @($Variant:ident, $field:ident : $t:ty, $($tail:tt)*) $($builder:tt)*) => {
        regexset_enum! {
            @build
            $captures
            @($Variant, $($tail)*)
            $($builder)*
            $field: $captures.name(stringify!($field)).map(|m| m.as_str().parse()).transpose()?.unwrap_or_default(),
        }
    };
    (@build $captures:ident @($Variant:ident $(,)?) $($builder:tt)*) => {
        Self::$Variant {
            $($builder)*
        }
    };
    (@trim $($t:tt)* $(,)?) => {
        $($t)*
    };
    (@debug @($Variant:ident { $($t:tt)* } => $pattern:literal {$($block:tt)*}, $($tail:tt)*)) => {
        $($block)*
    };
    (
        $(#[$outer:meta])*
        $vis:vis enum $Name:ident {
            $( $Variant:ident $({ $($t:tt)* })? $([ $($helpers:tt)* ])? => $pattern:literal $({
                $($block:tt)*
            })?),+ $(,)?
        }
    ) => {
        $(#[$outer])*
        $vis enum $Name {
            $($Variant $({ $($t)* })?,)*
        }

        impl $Name {
            pub fn pattern(&self) -> Option<&'static str> {
                match self {
                    $(Self::$Variant {..} => Some($pattern),)*
                }
            }
            pub fn patterns() -> &'static [&'static str] {
                &[$($pattern),*]
            }
            pub fn from_captures(index: usize, captures: regex::Captures) -> std::result::Result<Self, Box<dyn std::error::Error + Send + Sync>> {
                Ok(regexset_enum! {
                    @arms
                    captures
                    index
                    @(
                        0usize,
                        $($Variant $({ $($t)*, })? $([ $($helpers)*, ])? $(=> {$($block)*})?,)*
                    )
                })
            }
        }
    };
}

pub(crate) use regexset_enum;
