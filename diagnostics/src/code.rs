#[macro_export]
macro_rules! error_codes {
    ($ty:ident { $($input:tt)* }) => { 
        error_codes!(@munch ($($input)*) -> {$ty});
    };
    
    (@munch () -> {
        $ty:ident
        $((
            $item:ident,
            $severity:ident,
            $msg:expr,
            ($(($field:ident : $type:ty))*),
            $labels:expr
        ))*
    }) => {
        #[derive(Debug)]
        pub enum $ty {
            $(
                $item {
                    span: $crate::span::Span,
                    $($field : $type),*
                }
            ),*
        }
        
        impl $ty {
            pub fn span(&self) -> $crate::span::Span {
                match self {
                    $(
                        $ty::$item { span, .. } => *span
                    ),*
                }
            }
        }
        
        impl From<$ty> for $crate::diagnostic::Diagnostic {
            fn from(src: $ty) -> $crate::diagnostic::Diagnostic {
                match src {
                    $(
                        $ty::$item { span, $( ref $field),* } => {
                            $crate::diagnostic::Diagnostic::new(
                                $crate::diagnostic::Severity::$severity,
                                span,
                                $msg,
                                $labels
                            )
                        }
                    ),*
                }
            }
        }
        
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                        $ty::$item { span, $( ref $field ),* } => write!(f, "{}", $msg)
                    ),*
                }
            }
        }
    };
    
    (@munch ($item:ident {
        $severity:ident,
        $msg:literal [$($mfield:ident),*]
    }) -> {$($output:tt)*}) => {
        error_codes!(@munch () -> {$($output)* ($item, $severity, format!($msg, $($mfield),*), (), Vec::new())});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        $msg:literal [$($mfield:ident),*]
    }, $($next:tt)*) -> {$($output:tt)*}) => {
        error_codes!(@munch ($($next)*) -> {$($output)* ($item, $severity, format!($msg, $($mfield),*), (), Vec::new())});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*]
    }) -> {$($output:tt)*}) => {
        error_codes!(@munch () -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            Vec::new()
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*]
    },$($next:tt)*) -> {$($output:tt)*}) => {
        error_codes!(@munch ($($next)*) -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            Vec::new()
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        info: $info:literal [$($ifield:expr),*]
    }) -> {$($output:tt)*}) => {
        error_codes!(@munch () -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Info,
                    message: Some(format!($info, $($ifield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        info: $info:literal [$($ifield:expr),*]
    }, $($next:tt)*) -> {$($output:tt)*}) => {
        error_codes!(@munch ($($next)*) -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Info,
                    message: Some(format!($info, $($ifield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        help: $help:literal [$($hfield:expr),*]
    }) -> {$($output:tt)*}) => {
        error_codes!(@munch () -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Help,
                    message: Some(format!($help, $($hfield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        help: $help:literal [$($hfield:expr),*]
    }, $($next:tt)*) -> {$($output:tt)*}) => {
        error_codes!(@munch ($($next)*) -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Help,
                    message: Some(format!($help, $($hfield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        info: $info:literal [$($ifield:expr),*],
        help: $help:literal [$($hfield:expr),*]
    }) -> {$($output:tt)*}) => {
        error_codes!(@munch () -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Info,
                    message: Some(format!($info, $($ifield),*)),
                    span: Default::default()
                },
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Help,
                    message: Some(format!($help, $($hfield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
    
    (@munch ($item:ident {
        $severity:ident,
        [$($field:ident : $type:ty),*],
        $msg:literal [$($mfield:expr),*],
        info: $info:literal [$($ifield:expr),*],
        help: $help:literal [$($hfield:expr),*]
    }, $($next:tt)*) -> {$($output:tt)*}) => {
        error_codes!(@munch ($($next)*) -> {$($output)* (
            $item,
            $severity,
            format!($msg, $($mfield),*),
            ($(($field : $type))*),
            vec![
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Info,
                    message: Some(format!($info, $($ifield),*)),
                    span: Default::default()
                },
                $crate::diagnostic::Label {
                    severity: $crate::diagnostic::Severity::Help,
                    message: Some(format!($help, $($hfield),*)),
                    span: Default::default()
                }
            ]
        )});
    };
}