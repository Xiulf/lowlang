#![recursion_limit="128"]

extern crate proc_macro;

use proc_macro2::TokenStream;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[derive(Debug)]
struct Opts {
    severity: syn::Ident,
    msg: (syn::LitStr, Vec<syn::Ident>),
    labels: Vec<(Option<(syn::LitStr, Vec<syn::Ident>)>, syn::Ident, TokenStream)>
}

#[proc_macro_derive(ToDiagnostic, attributes(info, label))]
pub fn derive_to_diagnostic(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let variants = if let syn::Data::Enum(syn::DataEnum { ref variants, .. }) = ast.data {
        variants
    } else {
        return syn::Error::new(ast.span(), "expected an enum").to_compile_error().into();
    };
    
    let cases = variants.iter().map(|v| process_variant(name, v));
    
    let out = quote!{
        impl diagnostics::diagnostic::ToDiagnostic for #name {
            fn to_diagnostic(&self) -> diagnostics::diagnostic::Diagnostic {
                match self {
                    #(#cases),*
                }
            }
        }
    };
    
    out.into()
}

fn process_variant(typ: &syn::Ident, v: &syn::Variant) -> TokenStream {
    let v_name = &v.ident;
    let mut field_count = 0;
    let pattern = if let syn::Fields::Unnamed(ref fields) = v.fields {
        field_count = fields.unnamed.len();
        
        let names = (0..field_count).map(|i| {
            let s = format!("field_{}", i);
            
            syn::Ident::new(&s, fields.unnamed[i].span())
        });
        
        quote!{
            #v_name( #(#names),* )
        }
    } else if let syn::Fields::Named(ref fields) = v.fields {
        field_count = fields.named.len();
        
        let names = (0..field_count).map(|i| {
            let n = fields.named[i].ident.clone().unwrap();
            let s = format!("field_{}", i);
            let s = syn::Ident::new(&s, fields.named[i].span());
            
            quote!{ #n: #s }
        });
        
        quote!{
            #v_name{ #(#names),* }
        }
    } else {
        return syn::Error::new(v.span(), "enum variant must have at least one field").to_compile_error().into();
    };
    
    let opts = Opts::from(&v.attrs);
    
    let msg = {
        let msg = &opts.msg.0;
        let args = &opts.msg.1;
        
        if args.len() >= field_count {
            panic!("too many arguments applied to message");
        }
        
        quote!{
            format!(#msg, #(#args),*)
        }
    };
    
    let severity = &opts.severity;
    let label_severity = opts.labels.iter().map(|l| &l.1);
    let label_span = opts.labels.iter().map(|l| &l.2);
    let label_message = opts.labels.iter().map(|l| {
        if let Some((msg, args)) = &l.0 {
            quote!{ std::option::Option::Some(format!(#msg, #(#args),*)) }
        } else {
            quote!{ std::option::Option::None }
        }
    });
    
    quote!{
        #typ::#pattern => diagnostics::diagnostic::Diagnostic::new_derived(
            diagnostics::diagnostic::Severity::#severity,
            field_0.clone(),
            #msg,
            <[_]>::into_vec(std::boxed::Box::new([
                #(
                    diagnostics::diagnostic::Label {
                        severity: diagnostics::diagnostic::Severity::#label_severity,
                        span: #label_span.clone(),
                        message: #label_message
                    }
                ),*
            ]))
        )
    }
}

impl From<&Vec<syn::Attribute>> for Opts {
    fn from(src: &Vec<syn::Attribute>) -> Opts {
        let mut msg = None;
        let mut args = None;
        let mut severity = None;
        let mut labels = Vec::new();
        
        for attr in src {
            let meta = attr.parse_meta().expect("Expected a valid meta item");
            
            match meta {
                syn::Meta::List(ref list) if list.path.segments[0].ident == "info" => {
                    for nested in &list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                                if let syn::Lit::Str(val) = &nv.lit {
                                    let val_span = val.span();
                                    
                                    if nv.path.segments[0].ident == "msg" {
                                        msg = Some(val.clone());
                                    } else if nv.path.segments[0].ident == "args" {
                                        let val = val.value();
                                        let a = val.split(",")
                                            .map(|s| s.trim().parse::<usize>().expect("#[info(args)] only accepts integers"))
                                            .map(|n| format!("field_{}", n))
                                            .map(|n| syn::Ident::new(&n, val_span.clone()))
                                            .collect::<Vec<_>>();
                                        
                                        args = Some(a);
                                    } else if nv.path.segments[0].ident == "sev" {
                                        let ident = syn::Ident::new(&val.value(), val_span);
                                        
                                        severity = Some(ident);
                                    } else {
                                        panic!("#[info] only accepts 'msg', 'args' and 'sev'")
                                    }
                                } else {
                                    panic!("#[info] only accepts string values");
                                }
                            },
                            _ => panic!("#[info] only accepts name-value pairs")
                        }
                    }
                },
                syn::Meta::List(ref list) if list.path.segments[0].ident == "label" => {
                    let mut msg = None;
                    let mut args = None;
                    let mut severity = None;
                    let mut span = None;
                    
                    for nested in &list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                                if let syn::Lit::Str(val) = &nv.lit {
                                    let val_span = val.span();
                                    
                                    if nv.path.segments[0].ident == "msg" {
                                        msg = Some(val.clone());
                                    } else if nv.path.segments[0].ident == "args" {
                                        let val = val.value();
                                        let a = val.split(",")
                                            .map(|s| s.trim().parse::<usize>().expect("#[info(args)] only accepts integers"))
                                            .map(|n| format!("field_{}", n))
                                            .map(|n| syn::Ident::new(&n, val_span.clone()))
                                            .collect::<Vec<_>>();
                                        
                                        args = Some(a);
                                    } else if nv.path.segments[0].ident == "span" {
                                        let ident = syn::Ident::new(&format!("field_{}", val.value()), val_span);
                                        
                                        span = Some(quote!{ #ident });
                                    } else if nv.path.segments[0].ident == "sev" {
                                        let ident = syn::Ident::new(&val.value(), val_span);
                                        
                                        severity = Some(ident);
                                    } else {
                                        panic!("#[label] only accepts 'msg', 'args', 'span' and 'sev'");
                                    }
                                } else {
                                    panic!("#[label] only accepts string values");
                                }
                            },
                            _ => panic!("#[label] only accepts name-value pairs")
                        }
                    }
                    
                    let msg = if let Some(msg) = msg {
                        Some((msg, args.unwrap_or(Vec::new())))
                    } else {
                        None
                    };
                    
                    let severity = severity.expect("A severity must be provided to a label");
                    let span = span.unwrap_or(quote!{ diagnostics::span::Span::default() });
                    
                    labels.push((msg, severity, span));
                },
                syn::Meta::List(_) => (),
                _ => panic!("Expected a name-value list")
            }
        }
        
        let msg = msg.expect("A message must be provided");
        let args = args.unwrap_or(Vec::new());
        let severity = severity.expect("A severity must be provided");
        
        Opts {
            msg: (msg, args),
            severity,
            labels,
        }
    }
}