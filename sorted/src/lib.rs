use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    visit_mut::VisitMut,
    ItemEnum, ItemFn, Pat,
};
macro_rules! check_sorted {
    ($ident:ident) => {
        let mut pre = None;
        for v in &$ident.item.variants {
            match pre {
                Some(ref p) => {
                    if v.ident < *p {
                        let error = syn::Error::new(
                            v.ident.span(),
                            format!("{} should sort before {}", v.ident, p),
                        )
                        .to_compile_error();
                        let item = &$ident.item;
                        return quote!(#item #error).into();
                    }
                }
                None => {
                    pre = Some(v.ident.clone());
                }
            }
        }
    };
}
#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let sorted = parse_macro_input!(input as Sorted);
    check_sorted!(sorted);
    sorted.item.to_token_stream().into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut item_fn = parse_macro_input!(input as ItemFn);
    SortedCheck.visit_item_fn_mut(&mut item_fn);
    item_fn.to_token_stream().into()
}

struct Sorted {
    item: ItemEnum,
}

impl Sorted {
    fn new(item: ItemEnum) -> Self {
        Self { item }
    }
}
impl Parse for Sorted {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let item = ItemEnum::parse(input);
        if let Ok(i) = item {
            Ok(Self::new(i))
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "expected enum or match expression",
            ))
        }
    }
}

struct SortedCheck;

impl VisitMut for SortedCheck {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        for i in 0..node.attrs.len() {
            if node.attrs[i].path.is_ident("sorted") {
                node.attrs.remove(i);
                let mut pre = None;
                for arm in &mut node.arms {
                    let pat = &mut arm.pat;
                    let path_token = match pat {
                        Pat::Ident(pat) => pat.to_token_stream(),
                        Pat::Path(pat) => pat.path.to_token_stream(),
                        Pat::Struct(pat) => pat.path.to_token_stream(),
                        Pat::TupleStruct(pat) => pat.path.to_token_stream(),
                        Pat::Wild(pat) => pat.to_token_stream(),
                        other => {
                            *other = Pat::Verbatim(
                                syn::Error::new_spanned(&other, "unsupported by #[sorted]")
                                    .to_compile_error(),
                            );
                            arm.body = Box::new(syn::parse2(quote!(panic!())).unwrap());
                            break;
                        }
                    };
                    let path_str = path_token
                        .to_string()
                        .split_whitespace()
                        .collect::<Vec<&str>>()
                        .join("");
                    match pre {
                        Some(ref p) => {
                            if path_str == "_" {
                                pre = Some(path_str);
                            } else if path_str < *p || p == "_" {
                                *pat = Pat::Verbatim(
                                    syn::Error::new_spanned(
                                        path_token,
                                        format!("{} should sort before {}", path_str, p),
                                    )
                                    .to_compile_error(),
                                );
                                arm.body = Box::new(syn::parse2(quote!(panic!())).unwrap());
                                break;
                            }
                        }
                        None => {
                            pre = Some(path_str);
                        }
                    }
                }
            }
        }
    }
}
