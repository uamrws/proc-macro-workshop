use std::ops::Range;

use proc_macro::TokenStream;

use quote::quote;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _ = input;

    let seq = syn::parse_macro_input!(input as Seq);
    seq.expand().into()
}

struct Seq {
    ident: syn::Ident,
    _in: syn::Token![in],
    start: syn::LitInt,
    _dot2: syn::Token![..],
    eq: Option<syn::Token![=]>,
    end: syn::LitInt,
    _brace: syn::token::Brace,
    content: proc_macro2::TokenStream,
}
impl Seq {
    fn replace(
        &self,
        token_stream: &proc_macro2::TokenStream,
        val: isize,
        repeat_flag: &mut bool,
    ) -> proc_macro2::TokenStream {
        if let Ok(repeat) = syn::parse2::<Repeat>((*token_stream).clone()) {
            *repeat_flag = true;
            let mut tokens = Vec::new();
            for i in self.get_range() {
                tokens.push(self.replace(&repeat.contet, i, repeat_flag));
            }
            quote!(#(#tokens)*)
        } else {
            let token_trees: Vec<proc_macro2::TokenTree> =
                (*token_stream).clone().into_iter().collect();
            let mut need_merge = false;
            let mut new_token_trees = Vec::with_capacity(token_trees.len());
            for tree in token_trees {
                match tree {
                    proc_macro2::TokenTree::Group(ref token) => {
                        let mut group = proc_macro2::TokenTree::Group(proc_macro2::Group::new(
                            token.delimiter(),
                            self.replace(&token.stream(), val, repeat_flag),
                        ));
                        group.set_span(token.span());
                        new_token_trees.push(group);
                    }
                    proc_macro2::TokenTree::Ident(ref ident) => {
                        if need_merge {
                            let pre_ident = {
                                match new_token_trees.pop().expect("expected prefix~N~sufix") {
                                    proc_macro2::TokenTree::Ident(ident) => ident,
                                    _ => panic!("expected prefix~N~sufix"),
                                }
                            };
                            let new_str = pre_ident.to_string() + &val.to_string();
                            new_token_trees.push(proc_macro2::TokenTree::Ident(
                                proc_macro2::Ident::new(&new_str, pre_ident.span()),
                            ));
                            need_merge = false;
                        } else if *ident == self.ident {
                            let mut lit = proc_macro2::Literal::isize_unsuffixed(val);
                            lit.set_span(ident.span());
                            new_token_trees.push(proc_macro2::TokenTree::Literal(lit));
                        } else {
                            new_token_trees.push(tree);
                        }
                    }
                    proc_macro2::TokenTree::Punct(ref punct) => {
                        if punct.as_char() == '~' {
                            need_merge = true;
                        } else {
                            new_token_trees.push(tree);
                        }
                    }
                    proc_macro2::TokenTree::Literal(_) => new_token_trees.push(tree),
                }
            }
            new_token_trees.into_iter().collect()
        }
    }
    fn get_range(&self) -> Range<isize> {
        let start = self.start.base10_parse::<isize>().unwrap();
        let end = self.end.base10_parse::<isize>().unwrap();
        if let Some(_) = self.eq {
            start..(end + 1)
        } else {
            start..end
        }
    }

    fn expand(&self) -> proc_macro2::TokenStream {
        let mut tokens = Vec::new();
        let mut repeat_flag = false;
        for i in self.get_range() {
            if repeat_flag {
                break;
            }
            tokens.push(self.replace(&self.content, i, &mut repeat_flag));
        }
        quote!(#(#tokens)*)
    }
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Seq {
            ident: input.parse()?,
            _in: input.parse()?,
            start: input.parse()?,
            _dot2: input.parse()?,
            eq: input.parse()?,
            end: input.parse()?,
            _brace: syn::braced!(content in input),
            content: content.parse()?,
        })
    }
}

struct Repeat {
    _pound: syn::Token![#],
    _paren: syn::token::Paren,
    contet: proc_macro2::TokenStream,
    _star: syn::Token![*],
}

impl syn::parse::Parse for Repeat {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Repeat {
            _pound: input.parse()?,
            _paren: syn::parenthesized!(content in input),
            contet: content.parse()?,
            _star: input.parse()?,
        })
    }
}
