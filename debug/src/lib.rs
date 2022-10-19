use proc_macro::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::{spanned::Spanned, Field, WherePredicate};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_debug_trait(&ast)
}

fn impl_debug_trait(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let ident_str = ident.to_string();
    let mut skip_impl_where_clause = false;
    let mut debug_trait_helper = DebugTraitHelper::new(&ast.generics);
    for i in ast.attrs.iter() {
        skip_impl_where_clause =
            skip_impl_where_clause || debug_trait_helper.parse_debug_bound_attribute(i);
    }
    if let syn::Data::Struct(syn::DataStruct {
        struct_token: _,
        fields: syn::Fields::Named(fields),
        semi_token: _,
    }) = &ast.data
    {
        for i in &fields.named {
            debug_trait_helper.impl_field(i);
            if !skip_impl_where_clause {
                debug_trait_helper.impl_where_clause(i);
            }
        }
    }
    let DebugTraitHelper {
        field_tokens,
        generics,
    } = debug_trait_helper;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote_spanned! {
        ident.span()=>
        impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#ident_str)
                   #(#field_tokens)*
                   .finish()
            }
        }
    }
    .into()
}

struct DebugTraitHelper {
    field_tokens: Vec<Box<dyn ToTokens>>,
    generics: syn::Generics,
}

impl DebugTraitHelper {
    fn new(generics: &syn::Generics) -> Self {
        DebugTraitHelper {
            field_tokens: Vec::new(),
            generics: (*generics).clone(),
        }
    }

    fn need_add_where_clause(
        ty: &syn::Type,
        generic_ty_param: &syn::TypeParam,
        token: &mut Box<dyn ToTokens>,
    ) -> bool {
        match *ty {
            syn::Type::Path(ref ty_path) => {
                ty_path.path.is_ident(&generic_ty_param.ident) || {
                    let first = ty_path.path.segments.first().unwrap();
                    if first.ident == generic_ty_param.ident {
                        *token = Box::new((*ty_path).clone());
                        true
                    } else {
                        let last = ty_path.path.segments.last().unwrap();
                        if last.ident == "PhantomData" {
                            false
                        } else if let syn::PathArguments::AngleBracketed(ref arg) = last.arguments {
                            arg.args.iter().fold(false, |acc, arg| {
                                if let syn::GenericArgument::Type(ref ty) = arg {
                                    acc || Self::need_add_where_clause(ty, generic_ty_param, token)
                                } else {
                                    acc
                                }
                            })
                        } else {
                            false
                        }
                    }
                }
            }
            syn::Type::Reference(ref ty_ref) => {
                Self::need_add_where_clause(ty_ref.elem.as_ref(), generic_ty_param, token)
            }
            _ => false,
        }
    }

    fn impl_where_clause(&mut self, field: &Field) {
        let g_c = self.generics.clone();
        let generic_type_params = g_c.type_params();
        let where_clause = self.generics.where_clause.get_or_insert(syn::WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        });
        for i in generic_type_params {
            let mut token: Box<dyn ToTokens> = Box::new(i.ident.clone());
            if Self::need_add_where_clause(&field.ty, i, &mut token) {
                where_clause.predicates.push(
                    syn::parse2::<WherePredicate>(
                        quote_spanned!(field.span()=> #token: std::fmt::Debug),
                    )
                    .unwrap(),
                );
            }
        }
    }

    fn impl_field(&mut self, field: &Field) {
        let field_str = field.ident.as_ref().unwrap().to_string();

        let token = self.parse_debug_fmt_attribute(field);
        self.field_tokens.push(Box::new(quote_spanned! {
            field.span()=>
            .field(#field_str, #token)
        }));
    }

    fn parse_debug_fmt_attribute(&self, field: &Field) -> impl ToTokens {
        let field_ident = field.ident.as_ref().unwrap();
        for i in &field.attrs {
            if i.path.is_ident("debug") {
                if let Ok(attr) = syn::parse2::<BuilderAttrParser>(i.tokens.clone()) {
                    let fmt = attr.lit.value();
                    return quote_spanned!(i.span()=> &format_args!(#fmt, self.#field_ident));
                } else {
                    return syn::Error::new(i.span(), r#"expected `debug = "..."`"#)
                        .into_compile_error();
                }
            }
        }
        quote_spanned!(field.span()=> &self.#field_ident)
    }

    fn parse_debug_bound_attribute(&mut self, attr: &syn::Attribute) -> bool {
        if attr.path.is_ident("debug") {
            if let Ok(attr) = attr.parse_args::<BuilderAttrParser>() {
                if let Some(ident) = attr.ident {
                    if ident == "bound" {
                        let where_clause =
                            self.generics.where_clause.get_or_insert(syn::WhereClause {
                                where_token: Default::default(),
                                predicates: Default::default(),
                            });
                        where_clause
                            .predicates
                            .push(syn::parse_str::<WherePredicate>(&attr.lit.value()).unwrap());
                        return true;
                    }
                }
            }
        }
        false
    }
}

struct BuilderAttrParser {
    ident: Option<syn::Ident>,
    _eq: syn::Token!(=),
    lit: syn::LitStr,
}

impl syn::parse::Parse for BuilderAttrParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(BuilderAttrParser {
            ident: input.parse()?,
            _eq: input.parse()?,
            lit: input.parse()?,
        })
    }
}
