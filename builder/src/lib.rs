use proc_macro::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::{spanned::Spanned, DeriveInput, Field, Ident, Token, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<DeriveInput>(input).unwrap();
    impl_builder_macro(&ast)
}

fn impl_builder_macro(ast: &DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let builder_ident = syn::parse_str::<Ident>(&format!("{ident}Builder")).unwrap();
    let mut builder_struct_helper = BuilderStructHelper::new();
    if let syn::Data::Struct(data) = &ast.data {
        if let syn::Fields::Named(syn::FieldsNamed {
            brace_token: _,
            named,
        }) = &data.fields
        {
            for field in named {
                builder_struct_helper.impl_builder_field(field);
                builder_struct_helper.impl_builder_field_set_method(field);
                builder_struct_helper.impl_builder_field_check_method(field);
                builder_struct_helper.impl_builder_build_method(field);
                builder_struct_helper.impl_builder_init(field);
            }
        } else {
            panic!("unexpected struct without named fields")
        }
    } else {
        panic!("expected struct")
    };

    let BuilderStructHelper {
        field_tokens,
        field_set_tokens,
        field_check_tokens,
        build_tokens,
        init_tokens,
    } = builder_struct_helper;
    let (impl_generics, type_generics, whereclause) = ast.generics.split_for_impl();

    let gen = quote_spanned! {
        ast.span()=>

        pub struct #builder_ident #type_generics #whereclause {
            #(#field_tokens),*
        }

        impl #impl_generics #builder_ident #type_generics #whereclause{
            #(#field_set_tokens)*


            pub fn build(&mut self)->std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>>{
                #(#field_check_tokens)*
                Ok(#ident{
                    #(#build_tokens),*
                })
            }
        }

        impl #impl_generics #ident #type_generics #whereclause{
            pub fn builder() -> #builder_ident {
                #builder_ident{
                    #(#init_tokens),*
                }
            }
        }
    };
    gen.into()
}

struct BuilderStructHelper {
    field_tokens: Vec<proc_macro2::TokenStream>,
    field_set_tokens: Vec<proc_macro2::TokenStream>,
    field_check_tokens: Vec<proc_macro2::TokenStream>,
    build_tokens: Vec<proc_macro2::TokenStream>,
    init_tokens: Vec<proc_macro2::TokenStream>,
}
impl BuilderStructHelper {
    fn new() -> Self {
        BuilderStructHelper {
            field_tokens: Vec::new(),
            field_set_tokens: Vec::new(),
            field_check_tokens: Vec::new(),
            build_tokens: Vec::new(),
            init_tokens: Vec::new(),
        }
    }

    fn impl_builder_field(&mut self, field: &Field) {
        let field_ident = self.get_field_ident(field);
        let field_type = &field.ty;
        if self.is_option(field) {
            self.field_tokens
                .push(quote_spanned!(field.span()=>#field_ident: #field_type))
        } else {
            self.field_tokens
                .push(quote_spanned!(field.span()=>#field_ident: std::option::Option<#field_type>));
        }
    }

    fn impl_builder_field_set_method(&mut self, field: &Field) {
        let field_ident = self.get_field_ident(field);
        let mut field_type = &field.ty;
        let result;
        if self.is_option(field) {
            result = syn::parse2::<SingleTypeParser>(field_type.to_token_stream()).unwrap();
            field_type = &result.ty;
        }
        self.field_set_tokens.push(quote_spanned! {
            field_ident.span()=>
            fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                self.#field_ident = std::option::Option::Some(#field_ident);
                self
            }
        });
        for i in &field.attrs {
            if i.path.is_ident("builder") {
                let token = self
                    .parse_builder_attribute(field, i)
                    .unwrap_or_else(syn::Error::into_compile_error);
                self.field_set_tokens.push(token)
            }
        }
    }

    fn impl_builder_field_check_method(&mut self, field: &Field) {
        let field_ident = self.get_field_ident(field);
        let err_message = format!("{} must not be none", field_ident);
        if !self.is_option(field) {
            self.field_check_tokens.push(quote_spanned!(
                field.span()=>
                if self.#field_ident.is_none() {
                    return Err(#err_message.into());
                }
            ));
        }
    }
    fn impl_builder_build_method(&mut self, field: &Field) {
        let field_ident = self.get_field_ident(field);
        let set_token = if self.is_option(field) {
            quote_spanned!(field.span()=> self.#field_ident.take())
        } else {
            quote_spanned!(field.span()=> self.#field_ident.take().unwrap())
        };
        self.build_tokens
            .push(quote_spanned!(field.span()=> #field_ident: #set_token))
    }

    fn impl_builder_init(&mut self, field: &Field) {
        let field_ident = self.get_field_ident(field);
        self.init_tokens.push(quote_spanned! {
            field.span()=>
            #field_ident: std::option::Option::None
        })
    }

    fn get_field_ident(&self, field: &Field) -> Ident {
        field.ident.clone().unwrap()
    }

    fn is_option(&self, field: &Field) -> bool {
        if let Type::Path(syn::TypePath { qself: _, path }) = &field.ty {
            path.segments[0].ident == "Option"
        } else {
            false
        }
    }

    fn parse_builder_attribute(
        &self,
        field: &Field,
        attr: &syn::Attribute,
    ) -> syn::Result<proc_macro2::TokenStream> {
        let name_value = attr.parse_args::<syn::MetaNameValue>()?;
        if name_value.path.is_ident("each") {
            if let syn::Lit::Str(s) = name_value.lit {
                let ident = syn::parse_str::<Ident>(&s.value())?;
                let field_ident = self.get_field_ident(field);
                let mut field_type = &field.ty;
                let parse = syn::parse2::<SingleTypeParser>(field_type.to_token_stream())?;
                if parse.ident != "Vec" {
                    Err(syn::Error::new(
                        field.span(),
                        "builder attribute expected on a type Vec",
                    ))
                } else {
                    field_type = &parse.ty;
                    if field_ident != ident {
                        Ok(quote_spanned! {
                            attr.span()=>
                            fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                                let value = self.#field_ident.get_or_insert(Vec::new());
                                value.push(#ident);
                                self
                            }
                        })
                    } else {
                        Ok(quote::quote!())
                    }
                }
            } else {
                Err(syn::Error::new(
                    attr.span(),
                    "invalid builder attribute, not a str literal",
                ))
            }
        } else {
            let e = syn::Error::new_spanned(
                attr.parse_meta().unwrap(),
                r#"expected `builder(each = "...")`"#,
            );
            println!("{:#?}", e.span());
            Err(syn::Error::new_spanned(
                attr.parse_meta().unwrap(),
                r#"expected `builder(each = "...")`"#,
            ))
        }
    }
}

// parse type like Option<T> or Vec<T>
struct SingleTypeParser {
    ident: Ident,
    _lt_token: Token![<],
    ty: syn::Type,
    _gt_token: Token![>],
}

impl syn::parse::Parse for SingleTypeParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(SingleTypeParser {
            ident: input.parse()?,
            _lt_token: input.parse()?,
            ty: input.parse()?,
            _gt_token: input.parse()?,
        })
    }
}
