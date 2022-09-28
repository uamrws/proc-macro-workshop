use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_debug_trait(&ast)
}

fn impl_debug_trait(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let ident_str = ident.to_string();
    let fields_token = if let syn::Data::Struct(syn::DataStruct {
        struct_token: _,
        fields: syn::Fields::Named(fields),
        semi_token: _,
    }) = &ast.data
    {
        fields
            .named
            .iter()
            .map(|f| -> _ {
                let field_str = f.ident.as_ref().unwrap().to_string();
                let field_ident = f.ident.as_ref().unwrap();
                quote_spanned! {
                    f.span()=>
                    .field(#field_str, &self.#field_ident)
                }
            })
            .collect()
    } else {
        Vec::new()
    };
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    quote_spanned! {
        ident.span()=>
        impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#ident_str)
                   #(#fields_token)*
                   .finish()
            }
        }
    }
    .into()
}
