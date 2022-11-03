use proc_macro::TokenStream;
use syn::{parse_macro_input, visit_mut::VisitMut, Ident, ItemStruct, Type};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;

    let mut item_struct = parse_macro_input!(input as ItemStruct);
    let mut bitfield = Bitfield::new();
    bitfield.visit_item_struct_mut(&mut item_struct);

    bitfield.gen_token(item_struct)
}

struct Bitfield {
    impl_fn_token: Vec<proc_macro2::TokenStream>,
    impl_check_mod8: proc_macro2::TokenStream,
}
impl Bitfield {
    fn new() -> Self {
        Bitfield {
            impl_fn_token: Vec::new(),
            impl_check_mod8: Default::default(),
        }
    }

    fn gen_token(&self, item_struct: ItemStruct) -> TokenStream {
        let ident = &item_struct.ident;
        let (impl_generics, ty_generics, where_clause) = item_struct.generics.split_for_impl();
        let Bitfield {
            impl_fn_token,
            impl_check_mod8,
        } = &self;
        quote::quote_spanned! {
            ident.span()=>
            #item_struct
            #impl_check_mod8
            impl #impl_generics #ident #ty_generics #where_clause{
                #(#impl_fn_token)*
            }
        }
        .into()
    }

    fn gen_set_fn_token(
        &self,
        ident: &Ident,
        ty: &Type,
        pre_size: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        let fn_ident: Ident = syn::parse_str(&format!("set_{}", ident)).unwrap();
        quote::quote_spanned! {
            ident.span()=>
            fn #fn_ident (&mut self, num: <#ty as Specifier>::USIZE){
                unsafe {
                    let data = self.get_ptr();
                    let v = *data;
                    let pre = v % (1 << (#pre_size));
                    let suffix = v / (1 << (#pre_size + #ty::BITS));
                    let n = (num as usize) << (#pre_size);
                    let r = pre+n+suffix;
                    self.set_data(&r);
                }
            }
        }
    }
    
    fn gen_get_fn_token(
        &self,
        ident: &Ident,
        ty: &Type,
        pre_size: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        let fn_ident: Ident = syn::parse_str(&format!("get_{}", ident)).unwrap();
        quote::quote_spanned! {
            ident.span()=>
            fn #fn_ident (&self) -> <#ty as Specifier>::USIZE{
                unsafe{
                    let data = self.get_ptr();
                    let mut v = *data;
                    v %= 1 << (#ty::BITS + #pre_size);
                    v >>= #pre_size;
                    v as <#ty as Specifier>::USIZE
                }
            }
        }
    }
}

impl VisitMut for Bitfield {
    fn visit_fields_named_mut(&mut self, fields: &mut syn::FieldsNamed) {
        let mut bits = Vec::new();
        for i in &fields.named {
            let ty = &i.ty;
            let pre_size = {
                if bits.is_empty() {
                    quote::quote!(0)
                } else {
                    quote::quote!(#(#bits)+*)
                }
            };
            self.impl_fn_token.push(self.gen_get_fn_token(
                i.ident.as_ref().unwrap(),
                ty,
                &pre_size,
            ));
            self.impl_fn_token.push(self.gen_set_fn_token(
                i.ident.as_ref().unwrap(),
                ty,
                &pre_size,
            ));
            bits.push(quote::quote!(#ty::BITS));
        }
        let bits_len = quote::quote!((#(#bits)+*));
        let size = quote::quote!(#bits_len/8);
        let mod_size = quote::quote!(#bits_len%8);
        *fields = syn::parse2(quote::quote! {
            {data: [u8; #size]}
        })
        .unwrap();
        self.impl_check_mod8 = quote::quote! {
            trait CanMod8 {}
            trait CheckMod8:CanMod8 {}
            impl CanMod8 for [u8; 0]{}
            impl CheckMod8 for [u8; #mod_size] {}
        };
        self.impl_fn_token.push(
            syn::parse2(quote::quote! {
                fn new()->Self{
                    Self {
                        data: [0; #size]
                    }
                }
                fn get_ptr(&self) -> *const usize{
                    &self.data as *const [u8] as *const usize
                }
                fn set_data(&mut self, data: &usize){
                    unsafe {
                        self.data = *(data as *const usize as *const [u8; #size])
                    }
                }
            })
            .unwrap(),
        )
    }
}
