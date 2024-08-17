use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed,
    Ident,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Something
    let struct_name = &input.ident;

    // SomethingBuilder
    let builder_name =
        Ident::new(&format!("{struct_name}Builder"), struct_name.span());

    // list of field names of the struct
    // [ { ident: a, type: u32 }, { ident: b, type: u32 } ]
    // struct Something {
    //     a: u32,
    //     b: u32,
    // }
    let struct_field = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        // user tried to use the macro on something that is not a struct
        unimplemented!();
    };

    // list of structs ident (name)
    let mut struct_ident = Vec::new();

    // a keypair of name and type with comma:
    // a: u32,
    let mut struct_name_type = Vec::new();

    // a: 0,
    let mut struct_name_value = Vec::new();

    for field in struct_field {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        struct_ident.push(quote! {#ident});
        struct_name_type.push(quote! {
            #ident: Option<#ty>,
        });
        struct_name_value.push(quote! {
            #ident: None,
        });
    }

    let expr = quote! {
        pub struct #builder_name {
            #(
                #struct_name_type
            )*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(
                        #struct_name_value
                    )*
                }
            }
        }
    };

    TokenStream::from(expr)
}
