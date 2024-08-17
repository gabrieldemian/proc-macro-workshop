use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Fields, FieldsNamed, GenericArgument, Ident, Path,
    PathArguments, Type, TypePath,
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

    // required list of structs ident (name)
    let mut struct_ident = Vec::new();

    // optional list of structs ident (name)
    let mut struct_ident_opt = Vec::new();

    // a keypair of name and type with comma:
    // a: u32,
    let mut struct_name_type = Vec::new();

    // a: 0,
    let mut struct_name_value = Vec::new();

    // methods of the builder
    // pub fn #ident(&mut self, v: #ty) -> &mut Self
    let mut method = Vec::new();

    for field in struct_field {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let (is_option, inner_ty_option) = if let Type::Path(TypePath {
            path: Path { ref segments, .. },
            ..
        }) = ty
        {
            let segment = segments.first().unwrap();
            let outer = &segment.ident;
            let is_opt = *outer.to_string() == *"Option";

            if let PathArguments::AngleBracketed(
                AngleBracketedGenericArguments { ref args, .. },
            ) = segment.arguments
            {
                if let GenericArgument::Type(inner) = args.first().unwrap() {
                    eprintln!("inner {:?}", inner);
                    (is_opt, inner)
                } else {
                    unimplemented!()
                }
            } else {
                (false, ty)
            }
        } else {
            (false, ty)
        };

        if is_option {
            struct_ident_opt.push(ident);
            struct_name_type.push(quote! {
                #ident: #ty,
            });
        } else {
            struct_ident.push(ident);
            struct_name_type.push(quote! {
                #ident: Option<#ty>,
            });
        }

        struct_name_value.push(quote! {
            #ident: None,
        });

        method.push(if is_option {
            quote! {
                pub fn #ident(&mut self, v: #inner_ty_option) -> &mut Self {
                    self.#ident = Some(v);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, v: #ty) -> &mut Self {
                    self.#ident = Some(v);
                    self
                }
            }
        });
    }

    let expr = quote! {
        pub struct #builder_name {
            #(
                #struct_name_type
            )*
        }

        impl #builder_name {
            #(#method)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(
                        #struct_ident: self.#struct_ident
                            .clone()
                            .ok_or("field not set")?,
                    )*
                    #(
                        #struct_ident_opt: self.#struct_ident_opt
                            .clone()
                    )*
                })
            }
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
