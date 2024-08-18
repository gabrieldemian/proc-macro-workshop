use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Fields, FieldsNamed, GenericArgument, Ident, Path,
    PathArguments, Type, TypePath,
};

fn extract_type_from_option(ty: &syn::Type) -> Option<&syn::Type> {
    use syn::{GenericArgument, Path, PathArguments, PathSegment};

    fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            syn::Type::Path(ref typepath) if typepath.qself.is_none() => {
                Some(&typepath.path)
            }
            _ => None,
        }
    }

    // TODO store (with lazy static) the vec of string
    // TODO maybe optimization, reverse the order of segments
    fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
        let idents_of_path =
            path.segments.iter().fold(String::new(), |mut acc, v| {
                acc.push_str(&v.ident.to_string());
                acc.push('|');
                acc
            });
        vec!["Option|", "std|option|Option|", "core|option|Option|"]
            .into_iter()
            .find(|s| idents_of_path == *s)
            .and_then(|_| path.segments.last())
    }

    extract_type_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => {
                    params.args.first()
                }
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

#[proc_macro_derive(Builder, attributes(builder))]
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
        let mut ty = &field.ty;

        let opt = extract_type_from_option(ty);
        let is_option = opt.is_some();

        if let Some(opt) = opt {
            ty = opt;
        }

        struct_name_type.push(quote! {
            #ident: Option<#ty>,
        });

        if is_option {
            struct_ident_opt.push(ident);
        } else {
            struct_ident.push(ident);
        }

        struct_name_value.push(quote! {
            #ident: None,
        });

        method.push(quote! {
            pub fn #ident(&mut self, v: #ty) -> &mut Self {
                self.#ident = Some(v);
                self
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
                            .ok_or(concat!(stringify!(#struct_name), " not set"))?,
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
