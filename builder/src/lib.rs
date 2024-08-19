use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Parser},
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Data,
    DataStruct, DeriveInput, Expr, ExprLit, Fields, FieldsNamed,
    GenericArgument, Ident, Lit, MetaNameValue, PathArguments, Result, Token,
    Type, TypePath,
};

/// Try to extract the inner type of a type:
/// example: Option<T> or Vec<T>
///
/// Return: inner
///
/// ```ignore
/// let t: Ident = syn::parse_str("std::vec::Vec<String>")?;
/// extract_inner_type(&myty, &t)
/// ```
fn extract_inner_type<'a>(
    ty: &'a syn::Type,
    outter: &'a syn::Ident,
) -> Option<syn::Type> {
    let syn::Type::Path(TypePath { qself: None, ref path }) = ty else {
        return None;
    };

    let segment = path.segments.first()?;

    if segment.ident != *outter {
        return None;
    }

    let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
        ref args,
        ..
    }) = segment.arguments
    else {
        return None;
    };
    let GenericArgument::Type(Type::Path(TypePath { qself: None, path })) =
        args.first()?
    else {
        return None;
    };

    path.segments.first()?;

    // let toreturn = Type::Path(TypePath { qself: None, path: path.clone() });
    // println!("passed {toreturn:#?}");

    Some(Type::Path(TypePath { qself: None, path: path.clone() }))
}

struct StructField {
    // only accept #[builder(arg = "x")]
    attrs: Vec<Attribute>,
    name: Ident,
    ty: Type,
}

impl Parse for StructField {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(StructField {
            attrs: input.call(Attribute::parse_outer)?,
            name: input.parse()?,
            ty: input.parse()?,
        })
    }
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
        let mut ty = field.ty.clone();

        // let vec = extract_inner_type(&ty, &syn::parse_str("Vec").unwrap());
        // let is_vec = vec.is_some();
        // let attr = &field.attrs;
        // if !attr.is_empty() {
        //     let att = &attr[0];
        //     let parsed: MetaNameValue = att.parse_args().unwrap();
        //     let key = &parsed.path.segments[0].ident;
        //     let Expr::Lit(ExprLit { lit: Lit::Str(value), .. }) =
        // &parsed.value     else {
        //         panic!("Wrong attribute syntax for builder");
        //     };
        //
        //     // remove quotes from the string
        //     let value: Ident = value.parse().unwrap();
        //
        //     if !is_vec {
        //         panic!("Wrong syntax for builder, can only use 'each' for a
        // Vec type");     };
        //
        //     if key == "each" {
        //         let topush = quote! {
        //             pub fn #value(&mut self, v: String) -> &mut Self {
        //                 self.#ident.push(v);
        //                 self
        //             }
        //         };
        //         // eprintln!("topush {}", topush);
        //         method.push(topush);
        //     }
        // }

        eprintln!("{ty:#?}");

        let opt = extract_inner_type(&ty, &syn::parse_str("Option").unwrap());
        let is_option = opt.is_some();
        eprintln!("is_option {is_option}");

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

        // let topush = if is_vec {
        //     quote! {
        //         pub fn #ident(&mut self, v: #ty) -> &mut Self {
        //             self.#ident.extend_from_slice(&v);
        //             self
        //         }
        //     }
        // } else {
        //     quote! {
        //         pub fn #ident(&mut self, v: #ty) -> &mut Self {
        //             self.#ident = Some(v);
        //             self
        //         }
        //     }
        // };

        let topush = quote! {
            pub fn #ident(&mut self, v: #ty) -> &mut Self {
                self.#ident = Some(v);
                self
            }
        };

        method.push(topush);
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
