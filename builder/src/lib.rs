use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Parser},
    parse_macro_input,
    spanned::Spanned,
    AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput,
    Expr, ExprLit, Fields, FieldsNamed, GenericArgument, Ident, Lit,
    MetaNameValue, PathArguments, Result, Token, Type, TypePath,
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

        let opt_inner_ty =
            extract_inner_type(&ty, &syn::parse_str("Option").unwrap());
        let is_option = opt_inner_ty.is_some();

        let vec_inner_ty =
            extract_inner_type(&ty, &syn::parse_str("Vec").unwrap());
        let is_vec = vec_inner_ty.is_some();

        if let Some(opt_inner) = opt_inner_ty {
            ty = opt_inner;
        }

        if is_vec {
            struct_name_type.push(quote! {
                #ident: #ty,
            });
            struct_name_value.push(quote! {
                #ident: Vec::new(),
            });
        } else {
            struct_name_type.push(quote! {
                #ident: Option<#ty>,
            });
            struct_name_value.push(quote! {
                #ident: None,
            });
        }

        if is_option || is_vec {
            struct_ident_opt.push(ident);
        } else {
            struct_ident.push(ident);
        }

        let attr = &field.attrs;

        if !attr.is_empty() {
            let att = &attr[0];
            let span = att.meta.span();

            let parsed: MetaNameValue = att.parse_args().unwrap();

            let key = &parsed.path.segments[0].ident;

            let Expr::Lit(ExprLit { lit: Lit::Str(value), .. }) = &parsed.value
            else {
                panic!("Wrong attribute syntax for builder");
            };

            // remove quotes from the string
            // the name of the new method that pushes types to the vec
            let value: Ident = value.parse().unwrap();

            let Some(vec_inner_ty) = vec_inner_ty else {
                panic!(
                    "Wrong syntax for builder, can only use 'each' for a
        Vec type"
                );
            };

            if key == "each" {
                let topush = quote! {
                    pub fn #value(&mut self, v: #vec_inner_ty) -> &mut Self {
                        self.#ident.push(v);
                        self
                    }
                };
                method.push(topush);
            } else {
                return syn::Error::new(
                    span,
                    "expected `builder(each = \"...\")`",
                )
                .to_compile_error()
                .into();
            }

            // if the new method name is the same as the attribute, skip adding
            // it again, continue to the next iteration
            if value == *ident {
                continue;
            }
        }

        let topush = if is_vec {
            quote! {
                pub fn #ident(&mut self, v: #ty) -> &mut Self {
                    self.#ident.extend_from_slice(&v);
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
                            .ok_or(concat!(stringify!(#struct_ident), " not set"))?,
                    )*
                    #(
                        #struct_ident_opt: self.#struct_ident_opt
                            .clone(),
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
