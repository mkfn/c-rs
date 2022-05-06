use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, DataEnum, DataStruct, DeriveInput, Ident, Result};

#[proc_macro_derive(Parse)]
pub fn derive_parse(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match expand_parse(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_parse(input: &DeriveInput) -> Result<TokenStream2> {
    let id = &input.ident;
    match &input.data {
        syn::Data::Struct(data) => struct_parse(id, data),
        syn::Data::Enum(data) => enum_parse(id, data),
        _ => Err(syn::Error::new(
            id.span(),
            "only structs and enums are supported",
        )),
    }
}

fn struct_parse(name: &Ident, data: &DataStruct) -> Result<TokenStream2> {
    use syn::{Fields, FieldsNamed};
    if let Fields::Named(FieldsNamed { named, .. }) = &data.fields {
        let fields = named.iter().map(|f| &f.ident);
        let fields_copy = fields.clone();
        let tys = named.iter().map(|f| &f.ty);
        let inits = tys.map(|ty| {
            let (wh, t) = get_inner(&ty);
            match wh {
                InnerType::Box => {
                    quote!(Box::new(#t::parse(&mut _seq)?))
                }
                InnerType::Option => {
                    quote!(#t::parse(&mut _seq).ok())
                }
                InnerType::OptionBox => {
                    quote!(
                        #t::parse(&mut _seq).ok().map(|x| Box::new(x)))
                }
                InnerType::Vec => {
                    quote!(
                        {
                            let mut items = Vec::new();
                            while let Ok(item) = #t::parse(&mut _seq) {
                                items.push(item);
                            }
                            items
                        }
                    )
                }
                InnerType::None => {
                    quote!(#t::parse(&mut _seq)?)
                }
            }
        });
        Ok(quote!(
            impl Parse for #name {
                fn parse(seq: &mut Seq) -> Result<Self, crate::Span> {
                    let mut _seq = *seq;
                    #(let #fields = #inits;)*
                    *seq = _seq;
                    Ok(#name {
                        #(#fields_copy,)*
                    })
                }
            }
        ))
    } else {
        Err(syn::Error::new(
            name.span(),
            "unname or unit structs are not supported",
        ))
    }
}

fn enum_parse(name: &Ident, data: &DataEnum) -> Result<TokenStream2> {
    use syn::{Fields, FieldsUnnamed};
    let vars = data.variants.iter().map(|v| &v.ident);
    let mut tys = Vec::new();
    for v in data.variants.iter() {
        if let Fields::Unnamed(FieldsUnnamed { unnamed, .. }) = &v.fields {
            if unnamed.len() == 1 {
                tys.push(unnamed.first().unwrap().ty.clone());
            } else {
                return Err(syn::Error::new(
                    v.ident.span(),
                    "only support 1 type per variant",
                ));
            }
        } else {
            return Err(syn::Error::new(v.ident.span(), "only unname supported"));
        }
    }

    Ok(quote!(
        impl Parse for #name {
            fn parse(seq: &mut Seq) -> Result<Self, crate::Span> {
                let mut span = crate::Span::new();
                #({
                    let mut try_seq = *seq;
                    match #tys::parse(&mut try_seq) {
                        Ok(t) => {
                            *seq = try_seq;
                            return Ok(#name::#vars(t));
                        }
                        Err(s) => {
                            if span < s {
                                span = s;
                            }
                        }
                    }
                })*
                Err(span)
            }
        }
    ))
}

enum InnerType {
    None,
    Box,
    Option,
    Vec,
    OptionBox,
}

fn get_inner(ty: &syn::Type) -> (InnerType, syn::Type) {
    match _get_inner(ty) {
        Some((outer, t)) => match outer {
            "Box" => (InnerType::Box, t),
            "Option" => match _get_inner(&t) {
                Some(("Box", inner)) => (InnerType::OptionBox, inner),
                None => (InnerType::Option, t),
                _ => unreachable!(),
            },
            "Vec" => (InnerType::Vec, t),
            _ => unreachable!(),
        },
        None => (InnerType::None, ty.clone()),
    }
}

fn _get_inner(ty: &syn::Type) -> Option<(&str, syn::Type)> {
    use syn::{
        AngleBracketedGenericArguments, GenericArgument, Path, PathArguments, Type, TypePath,
    };
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        let seg = segments.last()?;
        let outer = match seg.ident.to_string().as_ref() {
            "Box" => "Box",
            "Option" => "Option",
            "Vec" => "Vec",
            _ => return None,
        };
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            &seg.arguments
        {
            let inner = args.first()?.clone();
            if let GenericArgument::Type(inner) = inner {
                return Some((outer, inner));
            }
        }
    }
    None
}
