use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(ResolveErrorLocation)]
pub fn derive_resolve_location(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ty = input.ident;

    let expanded = if let syn::Data::Enum(data) = &input.data {
        let variant = data.variants.iter().map(|v| &v.ident);
        quote! {
            impl crate::types::ResolveErrorLocation for #ty {
                fn resolve(&mut self, source: &[u8]) {
                    match self {
                        #(
                            Self::#variant { ref mut location, .. } => { location.resolve(source); }
                        ),*
                    };
                }
            }
        }
    } else {
        quote! {
            compile_error!("ResolveErrorLocation derive rule only supports enums")
        }
    };

    TokenStream::from(expanded)
}
