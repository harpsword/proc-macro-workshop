use proc_macro::{TokenStream};
use syn::{self, punctuated::Punctuated, spanned::Spanned};
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    // eprintln!("{:#?}", &st);
    match do_expand(&st) {
        Ok(token_stream) => {
            
            token_stream.into()
        },
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());
    let struct_ident = &st.ident;


    let fields = get_fields_from_derive_input(st)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    let buidler_struct_factor_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    let buidler_struct_setter = generate_builder_struct_setter(fields)?;
    // let builder_build_func = generate_build_function(fields, struct_ident)?;
    let builder_build_func = generate_build_func(fields, struct_ident)?;

    let ret =  quote! {     // ----------------------------------+
        pub struct #builder_name_ident {                   //   |
            #builder_struct_fields_def                     //   |
        }                                                  //   |
        impl #struct_ident {                               //   |
            pub fn builder() -> #builder_name_ident {      //  被quote!宏包裹的是模板代码
                #builder_name_ident{                       //   |
                  #(#buidler_struct_factor_init_clauses),* 
                }                                          //   |
            }                                              //   |

        }                                                  //   |

        impl #builder_name_ident {
            #builder_build_func

            #(#buidler_struct_setter)*
        }
    };                     // ----------------------------------+    
    // print_types_for_debug(fields);

    Ok(ret)
}

fn print_types_for_debug(fields: &StructFields) {
    for f in fields.iter() {
        eprintln!("{:#?}", f);
    }
}

fn get_inner_type(ty: &syn::Type, ident: String) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path{ ref segments, ..}, .. }) = ty {
        if let Some(seg) = segments.last() {
            if seg.ident == ident {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{
                    ref args,
                    ..
                })   = seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None 
}

fn get_optional_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    get_inner_type(ty, "Option".to_string())
}

fn get_vec_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    get_inner_type(ty, "Vec".to_string())
}

fn generate_build_func(fields: &StructFields, struct_ident: &syn::Ident) -> syn::Result<proc_macro2::TokenStream> {
    let check_code: Vec<_> = fields.iter().map(|f| {
        if let Some(_) = get_optional_inner_type(&f.ty) {
            return quote! {

            }
        }
        if let Some(_) = get_vec_inner_type(&f.ty) {
            return quote!{

            }
        }
        let ident = &f.ident;
        quote! {
            if self.#ident.is_none() {
                let err = format!("{} is missing", stringify!(#ident));
                return std::result::Result::Err(err.into())
            }
        }
    }).collect(); 

    let define_code: Vec<_> = fields.iter().map(|f|{
        let ident = &f.ident;
        match get_optional_inner_type(&f.ty) {
            Some(_) => {
                quote! {
                    #ident: self.#ident.clone()
                }
            }
            None => {
                match get_vec_inner_type(&f.ty) {
                    Some(_) => {
                        quote! {
                            #ident: self.#ident.clone()
                        }
                    }
                    None => {

                        quote! {
                            #ident: self.#ident.clone().unwrap()
                        }
                    }
                }
            }
        }
    }).collect();

    let func_define = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            // check first
            #(#check_code)*
           std::result::Result::Ok(
            #struct_ident {
                #(#define_code),*
            }
           )
        }
    };
    Ok(func_define)
}

fn generate_builder_struct_setter(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let func_clauses: Vec<_> = fields.iter().map(|f| {
        let ident = &f.ident;
        let mut type_ = &f.ty;
        if let Some(inner_type) = get_vec_inner_type(&f.ty) {
            let literal = &f.ident.clone().unwrap().to_string();
            let literal_sub = &literal[..literal.len()-1].to_string();
            let ident_sub = syn::Ident::new(literal_sub, ident.span());

            // let tmp = literal_byte.in[:literal_byte.len()]
            return quote! {
                // fn #ident_sub(&mut self, #ident_sub: #type_) -> &mut Self {
                //     self.#ident.push(#ident_sub);
                //     self
                // }

                fn #ident(&mut self, #ident: #type_) -> &mut Self {
                    self.#ident = #ident;
                    // self.#ident.push(#ident);
                    return self
                }
            }
        }
        type_ = get_optional_inner_type(&f.ty).or(Some(&f.ty)).unwrap();
        quote! {
            fn #ident(&mut self, #ident: #type_) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    }).collect();
    Ok(func_clauses)
}

fn generate_builder_struct_factory_init_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let init_clauses: Vec<_> = fields.iter().map(|f| {
        let ident = &f.ident;
        if let Some(inner_type) = get_vec_inner_type(&f.ty) {
            return quote! {
                #ident: std::vec::Vec::<#inner_type>::new()
            };
        }
        quote!{
            #ident: std::option::Option::None
        }
    }).collect();

    Ok(init_clauses)
}

fn generate_builder_struct_fields_def(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let struct_fields: Vec<_> = fields.iter().map(|f| {
        // 处理Option，拿到原始类型
        let ident = &f.ident;
        let type_ = get_optional_inner_type(&f.ty).or(Some(&f.ty)).unwrap();
        if let Some(inner_type) = get_vec_inner_type(type_) {
            // 是vector
            return quote! {
                #ident: std::vec::Vec::<#inner_type>
            }
        }
        return quote! {
            #ident: std::option::Option<#type_>
        }
    }).collect();

    Ok(quote! {
        #(#struct_fields),*
    })
}

type StructFields = Punctuated<syn::Field, syn::Token![,]>;

fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
   if  let syn::Data::Struct(syn::DataStruct{
       fields: syn::Fields::Named(syn::FieldsNamed{ ref named, ..}),
       ..
   }) = d.data {
        return Ok(named);
   }
   Err(syn::Error::new_spanned(d, "Must define on a Struct, not Enum".to_string()))
}