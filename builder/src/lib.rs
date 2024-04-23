use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{DeriveInput, Field};

struct BuilderInput {
  name: syn::Ident,
  generics: syn::Generics,
  fields: Vec<Field>,
}

impl TryFrom<DeriveInput> for BuilderInput {
  type Error = syn::Error;

  fn try_from(value: DeriveInput) -> Result<Self, Self::Error> {
    let DeriveInput {
      ident,
      generics,
      data,
      ..
    } = value;

    let struct_data = match data {
      syn::Data::Struct(data) => data,
      syn::Data::Enum(data) => {
        return Err(syn::parse::Error::new_spanned(
          data.enum_token,
          "Enums are not supported",
        ));
      }
      syn::Data::Union(data) => {
        return Err(syn::parse::Error::new_spanned(
          data.union_token,
          "Unions are not supported",
        ));
      }
    };

    let fields = match struct_data.fields {
      syn::Fields::Named(fields) => fields.named.into_iter().collect(),
      _ => {
        return Err(syn::parse::Error::new_spanned(
          struct_data.fields,
          "Only named fields are supported",
        ))
      }
    };

    Ok(Self {
      name: ident,
      fields,
      generics,
    })
  }
}

impl BuilderInput {
  fn constructor(&self) -> TokenStream {
    let builder_name = self.type_name();
    let generics = &self.generics;
    let name = &self.name;
    let field_names = &self.field_names();

    quote! {
      impl #generics #name #generics {
        pub fn builder() -> #builder_name #generics {
          #builder_name {
            #( #field_names: None ),*
          }
        }
      }
    }
  }

  fn field_names(&self) -> Vec<syn::Ident> {
    self
      .fields
      .iter()
      // unwrap is safe because we only deal with named fields
      .map(|field| field.ident.clone().unwrap())
      .collect()
  }

  fn type_data(&self) -> TokenStream {
    let name = self.type_name();
    let generics = &self.generics;
    let fields = self.fields();

    quote! {
      struct #name #generics { #fields }
    }
  }

  fn type_name(&self) -> syn::Ident {
    let name = &self.name;
    format_ident!("{name}Builder")
  }

  fn fields(&self) -> TokenStream {
    let fields = self.fields.iter().map(|field| {
      let name = &field.ident;
      let ty = &field.ty;
      quote! {
        #name: Option<#ty>
      }
    });

    quote! {
      #( #fields ),*
    }
  }

  fn builder_impl(&self) -> TokenStream {
    let methods = self.fields.iter().map(Self::builder_method);
    let name = self.type_name();
    let generics = &self.generics;
    let impl_header = quote! {
      impl #generics #name #generics
    };
    let build_method = self.build_method();

    quote! {
      #impl_header {
        #( #methods )*
        #build_method
      }
    }
  }

  fn build_method(&self) -> TokenStream {
    let name = &self.name;
    let check_not_none = |field: &Field| {
      let ident = field.ident.as_ref().unwrap();
      let ident_str = ident.to_string();
      let err_msg = format!("missing {} field", ident_str);
      quote! {
        let Some(#ident) = self.#ident else {
          return Err(std::boxed::Box::<dyn std::error::Error>::from(#err_msg));
        };
      }
    };
    let check_fields = self.fields.iter().map(check_not_none);
    let init_fields = self.fields.iter().map(|field| {
      let ident = field.ident.as_ref().unwrap();
      quote! {
        #ident,
      }
    });

    quote! {
      pub fn build(self) -> Result<#name, Box<dyn std::error::Error>> {
        #( #check_fields )*
        Ok(#name {#( #init_fields )*})
      }
    }
  }

  fn builder_method(field: &Field) -> TokenStream {
    let name = field.ident.as_ref().unwrap();
    let ty = &field.ty;

    quote! {
      pub fn #name(&mut self, #name: #ty) -> &mut Self {
        self.#name = Some(#name);
        self
      }
    }
  }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let derive_input = syn::parse_macro_input!(input as DeriveInput);
  let builder_input = match BuilderInput::try_from(derive_input) {
    Ok(builder_input) => builder_input,
    Err(err) => return err.to_compile_error().into(),
  };

  let constructor = builder_input.constructor();
  let builder_type_data = builder_input.type_data();
  let builder_impl = builder_input.builder_impl();

  let output = quote! {
    #constructor
    #builder_type_data
    #builder_impl
  };

  output.into()
}
