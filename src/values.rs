use std::fmt::{Debug, Formatter};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use reqwest::Url;
use crate::types::Type;

pub trait Value : Debug {
    fn get_type(&self) -> Type;
    fn to_string(&self) -> String;
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueString {
    pub val : String,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueIntegral {
    pub val : BigInt,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueFractional {
    pub val : BigDecimal,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueBoolean {
    pub val : bool,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueTime {
    pub val : u64,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ValueUrl {
    pub val : Url
}

impl Value for ValueString {
    fn get_type(&self) -> Type {
        Type::String
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}
impl Value for ValueIntegral {
    fn get_type(&self) -> Type {
        Type::Integral
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}
impl Value for ValueFractional {
    fn get_type(&self) -> Type {
        Type::Fractional
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}
impl Value for ValueBoolean {
    fn get_type(&self) -> Type {
        Type::Boolean
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}
impl Value for ValueTime {
    fn get_type(&self) -> Type {
        Type::Time
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}
impl Value for ValueUrl {
    fn get_type(&self) -> Type {
        Type::URI
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }
}