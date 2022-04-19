use std::fmt::Display;

use fixed::FixedI128;
use fixed::types::extra::U32;
use crate::expr::EvaluationContext;

pub type Number = FixedI128<U32>;

pub type BuiltinFunction = Box<dyn FnMut(Vec<Value>) -> Value>;

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub enum Value {
    Num(Number),
    Str(String),
    Bool(bool)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Num(n) => write!(f, "{}", n),
            Str(s) => write!(f, "{}", s),
            Bool(b) => write!(f, "{}", b),
        }
    }
}