use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use crate::record::Record;
use crate::types::{Value, Number, BuiltinFunction};

#[derive(Clone, Debug)]
pub enum Expr {
    StrField(FieldId),
    NumField(FieldId),
    BoolField(FieldId),
    FieldAssign(FieldId, Box<Expr>),

    Var(String),
    VarAssign(String, Box<Expr>),

    Deletion(FieldId),
    Keep(Vec<FieldId>),

    StrLiteral(String),
    NumLiteral(Number),
    BoolLiteral(bool),

    FnCall{ name: String, args: Vec<Expr> },

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
    ConcatWSep(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ineq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Lesser(Box<Expr>, Box<Expr>),
    LesserEq(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),

    RegexSearch{ re: Regex, field: FieldId }
}

pub struct EvaluationContext {
    current_record: RefCell<Record>,
    ofs: String,
    field_names: RefCell<HashMap<String, usize>>,

    functions: HashMap<String, BuiltinFunction>,
    variables: RefCell<HashMap<String, Value>>
}

#[derive(Debug, Clone)]
pub struct Rule {
    pattern: Expr,
    actions: Vec<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldId {
    Name(String),
    Idx(usize)
}

impl Expr {
    pub fn eval(&self, ctx: &mut EvaluationContext) -> Value {
        use Expr::*;
        match self {
            StrField(field_id) => {
                let idx = match field_id {
                    FieldId::Name(name) => ctx.field_names.borrow()[name],
                    FieldId::Idx(idx) => *idx,
                };
                if idx == 0 {
                    Value::Str(
                        ctx.current_record
                            .borrow()
                            .join(ctx.ofs.as_str())
                    )
                } else {
                    Value::Str(
                        ctx.current_record
                            .borrow()
                            .nth_str(idx)
                            .unwrap_or("")
                            .to_owned()
                    )
                }
            },
            NumField(field_id) => {
                let idx = match field_id {
                    FieldId::Name(name) => ctx.field_names.borrow()[name],
                    FieldId::Idx(idx) => *idx,
                };
                let rec = ctx.current_record.borrow();
                Value::Num(
                    rec
                        .nth_num(idx)
                        .unwrap()
                        .to_owned()
                )
            },
            BoolField(field_id) => {
                let idx = match field_id {
                    FieldId::Name(name) => ctx.field_names.borrow()[name],
                    FieldId::Idx(idx) => *idx
                };
                let rec = ctx.current_record.borrow();
                Value::Bool(
                    rec
                        .nth_bool(idx)
                        .unwrap()
                        .to_owned()
                )
            },

            FieldAssign(id, expr) => {
                let result = expr.eval(ctx);
                let idx = match id {
                    FieldId::Idx(idx) => idx.clone(),
                    FieldId::Name(name) => ctx.field_names.borrow()[name].clone(),
                };
                ctx.current_record
                    .borrow_mut()
                    .set(idx, result.to_string());
                result
            },

            Var(name) => {
                ctx.variables
                    .borrow()
                    .get(name)
                    .unwrap_or(&Value::Str(String::from("")))
                    .clone()
            },
            VarAssign(name, expr) => {
                let result = expr.eval(ctx);
                ctx.set_var(name.clone(), result.clone());
                result
            },

            Deletion(id) => {
                if let FieldId::Name(name) = id  {
                    if ctx.field_names.borrow().contains_key(name) {
                        let idx = ctx.field_names.borrow().get(name).unwrap().clone();
                        Value::Bool(ctx.current_record.borrow_mut().try_delete_field(idx))
                    } else {
                        Value::Bool(ctx.variables.borrow_mut().remove(name).is_some())
                    }
                } else if let FieldId::Idx(idx) = id {
                    if idx > &0 {
                        Value::Bool(ctx.current_record.borrow_mut().try_delete_field(*idx))
                    } else {
                        while ctx.current_record.borrow().len() > 0 {
                            let len = ctx.current_record.borrow().len();
                            ctx.current_record.borrow_mut().try_delete_field(len);
                        }
                        Value::Bool(true)
                    }
                } else {
                    panic!("this should never be reached: deletion on a non-string, non-index field id");
                }
            },

            Keep(fields_to_keep) => {
                let mut indices_to_keep = HashSet::with_capacity(fields_to_keep.len());
                for field_id in fields_to_keep.iter() {
                    indices_to_keep.insert(
                        match field_id {
                            FieldId::Name(name) => ctx.field_names().borrow()[name],
                            FieldId::Idx(idx) => idx.clone()
                        }
                    );
                }
                let len = ctx.current_record.borrow().len();
                for i in (1..len+1).rev() {
                    if !indices_to_keep.contains(&i) {
                        ctx.current_record.borrow_mut().remove_field(i);
                    }
                }
                Value::Bool(true)
            },

            StrLiteral(s) => Value::Str(s.clone()),
            NumLiteral(n) => Value::Num(*n),
            BoolLiteral(b) => Value::Bool(*b),

            Neg(x) => Value::Num(-x.eval_num(ctx)),
            Add(x, y) => Value::Num(x.eval_num(ctx) + y.eval_num(ctx)),
            Sub(x, y) => Value::Num(x.eval_num(ctx) - y.eval_num(ctx)),
            Mul(x, y) => Value::Num(x.eval_num(ctx) * y.eval_num(ctx)),
            Div(x, y) => Value::Num(x.eval_num(ctx) / y.eval_num(ctx)),
            Mod(x, y) => Value::Num(x.eval_num(ctx) % y.eval_num(ctx)),
            Concat(x, y) => {
                let mut s1 = x.eval_str(ctx);
                let s2 = y.eval_str(ctx);
                s1.push_str(s2.as_str());

                Value::Str(s1)
            },
            ConcatWSep(x, y) => {
                let mut s1 = x.eval_str(ctx);
                let s2 = y.eval_str(ctx);
                s1.push_str(ctx.ofs.as_str());
                s1.push_str(s2.as_str());

                Value::Str(s1)
            },

            Eq(x, y) => Value::Bool(x.eval(ctx) == y.eval(ctx)),
            Ineq(x, y) => Value::Bool(x.eval(ctx) != y.eval(ctx)),
            Greater(x, y) => Value::Bool(x.eval_num(ctx) > y.eval_num(ctx)),
            GreaterEq(x, y) => Value::Bool(x.eval_num(ctx) >= y.eval_num(ctx)),
            Lesser(x, y) => Value::Bool(x.eval_num(ctx) < y.eval_num(ctx)),
            LesserEq(x, y) => Value::Bool(x.eval_num(ctx) <= y.eval_num(ctx)),

            And(x, y) => Value::Bool(x.eval_bool(ctx) && y.eval_bool(ctx)),
            Or(x, y)=> Value::Bool(x.eval_bool(ctx) || y.eval_bool(ctx)),
            Not(x) => Value::Bool(!x.eval_bool(ctx)),
            FnCall { name, args } => {
                let mut vargs = vec![];
                for a in args {
                    vargs.push(a.eval(ctx));
                }
                if !ctx.functions.contains_key(name) {
                    panic!("unknown function: {}", name);
                }
                let f = ctx.functions.get_mut(name).unwrap();
                f(vargs)
            },
            RegexSearch { re, field } => {
                let field_idx = match field {
                    FieldId::Name(name) => ctx.field_names.borrow()[name],
                    FieldId::Idx(idx) => *idx,
                };
                let r = ctx.current_record.borrow();
                let s = if field_idx == 0 {
                    // search whole record if field is 0
                    r.original_string()
                } else {
                    r.field(field_idx)
                };
                Value::Bool(
                    re.find(s).is_some()
                )
            }
        }
    }

    pub fn eval_num(&self, ctx: &mut EvaluationContext) -> Number {
        match self.eval(ctx) {
            Value::Num(x) => x,
            Value::Str(s) => panic!("Expected number, found string `{}`", s),
            Value::Bool(b) => panic!("Expected number, found bool `{}`", b)
        }
    }

    pub fn eval_bool(&self, ctx: &mut EvaluationContext) -> bool {
        match self.eval(ctx) {
            Value::Bool(b) => b,
            // Value::Str(s) => panic!("Expected bool, found string `{}`", s),
            // Value::Num(x) => panic!("Expected bool, found number `{}`", x)
            Value::Str(s) => s != "",
            Value::Num(x) => x > 0
        }
    }

    pub fn eval_str(&self, ctx: &mut EvaluationContext) -> String {
        format!("{}", self.eval(ctx))
    }
}

impl Rule {
    pub fn new(pattern: Expr, actions: Vec<Expr>) -> Self {
        Self { pattern, actions }
    }

    pub fn applies(&self, ctx: &mut EvaluationContext) -> bool {
        self.pattern.eval_bool(ctx)
    }

    pub fn execute_if_applies(&self, ctx: &mut EvaluationContext) {
        if self.applies(ctx) {
            for action in self.actions.iter() {
                action.eval(ctx);
            }
        }
    }
}

impl EvaluationContext {
    pub fn new(ofs: String) -> Self {
        Self {
            current_record: RefCell::new(Record::empty()),
            ofs,
            field_names: RefCell::new(HashMap::new()),

            functions: HashMap::new(),
            variables: RefCell::new(HashMap::new())
        }
    }

    pub fn field_names(&self) -> &RefCell<HashMap<String, usize>> {
        &self.field_names
    }

    pub fn set_field_names(&mut self, header: Vec<String>) {
        for (i, field_name) in header.into_iter().enumerate() {
            if field_name.len() > 0 {
                self.field_names.borrow_mut().insert(field_name, i+1);
            }
        }
    }

    pub fn register_builtin<F: 'static + FnMut(Vec<Value>) -> Value>(&mut self, name: &str, f: F) {
        self.functions.insert(name.to_owned(), Box::new(f));
    }

    pub fn set_var(&mut self, name: String, v: Value) {
        self.variables.borrow_mut().insert(name, v);
    }

    pub fn set_current_record(&mut self, r: Record) {
        *self.current_record.borrow_mut() = r;
    }

    pub fn current_record(&self) -> &RefCell<Record> {
        &self.current_record
    }

    pub fn ofs(&self) -> &str {
        self.ofs.as_str()
    }

    pub fn variables(&self) -> &RefCell<HashMap<String, Value>> {
        &self.variables
    }
}