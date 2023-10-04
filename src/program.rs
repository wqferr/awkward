use std::cell::RefCell;

use crate::expr::{Rule, EvaluationContext};
use crate::record::Record;
use crate::types::{Number, Value};

use crate::grammar::program_parser;
use chumsky::Parser;

pub struct Program {
    rules: Vec<Rule>,
    state: EvaluationContext,
    record_terminator: String,

    output: RefCell<String>,
    record_number: RefCell<usize>,
}

impl Program {
    pub fn new(ofs: String, ors: String) -> Self {
        let mut s = Self {
            rules: vec![],
            state: EvaluationContext::new(ofs),
            record_terminator: ors,
            output: RefCell::new(String::new()),

            record_number: RefCell::new(0usize)
        };
        s.inject_bulitins();
        s
    }

    pub fn compile(source: &str) -> Vec<Rule> {
        program_parser().parse(source).unwrap()
    }

    pub fn set_field_names(&mut self, field_names: Vec<String>) {
        self.state.set_field_names(field_names);
    }

    pub fn register_builtin<F: 'static + FnMut(Vec<Value>) -> Value>(&mut self, name: &str, f: F) {
        self.state.register_builtin(name, f);
    }
    
    pub fn output_field_separator(&self) -> &str {
        self.state.ofs()
    }

    pub fn output_record_terminator(&self) -> &str {
        self.record_terminator.as_str()
    }

    pub fn push_rules(&mut self, rules: Vec<Rule>) {
        self.rules.extend(rules.into_iter());
    }

    fn inject_bulitins(&mut self) {
        let record_number = self.record_number.clone();
        self.register_builtin("nr", move |_| {
            let num = Number::from_num(*record_number.borrow());
            Value::Num(num)
        });

        let current_record = self.state.current_record().clone();
        self.register_builtin("nc", move |_| {
            let num = Number::from_num(current_record.borrow().len());
            Value::Num(num)
        });

        let separator = self.output_field_separator().to_owned();
        self.register_builtin("ofs", move |_| Value::Str(separator.clone()));

        let terminator = self.output_record_terminator().to_owned();
        self.register_builtin("ors", move |_| Value::Str(terminator.clone()));

        let current_record = self.state.current_record().clone();
        self.register_builtin("push", move |fields| {
            for field in fields.iter() {
                current_record.borrow_mut().push_field(field.to_string());
            }
            Value::Bool(true)
        });

        let variables = self.state.variables().clone();
        self.register_builtin("isvar", move |args| {
            let id = &args[0];
            if let Value::Str(name) = id {
                Value::Bool(variables.borrow().contains_key(name))
            } else {
                Value::Bool(false)
            }
        });

        // TODO fix field names with inserted / deleted fields
        // TODO insert function
        // TODO new record function

        let field_names = self.state.field_names().clone();
        let current_record = self.state.current_record().clone();
        self.register_builtin("isfield", move |args| {
            let id = &args[0];
            if let Value::Str(name) = id {
                Value::Bool(field_names.borrow().contains_key(name))
            } else if let Value::Num(idx) = id {
                Value::Bool(idx.frac() == 0 && idx <= &current_record.borrow().len())
            } else {
                Value::Bool(false)
            }
        });
    }

    pub fn consume(&mut self, record: Record) {
        self.output.borrow_mut().clear();

        self.state.set_current_record(record);
        self.state.reset_field_names();
        *self.record_number.borrow_mut() += 1;
        for d in self.rules.iter() {
            d.execute_if_applies(&mut self.state);
        }
        let record_str = self.state
            .current_record()
            .borrow()
            .join(self.output_field_separator());
        let mut output_borrow = self.output.borrow_mut();
        output_borrow.push_str(&record_str);
        output_borrow.push_str(self.record_terminator.as_str());
    }

    pub fn last_output(&self) -> String {
        self.output.borrow().to_owned()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple() {
        let rules = program_parser().parse(r#"@3 = n@1 + n@2, put(nr(), @0)"#).unwrap();
        let mut prog = Program::new(",".to_owned(), "\n".to_owned());
        prog.push_rules(rules);

        prog.consume(Record::from("1,2".to_owned(), ","));
        assert_eq!(prog.last_output(), "1,1,2,3\n".to_owned());

        prog.consume(Record::from("4,-0.5".to_owned(), ","));
        assert_eq!(prog.last_output(), "2,4,-0.5,3.5\n".to_owned());
    }
}