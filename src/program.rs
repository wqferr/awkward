use std::cell::RefCell;
use std::rc::Rc;

use chumsky::chain::Chain;

use crate::expr::{Rule, EvaluationContext};
use crate::record::Record;
use crate::types::{Number, Value};

pub struct Program {
    rules: Vec<Rule>,
    state: EvaluationContext,
    record_terminator: String,

    output: Rc<RefCell<String>>,
    record_number: Rc<RefCell<usize>>
}

impl Program {
    pub fn new(ofs: String, ors: String) -> Self {
        let mut s = Self {
            rules: vec![],
            state: EvaluationContext::new(ofs),
            record_terminator: ors,
            output: Rc::new(RefCell::new(String::new())),

            record_number: Rc::new(RefCell::new(0usize))
        };
        s.inject_bulitins();
        s
    }

    pub fn set_field_names(&mut self, field_names: Vec<String>) {
        self.state.set_field_names(field_names);
    }

    pub fn register_builtin<F: 'static + FnMut(Vec<Value>) -> Value>(&mut self, name: &str, f: F) {
        self.state.register_builtin(name, f);
    }
    
    pub fn field_separator(&self) -> &str {
        self.state.ofs()
    }

    pub fn record_terminator(&self) -> &str {
        self.record_terminator.as_str()
    }

    pub fn push_rules(&mut self, rules: Vec<Rule>) {
        self.rules.extend(rules.into_iter());
    }

    fn inject_bulitins(&mut self) {
        let output = self.output.clone();
        let ors = self.record_terminator.clone();
        let ofs = self.state.ofs().to_owned();
        self.register_builtin("put", move |args: Vec<Value>| {
            for v in args.iter() {
                let line_empty = output.borrow().is_empty()
                    || output
                        .borrow()
                        .ends_with(ors.as_str());
                
                if !line_empty {
                    output
                        .borrow_mut()
                        .push_str(ofs.as_str())
                }

                output
                    .borrow_mut()
                    .push_str(format!("{}", v).as_str());
            }
            Value::Bool(true)
        });

        let record_number = self.record_number.clone();
        self.register_builtin("nr", move |_args: Vec<Value>| {
            let num = Number::from_num(*record_number.borrow());
            Value::Num(num)
        });

        let separator = self.field_separator().to_owned();
        self.register_builtin("ofs", move |_| Value::Str(separator.clone()));

        let terminator = self.record_terminator().to_owned();
        self.register_builtin("ors", move |_| Value::Str(terminator.clone()));

        let output = self.output.clone();
        let ors = self.record_terminator().to_owned();
        self.register_builtin("newrec", move |_| {
            output.borrow_mut().push_str(ors.as_str());
            Value::Bool(true)
        });
    }

    pub fn consume(&mut self, record: Record) {
        self.output.borrow_mut().clear();

        self.state.set_current_record(record);
        *self.record_number.borrow_mut() += 1;
        for d in self.rules.iter() {
            d.execute_if_applies(&mut self.state);
        }
        self.output.borrow_mut()
            .push_str(self.record_terminator.as_str());
    }

    pub fn process(&mut self, record: Record) -> String {
        self.consume(record);
        self.last_output()
    }

    pub fn last_output(&self) -> String {
        self.output.borrow().to_owned()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use chumsky::Parser;

    use crate::grammar::program_parser;

    #[test]
    fn test_simple() {
        let rules = program_parser().parse(r#"
            put(nr(), @0);
            (nr() % 3 == 0) -> put("fizz");
            (nr() % 5 == 0) -> put("buzz");
        "#).unwrap();
        let mut prog = Program::new(",".to_owned(), "\n".to_owned());
        prog.push_rules(rules);

        prog.consume(Record::from("hello".to_owned(), ","));
        prog.consume(Record::from("there".to_owned(), ","));
        prog.consume(Record::from("general".to_owned(), ","));
        prog.consume(Record::from("kenobi".to_owned(), ","));
        prog.consume(Record::from("you".to_owned(), ","));
        prog.consume(Record::from("are".to_owned(), ","));
        prog.consume(Record::from("a".to_owned(), ","));
        prog.consume(Record::from("bold".to_owned(), ","));
        prog.consume(Record::from("one".to_owned(), ","));
        prog.consume(Record::from("that".to_owned(), ","));
        prog.consume(Record::from("was".to_owned(), ","));
        prog.consume(Record::from("the".to_owned(), ","));
        prog.consume(Record::from("quote".to_owned(), ","));
        prog.consume(Record::from("but".to_owned(), ","));
        prog.consume(Record::from("i".to_owned(), ","));
        prog.consume(Record::from("need".to_owned(), ","));
        prog.consume(Record::from("more".to_owned(), ","));
        prog.consume(Record::from("lines".to_owned(), ","));

        // println!("{}", prog.copy_output());
    }
}