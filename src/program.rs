use std::cell::RefCell;
use std::rc::Rc;

use chumsky::chain::Chain;

use crate::expr::{Declaration, EvaluationContext};
use crate::record::Record;
use crate::types::{Number, Value};

pub struct Program {
    declarations: Vec<Declaration>,
    state: EvaluationContext,
    ors: String,

    output: Rc<RefCell<String>>,
    record_number: Rc<RefCell<usize>>
}

impl Program {
    pub fn new(ofs: String, ors: String) -> Self {
        let mut s = Self {
            declarations: vec![],
            state: EvaluationContext::new(ofs),
            ors,
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

    pub fn push_decl(&mut self, decl: Declaration) {
        self.declarations.push(decl);
    }

    pub fn extend(&mut self, decls: Vec<Declaration>) {
        self.declarations.extend(decls.into_iter());
    }

    pub fn copy_output(&self) -> String {
        self.output.borrow().clone()
    }

    fn inject_bulitins(&mut self) {
        let output = self.output.clone();
        let ors = self.ors.clone();
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
    }

    pub fn next_record(&mut self, record: Record) {
        self.state.set_current_record(record);
        *self.record_number.borrow_mut() += 1;
        for d in self.declarations.iter() {
            d.execute_if_applies(&mut self.state);
        }
        self.output.borrow_mut().push_str(self.ors.as_str());
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use chumsky::Parser;

    use crate::grammar::program_parser;

    #[test]
    fn test_simple() {
        let decl = program_parser().parse(r#"
            put(nr(), @0);
            (nr() % 3 == 0) -> put("fizz");
            (nr() % 5 == 0) -> put("buzz");
        "#).unwrap();
        let mut prog = Program::new(",".to_owned(), "\n".to_owned());
        prog.extend(decl);

        prog.next_record(Record::from("hello".to_owned(), ","));
        prog.next_record(Record::from("there".to_owned(), ","));
        prog.next_record(Record::from("general".to_owned(), ","));
        prog.next_record(Record::from("kenobi".to_owned(), ","));
        prog.next_record(Record::from("you".to_owned(), ","));
        prog.next_record(Record::from("are".to_owned(), ","));
        prog.next_record(Record::from("a".to_owned(), ","));
        prog.next_record(Record::from("bold".to_owned(), ","));
        prog.next_record(Record::from("one".to_owned(), ","));
        prog.next_record(Record::from("that".to_owned(), ","));
        prog.next_record(Record::from("was".to_owned(), ","));
        prog.next_record(Record::from("the".to_owned(), ","));
        prog.next_record(Record::from("quote".to_owned(), ","));
        prog.next_record(Record::from("but".to_owned(), ","));
        prog.next_record(Record::from("i".to_owned(), ","));
        prog.next_record(Record::from("need".to_owned(), ","));
        prog.next_record(Record::from("more".to_owned(), ","));
        prog.next_record(Record::from("lines".to_owned(), ","));

        println!("{}", prog.copy_output());
    }
}