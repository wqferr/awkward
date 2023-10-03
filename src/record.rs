use std::io::BufRead;
use thiserror::Error;

use super::types::Number;

#[derive(Debug, Error)]
pub enum Error {
    #[error("field could not be parsed as a number")]
    NumberParseFailure,

    #[error("field could not be parsed as a bool")]
    BoolParseFailure,

    #[error("field with index {0} does not exist")]
    NoSuchField(usize),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Record {
    original_string: String,
    fields: Vec<String>
}

impl Record {
    pub fn from(s: String, field_sep: &str) -> Self {
        let fields = s
            .split(field_sep)
            .map(|x| x.to_owned())
            .collect();
        Self {
            original_string: s,
            fields
        }
    }

    pub fn empty() -> Self {
        Self {
            original_string: "".to_owned(),
            fields: vec![]
        }
    }

    pub fn read(source: &mut dyn BufRead, field_sep: &str) -> anyhow::Result<Self> {
        let mut buf = String::with_capacity(500);
        source.read_line(&mut buf)?;
        Ok(Self::from(buf, field_sep))
    }

    pub fn join(&self, field_sep: &str) -> String {
        self.fields.join(field_sep)
    }

    pub fn process(&mut self, skip_blanks: bool, trim: bool) {
        let mut processed: Box<dyn Iterator<Item=&str>> = Box::new(
            self.fields.iter()
                .map(|s| s.as_str())
        );
        if skip_blanks {
            processed = Box::new(processed.filter(|&x| !x.is_empty()));
        }
        if trim {
            processed = Box::new(processed.map(|s| s.trim()));
        }

        self.fields = processed.map(|s| s.to_owned()).collect();
    }

    pub fn set(&mut self, n: usize, s: String) {
        if n == 0 {
            self.fields[0] = s;
            while self.fields.len() > 1 {
                self.fields.pop();
            }
        } else {
            for _ in self.fields.len()..n {
                self.fields.push("".to_owned());
            }
            self.fields[n-1] = s;
        }
    }

    pub fn push_field(&mut self, s: String) {
        self.fields.push(s)
    }

    pub fn has_field(&self, idx: usize) -> bool {
        idx > 0 && idx <= self.fields.len()
    }

    pub fn try_delete_field(&mut self, idx: usize) -> bool {
        if self.has_field(idx) {
            self.fields.remove(idx-1);
            true
        } else {
            false
        }
    }

    pub fn field(&self, n: usize) -> &str {
        self.nth_str(n).unwrap_or("")
    }

    pub fn nth_str(&self, n: usize) -> Result<&str> {
        if !self.has_field(n) {
            return Err(Error::NoSuchField(n));
        }
        Ok(self.fields[n-1].as_str())
    }

    pub fn nth_num(&self, n: usize) -> Result<Number> {
        if !self.has_field(n) {
            return Err(Error::NoSuchField(n));
        }
        self.fields[n-1].parse::<Number>().map_err(|_| Error::NumberParseFailure)
    }

    pub fn nth_bool(&self, n: usize) -> Result<bool> {
        if !self.has_field(n) {
            return Err(Error::NoSuchField(n));
        }
        match self.fields[n-1].as_str() {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => Err(Error::BoolParseFailure)
        }
    }

    pub fn original_string(&self) -> &str {
        self.original_string.as_str()
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }
}

impl IntoIterator for Record {
    type Item = String;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}