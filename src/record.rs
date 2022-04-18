use std::io::{BufRead, Write};
use thiserror::Error;

use super::types::Number;

#[derive(Debug, Error)]
pub enum Error {
    #[error("field could not be parsed as a number")]
    NumberParseFailure,

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
            fields: fields
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

    pub fn write(&self, sink: &mut dyn Write, field_sep: &str) -> anyhow::Result<()> {
        let mut fields = self.fields.iter();
        if let Some(field) = fields.next() {
            sink.write(field.as_bytes())?;

            for field in fields {
                sink.write(field_sep.as_bytes())?;
                sink.write(field.as_bytes())?;
            }
        }
        Ok(())
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
        for _ in self.fields.len()..=n {
            self.fields.push("".to_owned());
        }
        self.fields[n-1] = s;
    }

    pub fn has_field(&self, idx: usize) -> bool {
        idx > 0 && idx <= self.fields.len()
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

    pub fn original_string(&self) -> &str {
        &self.original_string.as_str()
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