use std::io::{BufRead, Write};
use thiserror::Error;

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
    fields: Vec<String>
}

impl Record {
    pub fn read(source: &mut dyn BufRead, field_sep: &str) -> anyhow::Result<Self> {
        let mut buf = String::with_capacity(500);
        source.read_line(&mut buf)?;
        
        let fields = buf
            .split(field_sep)
            .map(|x| x.to_owned())
            .collect();
        Ok(Self { fields })
    }

    pub fn write(self, sink: &mut dyn Write, field_sep: &str) -> anyhow::Result<()> {
        let mut fields = self.fields.into_iter();
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

    pub fn nth_str(&self, n: usize) -> Result<&str> {
        if n > self.fields.len() {
            return Err(Error::NoSuchField(n));
        }
        Ok(self.fields[n].as_str())
    }

    pub fn nth_int(&self, n: usize) -> Result<i32> {
        if n > self.fields.len() {
            return Err(Error::NoSuchField(n));
        }
        self.fields[n].parse::<i32>()
            .map_err(|_| Error::NumberParseFailure)
    }

    pub fn nth_float(&self, n: usize) -> Result<f32> {
        if n > self.fields.len() {
            return Err(Error::NoSuchField(n));
        }
        self.fields[n].parse::<f32>()
            .map_err(|_| Error::NumberParseFailure)
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