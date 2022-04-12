use std::io::{BufRead, Write};

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

    pub fn process(self, skip_blanks: bool, trim: bool) -> Self {
        let mut processed: Box<dyn Iterator<Item=String>> = Box::new(self.fields.into_iter());
        if skip_blanks {
            processed = Box::new(processed.filter(|x| !x.is_empty()));
        }
        if trim {
            processed = Box::new(processed.map(|s| s.trim().to_owned()));
        }

        Self {
            fields: processed.collect()
        }
    }

    pub fn nth(&self, n: usize) -> &str {
        self.fields.get(n)
            .map(|s| s.as_str())
            .unwrap_or("")
    }
}

impl IntoIterator for Record {
    type Item = String;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}