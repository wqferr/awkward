use std::{io::BufRead, collections::{HashMap, HashSet}};
use thiserror::Error;

use itertools::Itertools;

use crate::expr::FieldId;

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
    field_names: HashMap<String, usize>,
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
            field_names: HashMap::new(),
            original_string: s,
            fields
        }
    }

    pub fn empty() -> Self {
        Self {
            field_names: HashMap::new(),
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

    pub fn set(&mut self, id: &FieldId, s: String) {
        let idx = self.get_idx(id);
        if idx == 0 {
            self.fields[0] = s;
            while self.fields.len() > 1 {
                self.fields.pop();
            }
            self.field_names.clear();
        } else {
            self.fields[idx-1] = s;
        }
    }

    pub fn get(&self, id: &FieldId) -> &str {
        let idx = self.get_idx(id);
        self.nth_str(idx).unwrap_or("")
    }

    pub fn get_possibly_entire_record(&self, id: &FieldId, field_sep: &str) -> String {
        let idx = self.get_idx(id);
        if idx == 0 {
            self.join(field_sep)
        } else {
            self.fields[idx-1].clone()
        }
    }

    fn get_idx(&self, id: &FieldId) -> usize {
        match id {
            FieldId::Idx(n) => n.clone(),
            FieldId::Name(name) => self.field_names[name]
        }
    }

    pub fn set_field_name(&mut self, name: String, idx: usize) {
        self.field_names.insert(name, idx);
    }

    pub fn push_field(&mut self, s: String) {
        self.fields.push(s)
    }

    pub fn has_field(&self, id: &FieldId) -> bool {
        match id {
            FieldId::Idx(idx) => idx > &0 && idx <= &self.fields.len(),
            FieldId::Name(name) => self.field_names.contains_key(name)
        }
    }

    pub fn try_delete_field(&mut self, id: &FieldId) -> bool {
        if !self.has_field(id) {
            return false;
        }
        let idx = self.get_idx(id);
        self.fields.remove(idx-1);
        let mut keys_to_be_dec = Vec::with_capacity(self.fields.len());
        let mut key_to_be_deleted = None;
        for (k, v) in self.field_names.iter() {
            if v > &idx {
                keys_to_be_dec.push(k.clone());
            } else if v == &idx {
                key_to_be_deleted = Some(k.clone());
            }
        }
        for key in keys_to_be_dec.iter() {
            self.field_names.insert(key.clone(), self.field_names[key] - 1);
        }
        if let Some(key) = key_to_be_deleted {
            self.field_names.remove(&key);
        }
        true
    }

    pub fn drop_all(&mut self, ids: &[FieldId]) {
        let mut indices = Vec::with_capacity(ids.len());
        indices.extend(ids.iter().map(|id| self.get_idx(id)));
        indices.sort_by(|a, b| b.cmp(a));
        for id in ids.iter().dedup() {
            self.try_delete_field(&id);
        }
    }

    pub fn drop_all_but(&mut self, ids: &[FieldId]) {
        let mut indices_deleted = HashSet::with_capacity(self.fields.len());
        for idx in 1..self.fields.len()+1 {
            indices_deleted.insert(idx.clone());
        }
        for id in ids {
            let idx = self.get_idx(id);
            indices_deleted.remove(&idx);
        }
        let mut indices_deleted: Vec<&usize> = indices_deleted.iter().collect();
        indices_deleted.sort_by(|a, b| b.cmp(a));
        for idx in indices_deleted {
            self.try_delete_field(&FieldId::Idx(idx.clone()));
        }
    }

    pub fn nth_str(&self, n: usize) -> Result<&str> {
        if !self.has_field(&FieldId::Idx(n)) {
            return Err(Error::NoSuchField(n));
        }
        Ok(self.fields[n-1].as_str())
    }

    pub fn nth_num(&self, n: usize) -> Result<Number> {
        if !self.has_field(&FieldId::Idx(n)) {
            return Err(Error::NoSuchField(n));
        }
        self.fields[n-1].parse::<Number>().map_err(|_| Error::NumberParseFailure)
    }

    pub fn nth_bool(&self, n: usize) -> Result<bool> {
        if !self.has_field(&FieldId::Idx(n)) {
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