use std::{collections::HashMap};

use crate::{Value, TokenContent};

pub struct Indexer;

impl Indexer {
    pub fn index(val : &Value) -> HashMap<String, &TokenContent> {
        let map : HashMap<String, &TokenContent> = HashMap::new();
        todo!()
    }
}