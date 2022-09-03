use std::{collections::HashMap, fmt::Debug};

use crate::{Value::{self, *}, TokenContent, JSONPath,};

///
/// Helper enum to allow for reference to both a value
/// and its key (if exists)
/// 
pub enum Location {
    Value(TokenContent),
    KeyValue(TokenContent, TokenContent)
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(arg0) => {
                f
                    .debug_tuple("Value")
                    .field(&format!("{:?}", arg0.loc()))
                    .finish()
            },
            Self::KeyValue(arg0, arg1) => 
                f.debug_map()
                    .entry(&"Key", &format!("{:?}", arg0.loc()))
                    .entry(&"Value", &format!("{:?}", arg1.loc()))
                    .finish(),
        }
    }
}

impl<'a> From<&'a TokenContent> for Location {
    fn from(a: &'a TokenContent) -> Self {
        Self::Value(a.clone())
    }
}

impl<'a> From<(&'a TokenContent, &'a TokenContent)> for Location {
    fn from((k, v): (&'a TokenContent, &'a TokenContent)) -> Self {
        Self::KeyValue(k.clone(), v.clone())
    }
}

///
/// Helper struct for indexing a parsed JSON file.
/// 
pub struct Indexer;

impl Indexer {
    ///
    /// Index a JSON root value into a HashMap of JSONPath => TokenContent (source location)
    /// 
    pub fn index(val : &Value, map : &mut HashMap<JSONPath, Location>, path: Option<JSONPath>) {
        let path = path.unwrap_or(JSONPath::default());

        map.insert(path.clone(), val.content().into());

        match val {
            StringLiteral(l, _)
            | NumberLiteral(l, _)
            | NullLiteral(l) => {
                map.insert(path.clone(), l.into());
            },
            Object(_, c) => {
                
                for (k, v) in c {
                    // Maps the Location in the index to the location of the value 
                    match k {
                        StringLiteral(_, val) => {
                            map.insert(
                                path.push(val.clone()), 
                                (k.content(), v.content()).into()
                            );
                        },
                        _ => unimplemented!()
                    }

                    match v {
                        StringLiteral(_, _) | NumberLiteral(_, _) | NullLiteral(_) => continue,
                        _ => {}
                    }

                    Self::index(v, map, Some(path.push(k)));
                }
            },
            Array(l, arr) => {
                map.insert(path.clone(), l.into());
                for (i, v) in arr.iter().enumerate() {

                    let p = path.push(i);
                    map.insert(p.clone(), v.content().into());

                    match v {
                        StringLiteral(_, _) | NumberLiteral(_, _) | NullLiteral(_) => {},
                        _ => {
                            Self::index(v, map, Some(p));
                        }
                    } 
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, fs};

    use crate::{Source, parser::Tokenizer, Value};

    use super::Indexer;

    #[test]
    fn index_object() {
        let mut src = Source::new(fs::read_to_string("./tests/object_index.json").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Err(e) => println!("{}", e),
            Ok(tkns) => {
                let vs = Value::parse(&mut tkns.iter().peekable());
                match vs {
                    Err(err) => println!("{}", err),
                    Ok(val) => {
                        let mut index = HashMap::new();
                        Indexer::index(&val, &mut index, None);


                        for (k, v) in index.iter() {
                            println!("{k} => {:?}", v);
                        }

                    }
                }
            }
        }
    }
}