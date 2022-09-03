# JSON Parser and Indexer

## Features
 * Parses JSON files
 * JSONC (JSON with Comments) support with the `jsonc` crate feature.
 * Pretty parser error messages.
 * Indexes JSON files structure with each value's original line and column position in source code, which allows for custom error messages when combined with another serializing library.

## Example
```rust
use std::fs;
use json_tree::{Source, Parser};

let mut src = Source::new(fs::read_to_string("./tests/object_index.txt").unwrap());

// Parse into tokens.
let tokens = Tokenizer::tokenize(&mut src).unwrap();

// Combine tokens into a value tree.
let root = Value::parse(&mut tokens.iter().peekable());

// Make an index 
let mut index = HashMap::new();
Indexer::index(&root, &mut index, None);

for (k, v) in index.iter() {
    println!("{k} => {:?}", v.loc());
}
```