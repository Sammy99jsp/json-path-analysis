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
let tokens = Tokenizer::tokenize(&mut src).unwrap();


```