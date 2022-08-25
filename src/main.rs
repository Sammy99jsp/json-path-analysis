use std::collections::HashMap;

mod parser;

fn main() {
    println!("Hello, world!");
}

enum Data {
    String(String),
    Number(f64),
    Object(HashMap<String, Node>),
    Array(Vec<Node>),
}

struct Node {
    path  : String,
    name  : String,
    value : Data
} 

