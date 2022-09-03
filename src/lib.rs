//!
//! A collection of utilities to assist
//! in parsing and traversing JSON files in Rust.
//! 
//! ## Why ?
//! The main focus of this crate is to allow easy parsing 
//! of JSON, with a focus on maintaining a clear route
//! back to the original JSON source code.
//! 
//! This crate can assist in providing more useful errors
//! with JSON files for use cases such as configs.
//! 
//! Instead of a General Error, such as:
//! ```txt
//!     Invalid value for "mood": "rambunctious"
//! ```
//! 
//! A more specific and useful (à la Rust) error can be given:
//! ```txt
//!     Invalid value for "mood" at [69:42]
//!         "mood" : "rambunctious"
//!                   ^^^^^^^^^^^^    
//! ```
//! The above can aid users in fixing the problem much sooner as it gives specific line and column numbers.
//! 

pub mod parser;
pub mod indexer;
pub mod path;

pub use crate::parser::Source;
pub use crate::parser::ParserError;
pub use crate::parser::TokenContent;
pub use crate::parser::Value;
pub use crate::indexer::Indexer;
pub use crate::parser::Tokenizer;
pub use crate::indexer::Location;
pub use crate::path::JSONPath;
pub use crate::path::PathSegment;

///
/// Parses JSON files into a Value,
/// which can then be further analyzed.  
/// 
pub struct Parser;

impl Parser {
    pub fn parse<T>(
        src : &mut Source,

    ) -> Result<Value, ParserError<TokenContent>>
    {
        let tokens = Tokenizer::tokenize(src)
            .map_err(|p| p.into())?;
        
        Value::parse(&mut tokens.iter().peekable())
    }
}