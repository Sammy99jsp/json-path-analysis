use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref LINES: Regex = Regex::new(r"\n\r?").unwrap();
}


enum ParserError {
    EMPTY,
    UNEXPECTED_TOKEN(CharLoc),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharLoc {
    /// The character itself.
    content  : char,
    /// Line and column number
    location : (usize, usize) 
}


enum TokenParseState {
    Continue(String),
    End
}

impl TokenParseState {
    pub fn is_end(&self) -> bool {
        match self {
            End => true,
            _   => false,
        }
    }
}

pub trait TokenParser {
    fn parse(&mut self, c : CharLoc)
        -> Result<TokenParseState, ParserError>;
}

enum Token {
    ObjectLiteral,
    ArrayLiteral,
    StringLiteral,
    NumberLiteral,
}

impl TokenParser for Token {
    fn parse(&mut self, c : CharLoc)
        -> Result<TokenParseState, ParserError>
    {
        match self {
            Self::ObjectLiteral => {
                
            }
        }    
    }
}


pub struct Source {
    text : String,
    pointer : usize,
    line_lookup : Vec<(usize, usize)>
}

impl Source {

    pub fn new(mut source : String) -> Self {
        // If JSON does not end in a new-line,
        // add one
        if source.chars().last() != Some('\n') {
            source = format!("{source}\n");
        }

        let lines : Vec<_> = LINES.find_iter(&source)
            .enumerate()
            .map(| (i, m) |
                (
                    i,
                    m.end()
                )
            )
            .collect();

        Self {
            text: source,
            pointer : 0,
            line_lookup: lines
        }
    }

    pub fn next(&mut self) -> Option<CharLoc> {
        let prev = &self.text[(self.pointer)..*&self.text.len()];
        match prev.chars().next() {
            Some(content) => {
        
                let mut line_no = 0usize;
                let mut last_abs_index = 0usize; 

                for (line_index, _abs_index) in &self.line_lookup {
                    if self.pointer < (*_abs_index) {
                        line_no = *line_index;
                        break;
                    }
                    last_abs_index = *_abs_index; 
                }
        
                self.pointer += 1;

                Some(CharLoc {
                    location: (line_no + 1, self.pointer - last_abs_index),
                    content
                })},
            None => None,
        }        
    }
}

pub struct Parser;
impl Parser {
    fn parse(src : &mut Source) -> Result<(), ParserError> {
        let f = src.next();

        if f.is_none() {
            return Err(ParserError::EMPTY);
        }

        let f = f.unwrap();

        let stack : Vec<Token> = vec![Token::start(f)?];

        let curr_token = ;

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use std::fs;

    use super::{Source, CharLoc};

    #[test]
    fn test_eat() {
        let mut src = Source::new(
            fs::read_to_string(
                "./tests/1.txt"
            ).unwrap()
        );

        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'H',
                location: (1,1)
            })
        );

        src.next(); // e
        src.next(); // l
        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'l',
                location: (1,4)
            })
        );
        src.next(); // o
        src.next(); // \n

        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'T',
                location: (2,1)
            })
        );

        src.next(); // h
        src.next(); // e
        src.next(); // r
        src.next(); // e
        src.next(); // \n
        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'W',
                location: (3,1)
            })
        );

    }

    #[test]
    fn test_token() {

    }
}