use std::fmt::{Display, Write, Debug};

use colored::Colorize;
use lazy_static::lazy_static;
use regex::{internal::Char, Regex};

lazy_static! {
    static ref LINES: Regex = Regex::new(r"\n\r?").unwrap();
}

#[derive(Debug)]
enum ParserError {
    Empty,
    UnexpectedToken(CharLoc),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharLoc {
    /// The character itself.
    content: char,
    /// Line and column number
    location: (usize, usize),
}

impl From<CharLoc> for char {
    fn from(c: CharLoc) -> Self {
        c.content
    }
}

#[derive(Debug, Copy, Clone)]
enum TokenParseState {
    Continue,
    End,
}

impl TokenParseState {
    pub fn is_end(&self) -> bool {
        match self {
            End => true,
            _ => false,
        }
    }
}

pub trait TokenParser {
    ///
    /// Returns if a character is escaped by '\\' at the end of the string
    ///
    fn end_escape_lookbehind(st: &str, target: char) -> bool {
        let mut count = 0usize;
        let mut start = false;
        for c in st.chars().rev() {
            if c == target {
                start = true;
                continue;
            }

            if c == '\\' && start {
                count += 1;
            }
        }

        return count % 2 == 1;
    }

    fn parse(&mut self, c: CharLoc) -> Result<TokenParseState, ParserError>;
}

#[derive(Debug, Clone)]
struct TokenContent {
    content: String,
    start: (usize, usize),
}

impl TokenContent {
    pub fn append<T: Into<char>>(&mut self, c: T) {
        self.content.push(c.into());
    }
}

impl From<CharLoc> for TokenContent {
    fn from(c: CharLoc) -> Self {
        Self {
            content: c.content.to_string(),
            start: c.location,
        }
    }
}

impl<'a> From<&'a str> for TokenContent {
    fn from(c: &'a str) -> Self {
        Self {
            content: c.to_string(),
            start: (1, 1),
        }
    }
}

#[derive(Clone)]
enum Token {
    ObjectOpen(TokenContent),
    ObjectClose(TokenContent),
    ArrayOpen(TokenContent),
    ArrayClose(TokenContent),
    StringLiteral(TokenContent),
    NumberLiteral(TokenContent),
    Comma(TokenContent),
    Colon(TokenContent),
    Empty(TokenContent),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        let t = match self {
            Self::ObjectOpen(_)     => format!("{}", "ObjectOpen".purple()),
            Self::ObjectClose(_)    => format!("{}", "ObjectClose".purple()),
            Self::ArrayOpen(_)      => format!("{}", "ArrayOpen".purple()),
            Self::ArrayClose(_)     => format!("{}", "ArrayClose".purple()),
            Self::StringLiteral(co)  => format!("{}({})", "StringLiteral".purple(), format!("{}", co.content).blue()),
            Self::NumberLiteral(co)  => format!("{}({})", "NumberLiteral".purple(), format!("{}", co.content).green()),
            Self::Comma(_)          => format!("{}", "Comma".purple()),
            Self::Colon(_)          => format!("{}", "Colon".purple()),
            Self::Empty(_)          => format!("{}", "Empty".purple()),

        };
        write!(f, "Token {t}")
    }
}


impl Token {}

impl TryFrom<CharLoc> for Token {
    type Error = ParserError;

    fn try_from(c: CharLoc) -> Result<Self, Self::Error> {
        match c.content {
            '{' => Ok(Self::ObjectOpen(c.into())),
            '}' => Ok(Self::ObjectClose(c.into())),

            '[' => Ok(Self::ArrayOpen(c.into())),
            ']' => Ok(Self::ArrayClose(c.into())),

            ',' => Ok(Self::Comma(c.into())),
            ':' => Ok(Self::Colon(c.into())),

            '"' => Ok(Self::StringLiteral(c.into())),

            ch if ch.is_ascii_digit() => Ok(Self::NumberLiteral(c.into())),

            ch if ch.is_ascii_whitespace() => Ok(Self::Empty(c.into())),

            _ => Err(ParserError::UnexpectedToken(c)),
        }
    }
}

impl TokenParser for Token {
    fn parse(&mut self, chr: CharLoc) -> Result<TokenParseState, ParserError> {
        match self {
            Self::Empty(content) => match chr.content {
                ch if ch.is_ascii_whitespace() => {
                    content.append(chr);
                    Ok(TokenParseState::Continue)
                }
                _ => Ok(TokenParseState::End),
            },

            Self::Comma(_) => Ok(TokenParseState::End),
            Self::Colon(_) => Ok(TokenParseState::End),
            Self::ArrayClose(_) => Ok(TokenParseState::End),
            Self::ArrayOpen(_) => Ok(TokenParseState::End),
            Self::ObjectOpen(_) => Ok(TokenParseState::End),
            Self::ObjectClose(_) => Ok(TokenParseState::End),

            Self::NumberLiteral(content) => {
                match chr.content {
                    ch if ch.is_ascii_digit() => {
                        content.append(chr);
                        Ok(TokenParseState::Continue)
                    },
                    // Disallow multiple .'s in numbers
                    '.' => {
                        if !content.content.contains(".") {
                            content.append(chr);
                            return Ok(TokenParseState::Continue);
                        }
                        Err(ParserError::UnexpectedToken(chr))
                    },
                    ch if ch.is_ascii_whitespace() => {
                        if content.content.ends_with(".") {
                            Err(ParserError::UnexpectedToken(chr))
                        } else {
                            Ok(TokenParseState::End)
                        }
                    },
                    ',' | ':' | '{' | '}' | '[' | ']' => Ok(TokenParseState::End),
                    _ => Err(ParserError::UnexpectedToken(chr)),
                }
            }

            Self::StringLiteral(content) => {
                if content.content.ends_with("\"") {
                    // Has the string got an escaped " ? 
                    if Token::end_escape_lookbehind(&content.content, '"') {
                        content.append(chr);
                        return Ok(TokenParseState::Continue);
                    } else {
                        if content.content.len() > 1 {
                            match chr.content {
                                ch if ch.is_whitespace() => {
                                    return Ok(TokenParseState::End);
                                },
                                _ => return Err(ParserError::UnexpectedToken(chr))
                            }
                        }
                    }
                }

                content.append(chr);
                Ok(TokenParseState::Continue)
            }
        }
    }
}

pub struct Source {
    text: Vec<char>,
    pointer: usize,
    line_lookup: Vec<(usize, usize)>,
}

impl Source {
    pub fn new(mut source: String) -> Self {
        // If JSON does not end in a new-line,
        // add one
        if source.chars().last() != Some('\n') {
            source = format!("{source}\n");
        }

        let lines: Vec<_> = LINES
            .find_iter(&source)
            .enumerate()
            .map(|(i, m)| (i, m.end()))
            .collect();

        Self {
            text: source.char_indices().map(|(_, c)| c).collect::<Vec<char>>(),
            pointer: 0,
            line_lookup: lines,
        }
    }

    pub fn next(&mut self) -> Option<CharLoc> {
        let prev = &self.text[(self.pointer)..*&self.text.len()];
        match prev.iter().next() {
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
                    content : *content,
                })
            }
            None => None,
        }
    }

    pub fn back(&mut self) {
        self.pointer -= 1;
    }
}

struct Tokenizer;

impl Tokenizer {
    fn tokenize(src : &mut Source) -> Result<Vec<Token>, ParserError> {
        let mut tokens: Vec<Token> = vec![];
        let mut current_token: Option<Token> = None;

        while let Some(c) = src.next() {
            match current_token.clone() {
                Some(mut token) => {
                    match token.parse(c)? {
                        // If this new character cannot be part
                        //  of the current token, push the current token,
                        //  and redo this last character.
                        TokenParseState::End => {
                            tokens.push(token);
                            current_token = None;
                            src.back();
                        }
                        TokenParseState::Continue => {
                            current_token = Some(token);
                        }
                    }
                }
                None => {
                    current_token = Some(c.try_into()?);
                }
            }
        }

        if let Some(t) = &current_token {
            tokens.push(t.clone());
        }

        Ok(tokens)
    } 
}

#[cfg(test)]
mod tests {
    use std::{fs, fmt::Display};

    use colored::Colorize;

    use crate::parser::TokenParseState;

    use super::{CharLoc, ParserError, Source, Token, TokenContent, TokenParser, Tokenizer};

    impl Display for ParserError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::UnexpectedToken(e) => {
                    write!(f, "{} {} {}\n  {} at {}",
                        "Error -".red(),
                        "Unexpected Token".bold().red(),
                        format!("\"{}\"", e.content).blue(),
                        "-->".blue().bold(),
                        format!("{}:{}", e.location.0, e.location.1).yellow().bold(),
                    )
                },
                Self::Empty => todo!()
            }
        }
    }

    #[test]
    fn test_eat() {
        let mut src = Source::new(fs::read_to_string("./tests/1.txt").unwrap());

        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'H',
                location: (1, 1)
            })
        );

        src.next(); // e
        src.next(); // l
        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'l',
                location: (1, 4)
            })
        );
        src.next(); // o
        src.next(); // \n

        assert_eq!(
            src.next(),
            Some(CharLoc {
                content: 'T',
                location: (2, 1)
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
                location: (3, 1)
            })
        );
    }

    #[test]
    fn test_token_content() {
        let mut content: TokenContent = "a".into();

        content.append('p');
        content.append('p');
        content.append('l');
        content.append('e');
        content.append('s');
        content.append('!');

        println!("{:?}", content)
    }

    #[test]
    fn test_escape_lookbehind() {
        assert_eq!(Token::end_escape_lookbehind(r"\\\\'", '\''), false);

        assert_eq!(Token::end_escape_lookbehind(r"\\\'", '\''), true);
    }

    #[test]
    fn test_token() {
        let mut src = Source::new(fs::read_to_string("./tests/2.txt").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Ok(tks) => {
                for t in tks {
                    println!("{}", t);
                }
            },
            Err(err) => println!("{}", err)
        }
    }
}
