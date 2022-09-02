use std::{fmt::Display, iter::Peekable, slice::Iter, str::FromStr};

use colored::Colorize;
use lazy_static::lazy_static;
use regex::Regex;

use crate::parser::{end_escape_lookbehind, TokenParser};
pub trait Segment {
    fn _path_to_string(&self) -> String;
    fn _path_to_string_debug(&self) -> String;
}

impl Segment for String {
    fn _path_to_string(&self) -> String {
        lazy_static! {
            // Key that would not be able to be used
            // with the JS "." syntax -- instead use ["%key%"]
            static ref UNSAFE_KEY : Regex = Regex::new("[^a-zA-Z_]+.*").unwrap();
        }

        match UNSAFE_KEY.is_match(&self) {
            true => format!(r#"["{}"]"#, self),
            false => format!(".{}", self),
        }
    }

    fn _path_to_string_debug(&self) -> String {
        lazy_static! {
            // Key that would not be able to be used
            // with the JS "." syntax -- instead use ["%key%"]
            static ref UNSAFE_KEY : Regex = Regex::new("[^a-zA-Z_]+.*").unwrap();
        }

        match UNSAFE_KEY.is_match(&self) {
            true => format!(r#"["{}"]"#, self.blue()),
            false => match self.len() {
                0 => format!(r#"["{}"]"#, self.blue()),
                _ => format!(".{}", self.blue())
            },
        }
    }
}

impl Segment for usize {
    fn _path_to_string(&self) -> String {
        format!("[{}]", self)
    }
    fn _path_to_string_debug(&self) -> String {
        format!("[{}]", self.to_string().bright_green())
    }
}

enum PathSegment {
    Index(usize),
    Key(String),
}

impl PathSegment {
    fn display(&self) -> String {
        match self {
            Self::Index(i) => i._path_to_string(),
            Self::Key(k) => k._path_to_string(),
        }
    }
}

impl Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a = match self {
            Self::Index(i) => i._path_to_string_debug(),
            Self::Key(i) => i._path_to_string_debug(),
        };
        write!(f, "{}", a)
    }
}

impl Into<String> for PathSegment {
    fn into(self) -> String {
        self.display()
    }
}
impl<'a> Into<String> for &'a PathSegment {
    fn into(self) -> String {
        self.display()
    }
}

impl From<String> for PathSegment {
    fn from(k: String) -> Self {
        Self::Key(k)
    }
}

impl<'a> From<&'a str> for PathSegment {
    fn from(k: &'a str) -> Self {
        Self::Key(k.to_string())
    }
}

impl From<usize> for PathSegment {
    fn from(i: usize) -> Self {
        Self::Index(i)
    }
}

pub struct JSONPath(Vec<PathSegment>);

impl Into<String> for JSONPath {
    fn into(self) -> String {
        let v: Vec<String> = self.0.iter().map(|s| s.into()).collect();
        format!("${}", v.concat())
    }
}

impl<'a> Into<String> for &'a JSONPath {
    fn into(self) -> String {
        let v: Vec<String> = self.0.iter().map(|s| s.into()).collect();
        format!("${}", v.concat())
    }
}

impl Display for JSONPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "$".truecolor(223, 84, 42))?;
        for s in self.0.iter() {
            s.fmt(f)?
        }

        std::fmt::Result::Ok(())
    }
}

impl TryFrom<String> for JSONPath {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[derive(Clone, Debug)]
        enum Delim {
            Open,
            Close,
        }

        struct ParseError(String);

        impl From<ParseError> for String {
            fn from(s: ParseError) -> String {
                format!("Unexpected token '{}'", s.0)
            }
        }

        #[derive(Clone, Debug)]
        enum Token {
            StringLiteral(String),
            NumberLiteral(String),
            Bracket(Delim),
            Identifier(String),
            Dot,
            Root,
        }

        enum ParseState {
            Continue,
            End,
        }

        impl Token {
            fn parse(&mut self, ch: char) -> Result<ParseState, ParseError> {
                use Token::*;

                match self {
                    Root => Ok(ParseState::End),
                    NumberLiteral(content) => match ch {
                        c if c.is_ascii_digit() => {
                            content.push(c);
                            Ok(ParseState::Continue)
                        }
                        ']' => Ok(ParseState::End),
                        _ => Err(ParseError(format!(
                            "Unexpected token '{}'",
                            ch.escape_debug()
                        ))),
                    },
                    Identifier(content) => match ch {
                        '.' | '[' => Ok(ParseState::End),
                        c if c.is_alphabetic() || c.is_ascii_digit() => {
                            content.push(c);
                            Ok(ParseState::Continue)
                        }
                        '_' | '$' => {
                            content.push(ch);
                            Ok(ParseState::Continue)
                        }
                        _ => Err(ParseError(format!(
                            "Unexpected token '{}'",
                            ch.escape_debug()
                        ))),
                    },
                    Dot => Ok(ParseState::End),
                    Bracket(_) => Ok(ParseState::End),
                    StringLiteral(content) => {
                        match end_escape_lookbehind(content, '"') {
                            true => {}
                            false => {
                                return match ch {
                                    ']' => {
                                        *content =
                                            content.trim_end_matches(|s| s == '"').to_string();
                                        Ok(ParseState::End)
                                    }
                                    _ => Err(ParseError(format!(
                                        "Unexpected token '{}'",
                                        ch.escape_debug()
                                    ))),
                                }
                            }
                        }

                        match ch {
                            '\n' => {
                                return Err(ParseError(format!(
                                    "Unexpected token '{}'",
                                    ch.escape_debug()
                                )))
                            }
                            _ => {}
                        }

                        content.push(ch);
                        Ok(ParseState::Continue)
                    }
                }
            }

            fn is(&self, o: Self) -> bool {
                use Token::*;
                match self {
                    Dot => match o {
                        Dot => true,
                        _ => false,
                    },
                    Root => match o {
                        Root => true,
                        _ => false,
                    },
                    Bracket(Delim::Close) => match o {
                        Bracket(Delim::Close) => true,
                        _ => false,
                    },
                    Bracket(Delim::Open) => match o {
                        Bracket(Delim::Open) => true,
                        _ => false,
                    },
                    Identifier(_) => match o {
                        Identifier(_) => true,
                        _ => false,
                    },
                    NumberLiteral(_) => match o {
                        NumberLiteral(_) => true,
                        _ => false,
                    },
                    StringLiteral(_) => match o {
                        StringLiteral(_) => true,
                        _ => false,
                    },
                }
            }
        }

        impl TryFrom<char> for Token {
            type Error = ParseError;
            fn try_from(value: char) -> Result<Self, Self::Error> {
                use Token::*;
                match value {
                    '\n' => Err(ParseError("\n".to_string())),
                    '$' => Ok(Root),
                    '.' => Ok(Dot),
                    '[' => Ok(Bracket(Delim::Open)),
                    ']' => Ok(Bracket(Delim::Close)),
                    '"' => Ok(StringLiteral("".to_string())),

                    c if c.is_ascii_digit() => Ok(NumberLiteral(c.to_string())),
                    c if c.is_ascii_alphanumeric() => Ok(Identifier(c.to_string())),

                    c => Err(ParseError(c.to_string())),
                }
            }
        }

        let mut list: Vec<Token> = vec![];
        let mut current: Option<Token> = None;
        let mut it = value.chars().into_iter().peekable();
        while let Some(chr) = it.peek() {
            match current.as_mut() {
                None => current = Some((*chr).try_into()?),
                Some(t) => match t.parse(*chr)? {
                    ParseState::Continue => {}
                    ParseState::End => {
                        list.push(t.clone());
                        current = None;
                        continue;
                    }
                },
            }

            it.next();
        }

        list.push(current.unwrap());

        // Syntax Analysis
        let mut iter = list.iter().peekable();

        let mut end_path: Vec<PathSegment> = vec![];

        let mut has_root = false;

        fn handle_identifier(iter: &mut Peekable<Iter<Token>>) -> Result<PathSegment, ParseError> {
            if !iter.peek().unwrap().is(Token::Dot) {
                return Err(ParseError(format!("Unexpected .")));
            }

            iter.next();
            let t = iter.next();

            match t {
                None => Err(ParseError(format!("Expected identifier after ."))),
                Some(t) => match t {
                    Token::Identifier(c) => Ok(PathSegment::Key(c.clone())),
                    _ => Err(ParseError(format!("Expected identifier after ."))),
                },
            }
        }

        fn handle_bracket_value(
            iter: &mut Peekable<Iter<Token>>,
        ) -> Result<PathSegment, ParseError> {
            if !iter.peek().unwrap().is(Token::Bracket(Delim::Open)) {
                return Err(ParseError("Unexpected [".to_string()));
            }

            iter.next();
            let tkn = iter.next();

            let inside = match tkn {
                None => return Err(ParseError("Expected String or Integer Literal".to_string())),
                Some(v) => match v {
                    Token::StringLiteral(content) => Ok(PathSegment::Key(content.clone())),
                    Token::NumberLiteral(content) => match content.parse::<usize>() {
                        Ok(c) => Ok(PathSegment::Index(c)),
                        Err(_) => return Err(ParseError("Invalid integer!".to_string())),
                    },
                    _ => return Err(ParseError("Expected String or Integer Literal".to_string())),
                },
            }?;

            let tkn = iter.next();

            match tkn {
                None => Err(ParseError("Expected ']'".to_string())),
                Some(tk) => match tk.is(Token::Bracket(Delim::Close)) {
                    true => Ok(inside),
                    false => Err(ParseError("Unexpected token; expected ']'".to_string())),
                },
            }
        }

        while let Some(tkn) = iter.peek() {
            use Token::*;
            if !has_root {
                match tkn {
                    Root => {
                        has_root = true;
                        iter.next();
                        continue;
                    }
                    _ => return Err(format!("Expected root token '$'")),
                }
            }

            match tkn {
                Root => return Err(format!("Cannot have multiple root tokens!")),
                Dot => {
                    end_path.push(handle_identifier(&mut iter)?);
                }
                Bracket(Delim::Open) => {
                    end_path.push(handle_bracket_value(&mut iter)?);
                }
                StringLiteral(_) => return Err(format!("Unexpected string literal!")),
                NumberLiteral(_) => return Err(format!("Unexpected number literal!")),
                Identifier(_) => return Err(format!("Unexpected identifier!")),
                Bracket(Delim::Close) => return Err(format!("Unexpected closing square bracket!")),
            }
        }

        Ok(Self(end_path))
    }
}

#[cfg(test)]
mod tests {
    use super::JSONPath;

    #[test]
    fn display() {
        let p = JSONPath(vec![
            "hello_there".into(),
            1.into(),
            r#""good job!""#.into(),
        ]);

        println!("{}", p);
    }
}


#[macro_export]
macro_rules! path {
    ($ root : tt $ path : tt $path2 : expr) => {
        let p = format!("{}{}{}", stringify!($root), stringify!($path), stringify!($path2));

        let p : JSONPath = p.try_into().unwrap();

        println!("{}", p);
    };
    ($ root : tt $ path : expr) => {
        let p = format!("{}{}", stringify!($root), stringify!($path));

        let p : JSONPath = p.try_into().unwrap();

        println!("{}", p);
    };
}

#[cfg(test)]
mod test {
    use super::JSONPath;

    #[test]
    fn help() {
        let key = "string";
        path!($["apple"].key["apples"][0]);
    }

    #[test]
    fn parsing() {
        let p: JSONPath = r#"$["apples are cool!"][1]."#.to_string().try_into().unwrap();
        println!("{}", p);
    }
}
