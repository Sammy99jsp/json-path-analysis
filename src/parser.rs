use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    ops::AddAssign,
};

use colored::Colorize;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref LINES: Regex = Regex::new(r"\n\r?").unwrap();
}

///
/// Arbitrary value useful for Token comparisons.
///
pub trait Unit {
    fn unit() -> Self;
}

///
/// Represents any error that can occur during
/// tokenization, or tree parsing
///
#[derive(Debug)]
pub enum ParserError<T>
where
    T: Into<TokenContent>,
{
    UnexpectedToken(T),
    UnexpectedEnd(T, char)
}

impl Into<ParserError<TokenContent>> for ParserError<CharLoc> {
    fn into(self) -> ParserError<TokenContent> {
        match self {
            Self::UnexpectedToken(s) => ParserError::UnexpectedToken(s.into()),
            Self::UnexpectedEnd(s, c) => ParserError::UnexpectedEnd(s.into(), c)
        }
    }
}

///
/// Represents the location of a single character.
/// Should Ideally be merged with TokenContent into a generic or enum.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharLoc {
    /// The character itself.
    content: char,
    /// Line and column number
    location: (usize, usize),
}

impl From<CharLoc> for String {
    fn from(c: CharLoc) -> Self {
        c.content.to_string()
    }
}

///
/// Represents the state of a token during tokenization:
/// It will either:
/// - keep accepting new characters
/// - send a signal to move on.
///
#[derive(Debug, Copy, Clone)]
pub enum ParseState {
    Continue,
    End,
}

pub fn end_escape_lookbehind(st: &str, target: char) -> bool {
    let mut count = 0usize;
    let mut start = false;
    if !st.ends_with(target) {
        return true;
    }

    for c in st.chars().rev() {
        if c == target {
            if start {
                break
            }
            start = true;
            continue;
        }

        if !start {
            continue;
        }

        match c == '\\' {
            true =>  count += 1,
            false=>  break
        } 
    }


        
    count % 2 == 1
}

pub trait TokenParser {
    ///
    /// Returns if a character is escaped by '\\' at the end of the string
    ///
    fn end_escape_lookbehind(st: &str, target: char) -> bool {
        end_escape_lookbehind(st, target)
    }

    ///
    /// Attempts to add another character to this token.
    ///
    fn parse_char(&mut self, c: CharLoc) -> Result<ParseState, ParserError<CharLoc>>;
}

///
/// Represents the source code of a token.
///
#[derive(Debug, Clone)]
pub struct TokenContent {
    ///
    /// Raw source string.
    ///
    content: String,

    ///
    /// Original (line, col) location within source code.
    ///
    start: (usize, usize),
}



impl AddAssign for TokenContent {
    fn add_assign(&mut self, rhs: Self) {
        self.content.push_str(&rhs.content);
    }
}

impl<'a> AddAssign<&'a Self> for TokenContent {
    fn add_assign(&mut self, rhs: &'a Self) {
        self.content.push_str(&rhs.content);
    }
}

impl Unit for TokenContent {
    fn unit() -> Self {
        Self {
            content: "".to_string(),
            start: (0, 0),
        }
    }
}

impl TokenContent {
    ///
    /// Add characters or strings to this tokens content.
    ///
    pub fn push<T: Into<String>>(&mut self, c: T) {
        self.content.push_str(&c.into());
    }

    ///
    /// Returns the starting index of this token.
    /// 
    pub fn loc(&self) -> (usize, usize) {
        self.start.clone()
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

impl From<TokenContent> for CharLoc {
    fn from(t: TokenContent) -> Self {
        Self {
            location: t.start,
            content: t.content.chars().nth(0).unwrap(),
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


#[cfg(feature = "jsonc")]
#[derive(Clone, Debug)]
pub enum CommentType {
    Line,
    MultiLine
}

///
/// Represents a raw text token, which has not had its syntax
/// checked.
///
#[derive(Clone)]
pub enum Token {
    ObjectOpen(TokenContent),
    ObjectClose(TokenContent),
    ArrayOpen(TokenContent),
    ArrayClose(TokenContent),
    StringLiteral(TokenContent),
    NumberLiteral(TokenContent),
    Comma(TokenContent),
    Colon(TokenContent),
    Empty(TokenContent),
    NullLiteral(TokenContent),
    __VALUE__,

    #[cfg(feature = "jsonc")]
    Comment(TokenContent, CommentType),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            Self::ObjectOpen(_) => format!("{}", "ObjectOpen".purple()),
            Self::ObjectClose(_) => format!("{}", "ObjectClose".purple()),
            Self::ArrayOpen(_) => format!("{}", "ArrayOpen".purple()),
            Self::ArrayClose(_) => format!("{}", "ArrayClose".purple()),
            Self::StringLiteral(co) => format!(
                "{}({})",
                "StringLiteral".purple(),
                format!("{}", co.content.escape_debug()).blue()
            ),
            Self::NumberLiteral(co) => format!(
                "{}({})",
                "NumberLiteral".purple(),
                format!("{}", co.content).green()
            ),
            Self::Comma(_) => format!("{}", "Comma".purple()),
            Self::Colon(_) => format!("{}", "Colon".purple()),
            Self::Empty(_) => format!("{}", "Empty".purple()),
            Self::NullLiteral(_) => format!("{}", "Null".purple()),
            Self::__VALUE__ => format!("{}", "%VALUE%".yellow()),

            #[cfg(feature = "jsonc")]
            Self::Comment(_, _) => format!("{}", "Comment".purple())
        };
        write!(f, "Token {t}")
    }
}

impl Token {
    ///
    /// Returns if this token should be ignored
    /// by the parser (e.g. Comments, blank space, etc.)
    ///  
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty(_) => true,

            #[cfg(feature = "jsonc")]
            Self::Comment(_, _) => true,

            _ => false,
        }
    }

    ///
    /// Returns if this token matches the other token
    /// used for ObjectOpen and ArrayOpen
    ///
    pub fn matches(&self, other: &Self) -> bool {
        match self {
            Self::ObjectOpen(_) => match other {
                Self::ObjectClose(_) => true,
                _ => false,
            },
            Self::ArrayOpen(_) => match other {
                Self::ArrayClose(_) => true,
                _ => false,
            },
            _ => unimplemented!(),
        }
    }

    ///
    /// Returns if this token represents a value such as:
    /// * a string literal
    /// * a number (f64) literal
    /// * a null (None) literal
    /// * an object ({}) -- only opening character `{`
    /// * an array ([]) -- only opening character `[`
    ///
    ///
    pub fn is_value(&self) -> bool {
        match self {
            Self::ArrayOpen(_) => true,
            Self::ObjectOpen(_) => true,
            Self::NumberLiteral(_) => true,
            Self::StringLiteral(_) => true,
            Self::NullLiteral(_) => true,
            _ => false,
        }
    }

    ///
    /// Checks to see if this token is the
    /// same token type as another.
    ///
    pub fn is(&self, a: &Self) -> bool {
        match a {
            Self::__VALUE__ => return self.is_value(),
            _ => {}
        }

        match self {
            Self::ArrayClose(_) => match a {
                Self::ArrayClose(_) => true,
                _ => false,
            },
            Self::ArrayOpen(_) => match a {
                Self::ArrayOpen(_) => true,
                _ => false,
            },
            Self::Colon(_) => match a {
                Self::Colon(_) => true,
                _ => false,
            },
            Self::Comma(_) => match a {
                Self::Comma(_) => true,
                _ => false,
            },
            Self::Empty(_) => match a {
                Self::Empty(_) => true,
                _ => false,
            },
            Self::NullLiteral(_) => match a {
                Self::NullLiteral(_) => true,
                _ => false,
            },
            Self::NumberLiteral(_) => match a {
                Self::NumberLiteral(_) => true,
                _ => false,
            },
            Self::ObjectClose(_) => match a {
                Self::ObjectClose(_) => true,
                _ => false,
            },
            Self::ObjectOpen(_) => match a {
                Self::ObjectOpen(_) => true,
                _ => false,
            },
            Self::StringLiteral(_) => match a {
                Self::StringLiteral(_) => true,
                _ => false,
            },
            Self::__VALUE__ => a.is_value(),
            
            #[cfg(feature = "jsonc")]
            Self::Comment(_, _) => match a {
                Self::Comment(_, _) => true,
                _ => false
            },
        }
    }

    ///
    /// Gets the content from this token.
    ///
    pub fn content(&self) -> &TokenContent {
        match self {
            Self::ArrayClose(l) => l,
            Self::ArrayOpen(l) => l,
            Self::Colon(l) => l,
            Self::Comma(l) => l,
            Self::Empty(l) => l,
            Self::NullLiteral(l) => l,
            Self::NumberLiteral(l) => l,
            Self::ObjectClose(l) => l,
            Self::ObjectOpen(l) => l,
            Self::StringLiteral(l) => l,
            Self::__VALUE__ => unimplemented!(),

            #[cfg(feature = "jsonc")]
            Self::Comment(l, _) => l,
        }
    }
}

impl TryFrom<CharLoc> for Token {
    type Error = ParserError<CharLoc>;

    fn try_from<'a>(c: CharLoc) -> Result<Self, Self::Error> {
        match c.content {
            #[cfg(feature = "jsonc")]
            '/' => Ok(Self::Comment(c.into(), CommentType::Line)),

            '{' => Ok(Self::ObjectOpen(c.into())),
            '}' => Ok(Self::ObjectClose(c.into())),

            '[' => Ok(Self::ArrayOpen(c.into())),
            ']' => Ok(Self::ArrayClose(c.into())),

            ',' => Ok(Self::Comma(c.into())),
            ':' => Ok(Self::Colon(c.into())),

            'n' => Ok(Self::NullLiteral(c.into())),

            '"' => Ok(Self::StringLiteral(c.into())),

            ch if ch.is_ascii_digit() => Ok(Self::NumberLiteral(c.into())),
            '-' => Ok(Self::NumberLiteral(c.into())),

            ch if ch.is_ascii_whitespace() => Ok(Self::Empty(c.into())),

            _ => Err(ParserError::UnexpectedToken(c)),
        }
    }
}

impl TokenParser for Token {
    fn parse_char(&mut self, chr: CharLoc) -> Result<ParseState, ParserError<CharLoc>> {
        match self {
            Self::__VALUE__ => unimplemented!(),

            Self::Empty(content) => match chr.content {
                ch if ch.is_ascii_whitespace() => {
                    content.push(chr);
                    Ok(ParseState::Continue)
                }
                _ => Ok(ParseState::End),
            },

            Self::Comma(_) => Ok(ParseState::End),
            Self::Colon(_) => Ok(ParseState::End),
            Self::ArrayClose(_) => Ok(ParseState::End),
            Self::ArrayOpen(_) => Ok(ParseState::End),
            Self::ObjectOpen(_) => Ok(ParseState::End),
            Self::ObjectClose(_) => Ok(ParseState::End),

            Self::NullLiteral(content) => match content.content.as_str() {
                "n" => match chr.content {
                    'u' => {
                        content.push(chr);
                        Ok(ParseState::Continue)
                    }
                    _ => Err(ParserError::UnexpectedToken(chr)),
                },
                "nu" => match chr.content {
                    'l' => {
                        content.push(chr);
                        Ok(ParseState::Continue)
                    }
                    _ => Err(ParserError::UnexpectedToken(chr)),
                },
                "nul" => match chr.content {
                    'l' => {
                        content.push(chr);
                        Ok(ParseState::Continue)
                    }
                    _ => Err(ParserError::UnexpectedToken(chr)),
                },
                "null" => match chr.content {
                    ch if ch.is_ascii_whitespace() => Ok(ParseState::End),
                    ',' | '}' | ']' => Ok(ParseState::End),
                    _ => Err(ParserError::UnexpectedToken(chr)),
                },
                _ => Err(ParserError::UnexpectedToken(content.clone().into())),
            },

            Self::NumberLiteral(content) => {
                match chr.content {
                    ch if ch.is_ascii_digit() => {
                        content.push(chr);
                        Ok(ParseState::Continue)
                    }
                    // Disallow multiple .'s in numbers
                    '.' => {
                        if !content.content.contains(".") {
                            content.push(chr);
                            return Ok(ParseState::Continue);
                        }
                        Err(ParserError::UnexpectedToken(chr))
                    }
                    ch if ch.is_ascii_whitespace() => {
                        if content.content.ends_with(".") {
                            Err(ParserError::UnexpectedToken(chr))
                        } else {
                            Ok(ParseState::End)
                        }
                    }
                    ',' | ':' | '{' | '}' | '[' | ']' => Ok(ParseState::End),
                    _ => Err(ParserError::UnexpectedToken(chr)),
                }
            }

            Self::StringLiteral(content) => {
                match Token::end_escape_lookbehind(&content.content, '"') {
                    false => {
                        if content.content.len() > 1 {
                            return Ok(ParseState::End);
                        }
                    },
                    true => {}
                }

                match chr.content {
                    '\n' => return Err(ParserError::UnexpectedToken(chr)),
                    _ => {}
                }
                
                content.push(chr);
                return Ok(ParseState::Continue);
            }
        
            #[cfg(feature = "jsonc")]
            Self::Comment(content, comment_type) => {
                match content.content.len() {
                    1 => match chr.content {
                        '*' => {
                            // Make a multiline comment.
                            *comment_type = CommentType::MultiLine;
                            content.push(chr);
                            return Ok(ParseState::Continue);
                        },
                        '/' => {
                            // Make a multiline comment.
                            *comment_type = CommentType::Line;
                            content.push(chr);
                            return Ok(ParseState::Continue);
                        },
                        _ => return Err(ParserError::UnexpectedToken(chr))
                    },
                    _ => {
                        println!("Inside a {:?} comment!", comment_type);
                        match comment_type {
                            CommentType::Line => {
                                if content.content.ends_with("\n") {
                                    return Ok(ParseState::End);
                                }

                                content.push(chr);
                                return Ok(ParseState::Continue);
                            },
                            CommentType::MultiLine => {
                                if content.content.ends_with("*/") {
                                    return Ok(ParseState::End);
                                }

                                content.push(chr);
                                return Ok(ParseState::Continue);
                            }
                        }
                    }
                }
            }
        }
    }
}

///
/// Represents a source code file.
///
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
                    content: *content,
                })
            }
            None => None,
        }
    }

    pub fn back(&mut self) {
        self.pointer -= 1;
    }
}

///
/// Utility struct that transforms text into tokens.
///
/// See Tokenizer::tokenize(...) for more information
///
pub struct Tokenizer;

impl Tokenizer {
    ///
    /// Transforms source code text into tokens.
    ///
    /// ## Example
    /// ```rust
    /// let mut src = Source::new(r#"{"source": "code"}"#);
    ///
    /// match Tokenizer::tokenize(&mut src) {
    ///     // Do something cool with the text tokens,
    ///     // such as putting them into a tree
    ///     Ok(t) => todo!()
    ///
    ///     // Print a prettily-formatted error to console.
    ///     Err(err) => println!("{}", err)
    /// }
    /// ```
    ///
    pub fn tokenize(src: &mut Source) -> Result<Vec<Token>, ParserError<CharLoc>> {
        let mut tokens: Vec<Token> = vec![];
        let mut current_token: Option<Token> = None;

        while let Some(c) = src.next() {
            match current_token.clone() {
                Some(mut token) => {
                    match token.parse_char(c)? {
                        // If this new character cannot be part
                        //  of the current token, push the current token,
                        //  and redo this last character.
                        ParseState::End => {
                            tokens.push(token);
                            current_token = None;
                            src.back();
                        }
                        ParseState::Continue => {
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

///
/// Represents a parsed JSON data value in a tree.
///
#[derive(Debug, Clone)]
pub enum Value {
    ///
    /// Represents a JSON Object, such as:
    ///
    /// ```json
    /// {
    ///   "key": "value",
    ///   "one": 1
    /// }
    /// ```
    ///
    Object(TokenContent, Vec<(Value, Value)>),

    ///
    /// Represents a JSON Array, such as:
    ///
    /// ```json
    /// [ "one", 2, "three", 4 ]
    /// ```
    ///
    Array(TokenContent, Vec<Value>),

    ///
    /// Represents a string literal in JSON (`String` in Rust), such as:
    ///
    /// ```json
    /// "Orphaned string!"
    /// ```
    ///
    StringLiteral(TokenContent, String),

    ///
    /// Represents a number literal in JSON (`f64` in Rust) such as:
    /// ```json
    /// 3.14159265358979
    /// ```
    ///
    NumberLiteral(TokenContent, f64),

    ///
    /// Represents a null literal in JSON (`None` in Rust).
    ///
    /// Example 1 (orphaned):
    /// ```json
    /// null
    /// ```
    ///
    /// Example 2 (in context):
    /// ```json
    /// {
    ///   "type": "taco",
    ///   "contents": [
    ///     { "avocado" : "🥑" },
    ///     { "mysterious_meat" : null }
    ///   ]
    /// }
    /// ```
    ///
    ///
    NullLiteral(TokenContent),
}

impl Value {
    ///
    /// Parses a single JSON value from a collection of Tokens.
    ///
    /// ## Example
    ///
    /// To parse:
    /// ```json
    /// { "abc": [1, 2, 3] }
    /// ```
    /// Method:
    /// ```rust
    /// let mut src = Source::new(r#"{ "abc": [1, 2, 3] }"#);
    ///
    /// // We can unwrap in this example, since it's valid JSON.
    /// let tokens = Tokenizer::tokenize(&mut src).unwrap();
    ///
    /// // The .peekable() is needed so that any current token can be checked twice
    /// // before manually advancing the Iterator.
    /// let value = Value::parse(&mut tokens.iter().peekable()).unwrap();
    ///
    /// println!("{value}");
    /// ```
    ///
    pub fn parse<'a>(
        iter: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Value, ParserError<TokenContent>> {
        loop {
            let tkn = match iter.peek().clone() {
                Some(tkn) => tkn.clone(),
                None => break,
            };

            if tkn.is_empty() {
                continue;
            }
            match tkn.is_value() {
                true => match tkn {
                    // Data structures.
                    Token::ArrayOpen(_) => {
                        iter.next();
                        let t = Self::_parse_array(iter, tkn);
                        return t;
                    }
                    Token::ObjectOpen(_) => {
                        iter.next();
                        let t = Self::_parse_object(iter, tkn);
                        return t;
                    }

                    // Primitives.
                    Token::NullLiteral(_) | Token::NumberLiteral(_) | Token::StringLiteral(_) => {
                        iter.next();
                        return Ok(tkn.try_into().unwrap());
                    }

                    // Others should be already covered by the .is_value() check
                    _ => unreachable!(),
                },
                false => return Err(ParserError::UnexpectedToken(tkn.content().clone())),
            }
        }
        todo!()
    }

    ///
    /// Internal helper function to parse arrays.
    ///
    fn _parse_array<'a>(
        iter: &mut Peekable<impl Iterator<Item = &'a Token>>,
        parent_tkn: &Token,
    ) -> Result<Value, ParserError<TokenContent>> {
        let format = [Token::__VALUE__, Token::Comma(TokenContent::unit())];

        let mut format_i = 0;

        let mut acc_location = parent_tkn.content().clone();

        let mut contents: Vec<Value> = vec![];
        let mut last_loc: TokenContent = TokenContent::unit();
        
        
        while let Some(tkn) = iter.peek() {
            last_loc = tkn.content().clone();
            println!("Expected {}", format[format_i]);
            println!("{}", tkn);
            acc_location += tkn.content();

            if tkn.is_empty() {
                iter.next();
                continue;
            }

            match tkn {
                Token::ArrayClose(_) => match format_i % 2 {
                    0 => {
                        if contents.len() == 0 {
                            iter.next();
                            println!("\n");
                            return Ok(Self::Array(acc_location, contents));
                        }

                        return Err(ParserError::UnexpectedToken(tkn.content().clone()))
                    },
                    1 => {
                        iter.next();
                        println!("\n");
                        return Ok(Self::Array(acc_location, contents));
                    }
                    _ => unreachable!(),
                },
                _ => {}
            }

            match tkn.is(&format[format_i]) {
                true => match format_i % 2 {
                    0 => {
                        contents.push(Self::parse(iter)?);
                    }
                    1 => {
                        iter.next();
                    }
                    _ => unreachable!(),
                },
                false => return Err(ParserError::UnexpectedToken(tkn.content().clone())),
            }

            format_i = (format_i + 1) % 2;
        }

        Err(ParserError::UnexpectedEnd(last_loc, ']'))
    }

    ///
    /// Internal helper function to parse objects.
    ///
    fn _parse_object<'a>(
        iter: &mut Peekable<impl Iterator<Item = &'a Token>>,
        parent_tkn: &Token,
    ) -> Result<Value, ParserError<TokenContent>> {
        // we're parsing {...}
        // check against our format until a }

        // Expected format { "<key>": <value>, ... }
        let format = [
            Token::StringLiteral(TokenContent::unit()), // The key
            Token::Colon(TokenContent::unit()),
            Token::__VALUE__, // The Value
            Token::Comma(TokenContent::unit()),
        ];

        // Tracks current index of the format (because spaces exist!).
        let mut format_i = 0;

        let mut acc_location = parent_tkn.content().clone();

        //   Represents        (    Key    ,    Value   )
        let mut contents: Vec<(Value, Value)> = vec![];

        let mut key: Option<Value> = None;

        let mut i = -1;

        let mut last_loc: TokenContent = TokenContent::unit();

        while let Some(tkn) = iter.peek() {
            last_loc = tkn.content().clone();
            i += 1;
            println!("E: {}", format[format_i]);
            println!("{} {}", i.to_string().color("#FFAA00"), tkn);

            acc_location += tkn.content();

            // Skip whitespace tokens.
            if tkn.is_empty() {
                iter.next();
                continue;
            }

            match tkn {
                Token::ObjectClose(_) => match format_i % 4 {
                    3 => {
                        iter.next();
                        println!("\n");
                        return Ok(Value::Object(acc_location, contents));
                    },
                    0 => {
                        if contents.len() == 0 {
                            iter.next();
                            println!("\n");
                            return Ok(Value::Object(acc_location, contents));
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            match tkn.is(&format[format_i]) {
                true => match format_i {
                    0 => {
                        key = Some((*tkn).try_into().unwrap());
                        iter.next();
                    }
                    2 => contents.push((key.clone().unwrap(), Self::parse(iter)?)),
                    1 | 3 => {
                        iter.next();
                    }
                    _ => unreachable!(),
                },
                false => return Err(ParserError::UnexpectedToken(tkn.content().clone())),
            }

            format_i = (format_i + 1) % 4;
        }

        Err(ParserError::UnexpectedEnd(last_loc, '}'))
    }

    ///
    /// Internal helper function to neatly display all contents of a value (and any possible children).
    ///
    fn display(&self, level: Option<usize>) -> String {
        let level = level.unwrap_or(0);

        // Type thing
        let token_type = match self {
            Self::Array(_, _) => "Array",
            Self::Object(_, _) => "Object",
            Self::StringLiteral(_, _) => "String",
            Self::NumberLiteral(_, _) => "Number",
            Self::NullLiteral(_) => "Null",
        }
        .truecolor(241, 113, 5);

        // Extra info
        let extra = match self {
            Self::Array(_, c) => format!("({})", c.len().to_string().cyan()),
            Self::Object(_, c) => format!("({})", c.len().to_string().cyan()),
            Self::StringLiteral(_, c) => format!("(\"{}\")", c.blue()),
            Self::NumberLiteral(_, c) => format!("({})", c.to_string().bright_green()),
            _ => "".to_string(),
        };

        // Line and Column number
        let (start_line, start_column) = match self {
            Self::Array(l, _) => l.start,
            Self::Object(l, _) => l.start,
            Self::StringLiteral(l, _) => l.start,
            Self::NumberLiteral(l, _) => l.start,
            Self::NullLiteral(l) => l.start,
        };

        let location = format!("{start_line}:{start_column}").blue();

        let mut lines: Vec<String> = vec![format!(
            "{pad}[{location}] {token_type}{extra}",
            pad = "  ".repeat(level)
        )];

        match self {
            Self::Array(_, c) => lines.append(
                &mut c
                    .iter()
                    .map(|f| f.display(Some(level + 1)))
                    .collect::<Vec<_>>(),
            ),
            Self::Object(_, c) => {
                lines.append(
                    &mut c
                        .iter()
                        .map(|(k, v)| {
                            format!(
                                "{} => {}",
                                k.display(Some(level + 1)),
                                v.display(Some(level + 1))
                            )
                        })
                        .collect::<Vec<_>>(),
                );
            }
            _ => {}
        }

        lines.join("\n")
    }

    pub fn content(&self) -> &TokenContent {
        match self {
            Value::Object(l, _) => l,
            Value::Array(l, _) => l,
            Value::StringLiteral(l, _) => l,
            Value::NumberLiteral(l, _) => l,
            Value::NullLiteral(l) => l,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display(None))
    }
}

impl<'a> TryFrom<&'a Token> for Value {
    type Error = String;

    fn try_from(value: &'a Token) -> Result<Self, Self::Error> {
        match value {
            Token::StringLiteral(p) => Ok(Self::StringLiteral(
                p.clone(),
                p.content[1..(p.content.len() - 1)].to_string(),
            )),
            Token::NumberLiteral(p) => Ok(Self::NumberLiteral(
                p.clone(),
                p.content.parse::<f64>().unwrap(),
            )),
            Token::NullLiteral(p) => Ok(Self::NullLiteral(p.clone())),
            _ => Err("Cannot directly convert this value to ParsedToken".to_string()),
        }
    }
}

impl<'a, T> Display for ParserError<T>
where
    T: Into<TokenContent> + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(e) => {
                let tmp: TokenContent = T::into(e.clone());
                write!(
                    f,
                    "{} {} {}\n  {} at {}",
                    "Error -".red(),
                    "Unexpected Token".bold().red(),
                    format!("\"{}\"", tmp.content.escape_debug()).blue(),
                    "-->".blue().bold(),
                    format!("{}:{}", tmp.start.0, tmp.start.1).yellow().bold(),
                )
            },
            Self::UnexpectedEnd(s, e) => {
                let tmp: TokenContent = T::into(s.clone());
                write!(
                    f,
                    "{} {}\n  after {} --> Expected a '{}'",
                    "Error -".red(),
                    "Unexpected End of token".bold().red(),
                    format!("{}:{}", tmp.start.0, tmp.start.1).yellow().bold(),
                    e
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        parser::{CharLoc, Token, TokenParser},
        Source, TokenContent,
    };

    use super::{Tokenizer, Value};

    #[test]
    fn test_eat() {
        let mut src = Source::new(fs::read_to_string("./tests/eater.txt").unwrap());

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

        content.push('p');
        content.push('p');
        content.push('l');
        content.push('e');
        content.push('s');
        content.push('!');

        println!("{:?}", content)
    }

    #[test]
    fn test_escape_lookbehind() {
        assert_eq!(
            Token::end_escape_lookbehind(r"\\\\'", '\''),
            false
        );

        assert_eq!(Token::end_escape_lookbehind(r"\\\'", '\''), true);
        
        
        assert_eq!(Token::end_escape_lookbehind(r"\\      '", '\''), false);
        assert_eq!(Token::end_escape_lookbehind(r"abcdefghijk", '\''), true);

    }

    #[test]
    fn test_value() {
        let mut src = Source::new(fs::read_to_string("./tests/simple_heirarchy.json").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Ok(tks) => match Value::parse(&mut tks.iter().peekable()) {
                Ok(e) => println!("{}", e.display(None)),
                Err(e) => println!("{e}"),
            },
            Err(err) => println!("{}", err),
        }
    }

    #[cfg(feature = "jsonc")]
    #[test]
    fn test_comments() {
        let mut src = Source::new(fs::read_to_string("./tests/comments.jsonc").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Ok(tks) => match Value::parse(&mut tks.iter().peekable()) {
                Ok(e) => println!("{}", e.display(None)),
                Err(e) => println!("{e}"),
            },
            Err(err) => println!("{}", err),
        }
    }

    #[test]
    fn empty_object() {
        let mut src = Source::new(fs::read_to_string("./tests/empty_object.json").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Ok(tks) => match Value::parse(&mut tks.iter().peekable()) {
                Ok(e) => println!("{}", e.display(None)),
                Err(e) => println!("{e}"),
            },
            Err(err) => println!("{}", err),
        }
    }

    #[test]
    fn empty_array() {
        let mut src = Source::new(fs::read_to_string("./tests/empty_array.json").unwrap());

        let tokens = Tokenizer::tokenize(&mut src);

        match tokens {
            Ok(tks) => match Value::parse(&mut tks.iter().peekable()) {
                Ok(e) => println!("{}", e.display(None)),
                Err(e) => println!("{e}"),
            },
            Err(err) => println!("{}", err),
        }
    }
}
