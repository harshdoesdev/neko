use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Span {
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Span {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenWithSpan {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),
    Boolean(bool),
    StringLiteral(String),
    InterpolatedString {
        parts: Vec<String>,
        vars: Vec<TokenWithSpan>,
    },
    Identifier(String),
    Symbol(String),
    Keyword(Keyword),
    Operator(Operator),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Fn,
    Mod,
    If,
    Elsif,
    Else,
    Match,
    For,
    In,
    Do,
    Return,
    End,
    Nil,
}

impl Keyword {
    fn from_str(ident: &str) -> Option<Self> {
        match ident {
            "nil" => Some(Keyword::Nil),
            "fn" => Some(Keyword::Fn),
            "mod" => Some(Keyword::Mod),
            "in" => Some(Keyword::In),
            "elsif" => Some(Keyword::Elsif),
            "else" => Some(Keyword::Else),
            "match" => Some(Keyword::Match),
            "for" => Some(Keyword::For),
            "if" => Some(Keyword::If),
            "do" => Some(Keyword::Do),
            "return" => Some(Keyword::Return),
            "end" => Some(Keyword::End),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
    Not,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    BraceOpen,
    BraceClose,
    Arrow,
}

impl Operator {
    fn from_str(ident: &str) -> Option<Self> {
        match ident {
            "not" => Some(Operator::Not),
            "and" => Some(Operator::And),
            "or" => Some(Operator::Or),
            _ => None,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Slash | Operator::Percent => true,
            _ => false,
        }
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum TokenError {
    #[error("Invalid number: {0}")]
    InvalidNumber(String),
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(char),
    #[error("Invalid character: {0}")]
    InvalidCharacter(char),
    #[error("Invalid operator sequence")]
    InvalidOperator,
    #[error("Invalid symbol: {0}")]
    InvalidSymbol(String),
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            input: source.chars().peekable(),
            line: 1,
            col: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<TokenWithSpan>, TokenError> {
        while let Some(&ch) = self.input.peek() {
            let start_line = self.line;
            let start_col = self.col;
    
            match ch {
                ' ' | '\t' => {
                    self.input.next();
                    self.col += 1;
                }
                '\n' => {
                    self.input.next();
                    self.line += 1;
                    self.col = 0;
                }
                '\r' => {
                    self.input.next();
                    self.col += 1;
                    if self.input.peek() == Some(&'\n') {
                        self.input.next();
                        self.col = 0;
                    }
                    self.line += 1;
                    self.col = 0;
                }
                '0'..='9' => {
                    let token = self.parse_number(false)?; // No leading minus
                    let end_line = self.line;
                    let end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token,
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                '-' => {
                    self.input.next(); // Consume the '-'
                    self.col += 1;
                    if let Some(&next_ch) = self.input.peek() {
                        if next_ch.is_ascii_digit() {
                            let token = self.parse_number(true)?; // Has leading minus
                            let end_line = self.line;
                            let end_col = self.col;
                            return Ok(Some(TokenWithSpan {
                                token,
                                span: Span::new(start_line, start_col, end_line, end_col),
                            }));
                        } else {
                            return Ok(Some(TokenWithSpan {
                                token: Token::Operator(Operator::Minus),
                                span: Span::new(start_line, start_col, self.line, self.col),
                            }));
                        }
                    } else {
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::Minus),
                            span: Span::new(start_line, start_col, self.line, self.col),
                        }));
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let token = self.parse_word_token();
                    let end_line = self.line;
                    let end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token,
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }    
                ':' => {
                    let token = self.parse_symbol_or_colon()?;
                    let end_line = self.line;
                    let end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token,
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                '"' => {
                    let token = self.parse_string()?;
                    let end_line = self.line;
                    let end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token,
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                '#' => {
                    self.skip_comment();
                    continue;
                }
                '+' => return Ok(self.consume_operator(Operator::Plus)),
                '*' => return Ok(self.consume_operator(Operator::Asterisk)),
                '/' => return Ok(self.consume_operator(Operator::Slash)),
                '%' => return Ok(self.consume_operator(Operator::Percent)),
                '=' => {
                    self.input.next();
                    self.col += 1;
                    let mut end_line = self.line;
                    let mut end_col = self.col;
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        self.col += 1;
                        end_line = self.line;
                        end_col = self.col;
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::DoubleEqual),
                            span: Span::new(start_line, start_col, end_line, end_col),
                        }));
                    }
                    if self.input.peek() == Some(&'>') {
                        self.input.next();
                        self.col += 1;
                        end_line = self.line;
                        end_col = self.col;
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::Arrow),
                            span: Span::new(start_line, start_col, end_line, end_col),
                        }));
                    }
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::Equal),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                '!' => {
                    self.input.next();
                    self.col += 1;
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        self.col += 1;
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::NotEqual),
                            span: Span::new(start_line, start_col, self.line, self.col),
                        }));
                    }
                    return Err(TokenError::InvalidOperator);
                }
                '>' => {
                    self.input.next();
                    self.col += 1;
                    let mut end_line = self.line;
                    let mut end_col = self.col;
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        self.col += 1;
                        end_line = self.line;
                        end_col = self.col;
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::GreaterEqual),
                            span: Span::new(start_line, start_col, end_line, end_col),
                        }));
                    }
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::Greater),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                '<' => {
                    self.input.next();
                    self.col += 1;
                    let mut end_line = self.line;
                    let mut end_col = self.col;
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        self.col += 1;
                        end_line = self.line;
                        end_col = self.col;
                        return Ok(Some(TokenWithSpan {
                            token: Token::Operator(Operator::LessEqual),
                            span: Span::new(start_line, start_col, end_line, end_col),
                        }));
                    }
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::Less),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                ',' => return Ok(self.consume_operator(Operator::Comma)),
                '(' => return Ok(self.consume_operator(Operator::LeftParen)),
                ')' => return Ok(self.consume_operator(Operator::RightParen)),
                '{' => return Ok(self.consume_operator(Operator::BraceOpen)),
                '}' => return Ok(self.consume_operator(Operator::BraceClose)),
                '[' => return Ok(self.consume_operator(Operator::LeftBracket)),
                ']' => return Ok(self.consume_operator(Operator::RightBracket)),
                _ => {
                    let invalid_char = self.input.next().unwrap();
                    self.col += 1;
                    return Err(TokenError::InvalidCharacter(invalid_char));
                }
            }
        }
        Ok(None)
    }

    fn consume_operator(&mut self, operator: Operator) -> Option<TokenWithSpan> {
        let start_line = self.line;
        let start_col = self.col;
        self.input.next();
        self.col += 1;
        let end_line = self.line;
        let end_col = self.col;
        Some(TokenWithSpan {
            token: Token::Operator(operator),
            span: Span::new(start_line, start_col, end_line, end_col),
        })
    }

    fn parse_symbol_or_colon(&mut self) -> Result<Token, TokenError> {
        self.input.next();
        self.col += 1;
        if let Some(&ch) = self.input.peek() {
            if ch.is_alphabetic() || ch == '_' {
                let mut symbol = String::new();
                while let Some(&ch) = self.input.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        symbol.push(ch);
                        self.input.next();
                        self.col += 1;
                    } else {
                        break;
                    }
                }
                if symbol.is_empty() {
                    Err(TokenError::InvalidSymbol("empty symbol".to_string()))
                } else {
                    Ok(Token::Symbol(symbol))
                }
            } else {
                Ok(Token::Operator(Operator::Colon))
            }
        } else {
            Ok(Token::Operator(Operator::Colon))
        }
    }

    fn parse_number(&mut self, has_minus: bool) -> Result<Token, TokenError> {
        let mut num_str = String::new();
        let mut has_dot = false;
    
        // Add the minus sign if it was already consumed
        if has_minus {
            num_str.push('-');
        }
    
        // Parse digits and optional decimal point
        while let Some(&ch) = self.input.peek() {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.input.next();
                self.col += 1;
            } else if ch == '.' && !has_dot {
                self.input.next();
                self.col += 1;
                num_str.push('.');
                has_dot = true;
                if let Some(&next_ch) = self.input.peek() {
                    if !next_ch.is_ascii_digit() && next_ch != '.' {
                        return Err(TokenError::InvalidNumber(num_str.clone()));
                    }
                } else {
                    return Err(TokenError::InvalidNumber(num_str.clone()));
                }
            } else {
                break;
            }
        }
    
        match num_str.parse::<f64>() {
            Ok(value) => Ok(Token::Number(value)),
            Err(_) => Err(TokenError::InvalidNumber(num_str)),
        }
    }
    
    fn parse_word_token(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(&ch) = self.input.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.input.next();
                self.col += 1;
            } else {
                break;
            }
        }

        if let Some(op) = Operator::from_str(&ident) {
            Token::Operator(op)
        } else if let Some(keyword) = Keyword::from_str(&ident) {
            Token::Keyword(keyword)
        } else if let Some(boolean) = Self::parse_bool(&ident) {
            return boolean;
        } else {
            Token::Identifier(ident)
        }
    }

    fn parse_string(&mut self) -> Result<Token, TokenError> {
        let mut parts = Vec::new();
        let mut current_part = String::new();
        let mut vars = Vec::new();
        let mut in_interpolation = false;
        let mut var_buffer = String::new();

        self.input.next(); // Consume the opening quote
        self.col += 1;

        while let Some(ch) = self.input.next() {
            self.col += 1;
            match ch {
                '\r' => {
                    if self.input.peek() == Some(&'\n') {
                        self.input.next();
                        self.col = 0;
                    }
                    self.line += 1;
                    self.col = 0;
                    current_part.push('\n');
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    current_part.push('\n');
                }
                '"' => {
                    if !current_part.is_empty() {
                        parts.push(current_part);
                    }
                    return Ok(if vars.is_empty() {
                        Token::StringLiteral(parts[0].clone())
                    } else {
                        Token::InterpolatedString { parts, vars }
                    });
                }
                '\\' => {
                    if let Some(escaped) = self.input.next() {
                        self.col += 1;
                        if escaped == '\r' {
                            if self.input.peek() == Some(&'\n') {
                                self.input.next();
                                self.col = 0;
                            }
                            self.line += 1;
                            self.col = 0;
                            current_part.push('\n');
                        } else if escaped == '\n' {
                            self.line += 1;
                            self.col = 0;
                            current_part.push('\n');
                        } else {
                            current_part.push(match escaped {
                                '"' => '"',
                                '#' => '#',
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                _ => return Err(TokenError::InvalidEscapeSequence(escaped)),
                            });
                        }
                    }
                }
                '#' => {
                    if let Some(&'{') = self.input.peek() {
                        self.input.next(); // Consume '{'
                        self.col += 1;
                        if !current_part.is_empty() {
                            parts.push(current_part);
                            current_part = String::new();
                        }
                        in_interpolation = true;
                        var_buffer.clear();
                    } else {
                        current_part.push('#');
                    }
                }
                '}' if in_interpolation => {
                    if !var_buffer.is_empty() {
                        let var_start_line = self.line;
                        let var_start_col = self.col - var_buffer.len() - 1; // Adjust for '}' and buffer
                        let var_end_line = self.line;
                        let var_end_col = self.col - 1; // Before '}'
                        vars.push(TokenWithSpan {
                            token: Token::Identifier(var_buffer.clone()),
                            span: Span::new(var_start_line, var_start_col, var_end_line, var_end_col),
                        });
                    }
                    in_interpolation = false;
                }
                _ => {
                    if in_interpolation {
                        if ch.is_alphanumeric() || ch == '_' {
                            var_buffer.push(ch);
                        } else {
                            return Err(TokenError::InvalidCharacter(ch));
                        }
                    } else {
                        current_part.push(ch);
                    }
                }
            }
        }

        Err(TokenError::UnterminatedString)
    }

    fn parse_bool(input: &str) -> Option<Token> {
        let Ok(value) = input.parse::<bool>() else {
            return None;
        };
        Some(Token::Boolean(value))
    }
    
    fn skip_comment(&mut self) {
        self.input.next();
        self.col += 1;
        while let Some(ch) = self.input.next() {
            self.col += 1;
            match ch {
                '\r' => {
                    if self.input.peek() == Some(&'\n') {
                        self.input.next();
                    }
                    self.line += 1;
                    self.col = 0;
                    break;
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    break;
                }
                _ => {}
            }
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<TokenWithSpan, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Some(token)) => Some(Ok(token)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

#[test]
pub fn test_negative_numbers() {
    let source = "-42 -3.14";
    let tokenizer = Tokenizer::new(source);
    let tokens: Vec<_> = tokenizer.collect::<Result<Vec<_>, _>>().unwrap();
    
    assert_eq!(
        tokens[0].token,
        Token::Number(-42.0)
    );
    assert_eq!(
        tokens[1].token,
        Token::Number(-3.14)
    );
}
