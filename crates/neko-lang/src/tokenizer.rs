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
        matches!(
            self,
            Operator::Plus
                | Operator::Minus
                | Operator::Asterisk
                | Operator::Slash
                | Operator::Percent
        )
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
    #[error("Unterminated variable interpolation")]
    UnterminatedInterpolation,
    #[error("Empty interpolation")]
    EmptyInterpolation,
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    current_char: Option<char>,
    line: usize,
    col: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            input: source.chars().peekable(),
            current_char: None,
            line: 1,
            col: 0,
        }
    }

    fn consume_char(&mut self) -> Option<char> {
        self.current_char = self.input.next();
        self.current_char
    }

    pub fn next_token(&mut self) -> Result<Option<TokenWithSpan>, TokenError> {
        while let Some(ch) = self.input.peek() {
            match ch {
                ' ' | '\t' => {
                    self.consume_char();
                    self.col += 1;
                }
                '\n' => {
                    self.consume_char();
                    self.line += 1;
                    self.col = 0;
                }
                '\r' => {
                    self.consume_char();
                    self.col += 1;
                    if self.input.peek() == Some(&'\n') {
                        self.consume_char();
                        self.col = 0;
                    }
                    self.line += 1;
                    self.col = 0;
                }
                _ => break,
            }
        }

        let Some(&ch) = self.input.peek() else {
            return Ok(None);
        };

        let start_line = self.line;
        let start_col = self.col;

        match ch {
            '0'..='9' => {
                let token = self.parse_number(false)?; // No leading minus
                let end_line = self.line;
                let end_col = self.col;
                Ok(Some(TokenWithSpan {
                    token,
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            '-' => {
                let was_alphanumeric = self
                    .current_char
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false);

                self.consume_char();
                self.col += 1;

                if was_alphanumeric {
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::Minus),
                        span: Span::new(start_line, start_col, self.line, self.col),
                    }));
                }

                if self
                    .input
                    .peek()
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
                {
                    let token = self.parse_number(true)?;
                    return Ok(Some(TokenWithSpan {
                        token,
                        span: Span::new(start_line, start_col, self.line, self.col),
                    }));
                }

                Ok(Some(TokenWithSpan {
                    token: Token::Operator(Operator::Minus),
                    span: Span::new(start_line, start_col, self.line, self.col),
                }))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let token = self.parse_word_token();
                let end_line = self.line;
                let end_col = self.col;
                Ok(Some(TokenWithSpan {
                    token,
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            ':' => {
                let token = self.parse_symbol_or_colon()?;
                let end_line = self.line;
                let end_col = self.col;
                Ok(Some(TokenWithSpan {
                    token,
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            '"' => {
                let token = self.parse_string()?;
                let end_line = self.line;
                let end_col = self.col;
                Ok(Some(TokenWithSpan {
                    token,
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            '#' => {
                self.skip_comment();
                self.next_token()
            }
            '+' => Ok(self.consume_operator(Operator::Plus)),
            '*' => Ok(self.consume_operator(Operator::Asterisk)),
            '/' => Ok(self.consume_operator(Operator::Slash)),
            '%' => Ok(self.consume_operator(Operator::Percent)),
            '=' => {
                self.consume_char();
                self.col += 1;
                let mut end_line = self.line;
                let mut end_col = self.col;
                if self.input.peek() == Some(&'=') {
                    self.consume_char();
                    self.col += 1;
                    end_line = self.line;
                    end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::DoubleEqual),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                if self.input.peek() == Some(&'>') {
                    self.consume_char();
                    self.col += 1;
                    end_line = self.line;
                    end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::Arrow),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                Ok(Some(TokenWithSpan {
                    token: Token::Operator(Operator::Equal),
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            '!' => {
                self.consume_char();
                self.col += 1;
                if self.input.peek() == Some(&'=') {
                    self.consume_char();
                    self.col += 1;
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::NotEqual),
                        span: Span::new(start_line, start_col, self.line, self.col),
                    }));
                }
                Err(TokenError::InvalidOperator)
            }
            '>' => {
                self.consume_char();
                self.col += 1;
                let mut end_line = self.line;
                let mut end_col = self.col;
                if self.input.peek() == Some(&'=') {
                    self.consume_char();
                    self.col += 1;
                    end_line = self.line;
                    end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::GreaterEqual),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                Ok(Some(TokenWithSpan {
                    token: Token::Operator(Operator::Greater),
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            '<' => {
                self.consume_char();
                self.col += 1;
                let mut end_line = self.line;
                let mut end_col = self.col;
                if self.input.peek() == Some(&'=') {
                    self.consume_char();
                    self.col += 1;
                    end_line = self.line;
                    end_col = self.col;
                    return Ok(Some(TokenWithSpan {
                        token: Token::Operator(Operator::LessEqual),
                        span: Span::new(start_line, start_col, end_line, end_col),
                    }));
                }
                Ok(Some(TokenWithSpan {
                    token: Token::Operator(Operator::Less),
                    span: Span::new(start_line, start_col, end_line, end_col),
                }))
            }
            ',' => Ok(self.consume_operator(Operator::Comma)),
            '(' => Ok(self.consume_operator(Operator::LeftParen)),
            ')' => Ok(self.consume_operator(Operator::RightParen)),
            '{' => Ok(self.consume_operator(Operator::BraceOpen)),
            '}' => Ok(self.consume_operator(Operator::BraceClose)),
            '[' => Ok(self.consume_operator(Operator::LeftBracket)),
            ']' => Ok(self.consume_operator(Operator::RightBracket)),
            c => {
                self.col += 1;
                Err(TokenError::InvalidCharacter(c))
            }
        }
    }

    fn consume_operator(&mut self, operator: Operator) -> Option<TokenWithSpan> {
        let start_line = self.line;
        let start_col = self.col;
        self.consume_char();
        self.col += 1;
        let end_line = self.line;
        let end_col = self.col;
        Some(TokenWithSpan {
            token: Token::Operator(operator),
            span: Span::new(start_line, start_col, end_line, end_col),
        })
    }

    fn parse_symbol_or_colon(&mut self) -> Result<Token, TokenError> {
        self.consume_char();
        self.col += 1;
        if let Some(&ch) = self.input.peek() {
            if ch.is_alphabetic() || ch == '_' {
                let mut symbol = String::new();
                while let Some(&ch) = self.input.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        symbol.push(ch);
                        self.consume_char();
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
                self.consume_char();
                self.col += 1;
            } else if ch == '.' && !has_dot {
                self.consume_char();
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
                self.consume_char();
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

        self.consume_char(); // Consume the opening quote
        self.col += 1;

        while let Some(ch) = self.consume_char() {
            self.col += 1;
            match ch {
                '"' => {
                    if in_interpolation {
                        return Err(TokenError::UnterminatedInterpolation);
                    }
                    if !current_part.is_empty() {
                        parts.push(current_part);
                    }
                    return Ok(if vars.is_empty() && parts.len() == 1 {
                        Token::StringLiteral(parts[0].clone())
                    } else {
                        Token::InterpolatedString { parts, vars }
                    });
                }
                '\\' => {
                    if in_interpolation {
                        var_buffer.push('\\');
                    } else if let Some(escaped) = self.consume_char() {
                        self.col += 1;
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
                '#' => {
                    if in_interpolation {
                        var_buffer.push('#');
                    } else if self.input.peek() == Some(&'{') {
                        self.consume_char(); // Consume '{'
                        self.col += 1;
                        parts.push(current_part);
                        current_part = String::new();
                        in_interpolation = true;
                        var_buffer.clear();
                    } else {
                        current_part.push('#');
                    }
                }
                '}' if in_interpolation => {
                    if var_buffer.is_empty() {
                        return Err(TokenError::EmptyInterpolation);
                    }
                    let var_start_line = self.line;
                    let var_start_col = self.col - var_buffer.len() - 1;
                    let var_end_line = self.line;
                    let var_end_col = self.col - 1;
                    vars.push(TokenWithSpan {
                        token: Token::Identifier(var_buffer.clone()),
                        span: Span::new(var_start_line, var_start_col, var_end_line, var_end_col),
                    });
                    in_interpolation = false;
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    if in_interpolation {
                        var_buffer.push('\n');
                    } else {
                        current_part.push('\n');
                    }
                }
                ch => {
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
        self.consume_char();
        self.col += 1;
        while let Some(ch) = self.consume_char() {
            self.col += 1;
            match ch {
                '\r' => {
                    if self.input.peek() == Some(&'\n') {
                        self.consume_char();
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

impl Iterator for Tokenizer<'_> {
    type Item = Result<TokenWithSpan, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_negative_numbers() {
        let source = "-42 -3.14";
        let tokenizer = Tokenizer::new(source);
        let tokens: Vec<_> = tokenizer.collect::<Result<Vec<_>, _>>().unwrap();

        assert_eq!(tokens[0].token, Token::Number(-42.0));
        assert_eq!(tokens[1].token, Token::Number(-3.14));
    }

    #[test]
    fn test_newline_between_tokens() {
        let source = "name =\nadd";
        let tokenizer = Tokenizer::new(source);
        let tokens: Vec<_> = tokenizer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens[0].token, Token::Identifier("name".to_string()));
        assert_eq!(tokens[0].span.start_line, 1);
        assert_eq!(tokens[1].token, Token::Operator(Operator::Equal));
        assert_eq!(tokens[1].span.start_line, 1);
        assert_eq!(tokens[2].token, Token::Identifier("add".to_string()));
        assert_eq!(tokens[2].span.start_line, 2);
    }
}
