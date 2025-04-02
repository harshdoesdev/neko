use neko_lang::tokenizer::{Keyword, Operator, Span, Token, TokenError, TokenWithSpan};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Nil,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    StringLiteral(String),
    InterpolatedString {
        parts: Vec<String>,
        vars: Vec<AstNode>,
    },
    List(Vec<AstNode>),
    Map(Vec<(AstNode, AstNode)>),
    Identifier(String),
    Symbol(String),
    UnaryExpr {
        op: Operator,
        expr: Box<AstNode>,
    },
    BinaryExpr {
        left: Box<AstNode>,
        op: Operator,
        right: Box<AstNode>,
    },
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Box<AstNode>,
    },
    AnonFunctionDef {
        params: Vec<String>,
        body: Box<AstNode>,
    },
    FunctionCall {
        name: String,
        args: Vec<AstNode>,
    },
    IfStmt {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        elsif_branches: Vec<(Box<AstNode>, Box<AstNode>)>, // (condition, block) pairs
        else_branch: Option<Box<AstNode>>,
    },
    ForStmt {
        item: String,           // The identifier (e.g., "item")
        iterator: Box<AstNode>, // The iterator expression
        body: Box<AstNode>,     // The loop body
    },
    Block(Vec<AstNode>),
    Assignment {
        name: String,
        value: Box<AstNode>,
    },
    LocalVariableDef {
        name: String,
        value: Box<AstNode>,
    },
    Return(Box<AstNode>),
    Subscript {
        name: String,
        indexes: Vec<AstNode>,
    },
    SubscriptAssignment {
        name: String,
        indexes: Vec<AstNode>,
        value: Box<AstNode>,
    },
    PropertyAccess {
        base: Box<AstNode>,
        property: Box<AstNode>,
    },
    PropertyAssignment {
        base: Box<AstNode>,
        property: Box<AstNode>,
        value: Box<AstNode>,
    },
}

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("Unexpected token at {span:?}: {token:?}")]
    UnexpectedToken { token: Token, span: Span },
    #[error("Unexpected end of input")]
    UnexpectedEof,
    #[error("Tokenization error: {0:?}")]
    TokenizationError(TokenError),
    #[error("Assignment operator must be on the same line as the identifier at {span:?}")]
    AssignmentNewline { span: Span },
}

pub struct Parser<I>
where
    I: Iterator<Item = Result<TokenWithSpan, TokenError>>,
{
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Result<TokenWithSpan, TokenError>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, ParseError> {
        self.parse_block(vec![])
    }

    fn parse_block(&mut self, terminators: Vec<Keyword>) -> Result<AstNode, ParseError> {
        let mut statements = Vec::new();
        while let Some(token) = self.peek()? {
            match token {
                Token::Keyword(keyword) if terminators.contains(keyword) => break,
                _ => {
                    statements.push(self.parse_statement()?);
                }
            }
        }
        Ok(AstNode::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParseError> {
        match self.peek()? {
            Some(Token::Keyword(Keyword::Fn)) => self.parse_function_definition(),
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Token::Keyword(Keyword::If)) => self.parse_if_statement(),
            Some(Token::Keyword(Keyword::For)) => self.parse_for_statement(),
            Some(Token::Keyword(Keyword::Local)) => self.parse_local_variable_definition(),
            Some(Token::Identifier(_) | Token::Symbol(_)) => self.parse_identifier_statement(),
            Some(_) => {
                let TokenWithSpan { token, span } =
                    self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                Err(ParseError::UnexpectedToken { token, span })
            }
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_local_variable_definition(&mut self) -> Result<AstNode, ParseError> {
        self.consume(&Token::Keyword(Keyword::Local))?;

        let (name, ident_span) = match self.next_token()? {
            Some(TokenWithSpan {
                token: Token::Identifier(name),
                span,
            }) => (name, span),
            Some(_) => {
                let TokenWithSpan { token, span } =
                    self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                return Err(ParseError::UnexpectedToken { token, span });
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        match self.peek()? {
            Some(Token::Operator(Operator::Equal)) => {
                let equal_token = self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                if equal_token.span.start_line != ident_span.start_line {
                    return Err(ParseError::UnexpectedToken {
                        token: equal_token.token,
                        span: equal_token.span,
                    });
                }
                let next_token_span = match self.peek_with_span()? {
                    Some(TokenWithSpan { span, .. }) => *span,
                    None => return Err(ParseError::UnexpectedEof),
                };
                if next_token_span.start_line != ident_span.start_line {
                    return Err(ParseError::AssignmentNewline {
                        span: next_token_span,
                    });
                }
                let value = self.parse_expression()?;
                Ok(AstNode::LocalVariableDef {
                    name,
                    value: Box::new(value),
                })
            }
            Some(_) => {
                let TokenWithSpan { token, span } =
                    self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                Err(ParseError::UnexpectedToken { token, span })
            }
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_for_statement(&mut self) -> Result<AstNode, ParseError> {
        self.expect(&Token::Keyword(Keyword::For))?;

        let item = match self.next_token()? {
            Some(TokenWithSpan {
                token: Token::Identifier(name),
                ..
            }) => name,
            Some(TokenWithSpan { token, span }) => {
                return Err(ParseError::UnexpectedToken { token, span })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        self.expect(&Token::Keyword(Keyword::In))?;

        let iterator = self.parse_expression()?;

        let body = self.parse_block(vec![Keyword::End])?;

        self.expect(&Token::Keyword(Keyword::End))?;

        Ok(AstNode::ForStmt {
            item,
            iterator: Box::new(iterator),
            body: Box::new(body),
        })
    }

    fn parse_conditional_block(&mut self) -> Result<AstNode, ParseError> {
        self.parse_block(vec![Keyword::End, Keyword::Elsif, Keyword::Else])
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, ParseError> {
        self.expect(&Token::Keyword(Keyword::If))?;

        let condition = self.parse_expression()?;

        self.expect(&Token::Keyword(Keyword::Do))?;

        let then_branch = self.parse_conditional_block()?;

        let mut elsif_branches = Vec::new();
        let mut else_branch = None;

        loop {
            match self.peek()? {
                Some(Token::Keyword(Keyword::Elsif)) => {
                    self.next_token()?;
                    let elsif_condition = self.parse_expression()?;
                    self.expect(&Token::Keyword(Keyword::Do))?;
                    let elsif_block = self.parse_conditional_block()?;
                    elsif_branches.push((Box::new(elsif_condition), Box::new(elsif_block)));
                }
                Some(Token::Keyword(Keyword::Else)) => {
                    self.next_token()?;
                    let else_block = self.parse_conditional_block()?;
                    self.expect(&Token::Keyword(Keyword::End))?;
                    else_branch = Some(Box::new(else_block));
                    break;
                }
                Some(Token::Keyword(Keyword::End)) => {
                    self.next_token()?;
                    break;
                }
                Some(_) => {
                    let TokenWithSpan { token, span } =
                        self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                    return Err(ParseError::UnexpectedToken { token, span });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        Ok(AstNode::IfStmt {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            elsif_branches,
            else_branch,
        })
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, ParseError> {
        self.consume(&Token::Keyword(Keyword::Return))?;
        let value = self.parse_expression()?;

        match self.peek()? {
            Some(Token::Keyword(Keyword::End)) => Ok(AstNode::Return(Box::new(value))),
            Some(Token::Keyword(Keyword::Else) | Token::Keyword(Keyword::Elsif)) => {
                Ok(AstNode::Return(Box::new(value)))
            }
            Some(_) => {
                let TokenWithSpan { token, span } =
                    self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                Err(ParseError::UnexpectedToken { token, span })
            }
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_function_definition(&mut self) -> Result<AstNode, ParseError> {
        self.consume(&Token::Keyword(Keyword::Fn))?;

        if matches!(self.peek()?, Some(Token::LeftParen)) {
            return self.parse_anonymous_function();
        }

        let name = match self.next_token()? {
            Some(TokenWithSpan {
                token: Token::Identifier(name),
                ..
            }) => name,
            Some(TokenWithSpan { token, span }) => {
                return Err(ParseError::UnexpectedToken { token, span })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        let params = self.parse_function_params()?;
        let body = self.parse_block(vec![Keyword::End])?;

        self.expect(&Token::Keyword(Keyword::End))?;

        Ok(AstNode::FunctionDef {
            name,
            params,
            body: Box::new(body),
        })
    }

    fn parse_anonymous_function(&mut self) -> Result<AstNode, ParseError> {
        let params = self.parse_function_params()?;
        let body = match self.peek()? {
            Some(&Token::Arrow) => {
                self.next_token()?;
                return Ok(AstNode::AnonFunctionDef {
                    params,
                    body: Box::new(AstNode::Block(vec![self.parse_expression()?])),
                });
            }
            Some(_) => self.parse_block(vec![Keyword::End])?,
            None => return Err(ParseError::UnexpectedEof),
        };

        self.expect(&Token::Keyword(Keyword::End))?;

        Ok(AstNode::AnonFunctionDef {
            params,
            body: Box::new(body),
        })
    }

    fn parse_function_params(&mut self) -> Result<Vec<String>, ParseError> {
        let mut params = Vec::new();
        if self.consume(&Token::LeftParen)? {
            while let Some(Token::Identifier(param)) = self.peek()? {
                params.push(param.clone());
                self.next_token()?;
                if !self.consume(&Token::Comma)? {
                    break;
                }
            }
            self.consume(&Token::RightParen)?;
        }
        Ok(params)
    }

    fn parse_identifier_statement(&mut self) -> Result<AstNode, ParseError> {
        let (name, is_symbol, ident_span) = match self.next_token()? {
            Some(TokenWithSpan {
                token: Token::Identifier(name),
                span,
            }) => (name, false, span),
            Some(TokenWithSpan {
                token: Token::Symbol(name),
                span,
            }) => (name, true, span),
            _ => unreachable!("Checked in parse_statement"),
        };

        match self.peek()? {
            Some(Token::Dot) => {
                let base_node = if is_symbol {
                    AstNode::Symbol(name)
                } else {
                    AstNode::Identifier(name)
                };
                self.parse_property_access(base_node, ident_span)
            }
            Some(Token::LeftBracket) => {
                let indexes = self.parse_subscript()?;
                match self.peek()? {
                    Some(Token::Operator(Operator::Equal)) => {
                        self.next_token()?;
                        let value = self.parse_expression()?;
                        Ok(AstNode::SubscriptAssignment {
                            name,
                            indexes,
                            value: Box::new(value),
                        })
                    }
                    Some(_) => Ok(AstNode::Subscript { name, indexes }),
                    None => Err(ParseError::UnexpectedEof),
                }
            }
            Some(Token::Operator(Operator::Equal)) => {
                let equal_token = self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                if equal_token.span.start_line != ident_span.start_line {
                    return Err(ParseError::UnexpectedToken {
                        token: equal_token.token,
                        span: equal_token.span,
                    });
                }
                let next_token_span = match self.peek_with_span()? {
                    Some(TokenWithSpan { span, .. }) => *span,
                    None => return Err(ParseError::UnexpectedEof),
                };
                if next_token_span.start_line != ident_span.start_line {
                    return Err(ParseError::AssignmentNewline {
                        span: next_token_span,
                    });
                }
                let value = self.parse_expression()?;
                Ok(AstNode::Assignment {
                    name,
                    value: Box::new(value),
                })
            }
            Some(Token::LeftParen) => {
                self.next_token()?;
                let args = self.parse_args()?;

                let node = AstNode::FunctionCall { name, args };

                if let Some(&Token::Dot) = self.peek()? {
                    self.next_token()?;
                    return self.parse_property_access(node, ident_span);
                }

                Ok(node)
            }
            _ => Ok(if is_symbol {
                AstNode::Symbol(name)
            } else {
                AstNode::Identifier(name)
            }),
        }
    }

    fn parse_property_access(
        &mut self,
        base_node: AstNode,
        base_span: Span,
    ) -> Result<AstNode, ParseError> {
        self.consume(&Token::Dot)?;
        let property = self.parse_expression()?;

        if let Some(Token::Operator(Operator::Equal)) = self.peek()? {
            let equal_token = self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
            if equal_token.span.start_line != base_span.start_line {
                return Err(ParseError::UnexpectedToken {
                    token: equal_token.token,
                    span: equal_token.span,
                });
            }
            let next_token_span = match self.peek_with_span()? {
                Some(TokenWithSpan { span, .. }) => *span,
                None => return Err(ParseError::UnexpectedEof),
            };
            if next_token_span.start_line != base_span.start_line {
                return Err(ParseError::AssignmentNewline {
                    span: next_token_span,
                });
            }
            let value = self.parse_expression()?;
            return Ok(AstNode::PropertyAssignment {
                base: Box::new(base_node),
                property: Box::new(property),
                value: Box::new(value),
            });
        }

        Ok(AstNode::PropertyAccess {
            base: Box::new(base_node),
            property: Box::new(property),
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, ParseError> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, min_precedence: u8) -> Result<AstNode, ParseError> {
        let mut left = self.parse_unary()?;

        while let Some(op) = self.peek_operator()? {
            let precedence = Self::op_precedence(&op);
            if precedence < min_precedence {
                break;
            }
            self.next_token()?;
            let right = self.parse_binary_expression(precedence + 1)?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<AstNode, ParseError> {
        if let Some(Token::Operator(Operator::Not)) = self.peek()? {
            self.next_token()?;
            let expr = self.parse_unary()?;
            Ok(AstNode::UnaryExpr {
                op: Operator::Not,
                expr: Box::new(expr),
            })
        } else {
            self.parse_primary()
        }
    }

    fn parse_list(&mut self) -> Result<AstNode, ParseError> {
        let mut elements = Vec::new();
        if self.consume(&Token::RightBracket)? {
            return Ok(AstNode::List(elements));
        }

        loop {
            let item = self.parse_expression()?;
            elements.push(item);
            match self.peek()? {
                Some(Token::Comma) => {
                    self.next_token()?;
                    if self.consume(&Token::RightBracket)? {
                        break;
                    }
                }
                Some(Token::RightBracket) => {
                    self.next_token()?;
                    break;
                }
                Some(_) => {
                    let TokenWithSpan { token, span } =
                        self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                    return Err(ParseError::UnexpectedToken { token, span });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        Ok(AstNode::List(elements))
    }

    fn parse_map(&mut self) -> Result<AstNode, ParseError> {
        self.consume(&Token::BraceOpen)?;
        let mut pairs = Vec::new();

        if self.consume(&Token::BraceClose)? {
            return Ok(AstNode::Map(pairs));
        }

        loop {
            let key = match self.next_token()? {
                Some(TokenWithSpan {
                    token: Token::StringLiteral(s),
                    ..
                }) => AstNode::StringLiteral(s),
                Some(TokenWithSpan {
                    token: Token::Symbol(s),
                    ..
                }) => AstNode::Symbol(s),
                Some(TokenWithSpan { token, span }) => {
                    return Err(ParseError::UnexpectedToken { token, span })
                }
                None => return Err(ParseError::UnexpectedEof),
            };
            self.expect(&Token::Operator(Operator::Equal))?;

            let value = self.parse_expression()?;
            pairs.push((key, value));

            match self.peek()? {
                Some(Token::Comma) => {
                    self.next_token()?;
                    if self.consume(&Token::BraceClose)? {
                        break;
                    }
                }
                Some(Token::BraceClose) => {
                    self.next_token()?;
                    break;
                }
                Some(_) => {
                    let TokenWithSpan { token, span } =
                        self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                    return Err(ParseError::UnexpectedToken { token, span });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        Ok(AstNode::Map(pairs))
    }

    fn parse_subscript(&mut self) -> Result<Vec<AstNode>, ParseError> {
        let mut indexes = Vec::new();
        while self.consume(&Token::LeftBracket)? {
            let index = self.parse_expression()?;
            indexes.push(index);
            match self.peek()? {
                Some(Token::RightBracket) => {
                    self.next_token()?;
                }
                Some(_) => {
                    let TokenWithSpan { token, span } =
                        self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                    return Err(ParseError::UnexpectedToken { token, span });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        Ok(indexes)
    }

    fn parse_primary(&mut self) -> Result<AstNode, ParseError> {
        match self.next_token()? {
            Some(TokenWithSpan {
                token: Token::Keyword(Keyword::Fn),
                ..
            }) => self.parse_function_definition(),
            Some(TokenWithSpan {
                token: Token::Keyword(Keyword::Nil),
                ..
            }) => Ok(AstNode::Nil),
            Some(TokenWithSpan {
                token: Token::Integer(n),
                ..
            }) => Ok(AstNode::Integer(n)),
            Some(TokenWithSpan {
                token: Token::Float(n),
                ..
            }) => Ok(AstNode::Float(n)),
            Some(TokenWithSpan {
                token: Token::StringLiteral(s),
                ..
            }) => Ok(AstNode::StringLiteral(s)),
            Some(TokenWithSpan {
                token: Token::Boolean(b),
                ..
            }) => Ok(AstNode::Boolean(b)),
            Some(TokenWithSpan {
                token: Token::Symbol(s),
                ..
            }) => Ok(AstNode::Symbol(s)),
            Some(TokenWithSpan {
                token: Token::InterpolatedString { parts, vars },
                ..
            }) => {
                let ast_vars = vars
                    .into_iter()
                    .map(|TokenWithSpan { token, span }| match token {
                        Token::Identifier(name) => Ok(AstNode::Identifier(name)),
                        token => Err(ParseError::UnexpectedToken { token, span }),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(AstNode::InterpolatedString {
                    parts,
                    vars: ast_vars,
                })
            }
            Some(TokenWithSpan {
                token: Token::Identifier(id),
                span,
            }) => match self.peek()? {
                Some(Token::LeftParen) => {
                    self.next_token()?;
                    let args = self.parse_args()?;
                    Ok(AstNode::FunctionCall { name: id, args })
                }
                Some(Token::LeftBracket) => {
                    let indexes = self.parse_subscript()?;
                    Ok(AstNode::Subscript { name: id, indexes })
                }
                Some(Token::Dot) => self.parse_property_access(AstNode::Identifier(id), span),
                _ => Ok(AstNode::Identifier(id)),
            },
            Some(TokenWithSpan {
                token: Token::LeftParen,
                ..
            }) => {
                let expr = self.parse_expression()?;
                match self.next_token()? {
                    Some(TokenWithSpan {
                        token: Token::RightParen,
                        ..
                    }) => Ok(expr),
                    Some(TokenWithSpan { token, span }) => {
                        Err(ParseError::UnexpectedToken { token, span })
                    }
                    None => Err(ParseError::UnexpectedEof),
                }
            }
            Some(TokenWithSpan {
                token: Token::LeftBracket,
                ..
            }) => self.parse_list(),
            Some(TokenWithSpan {
                token: Token::BraceOpen,
                ..
            }) => self.parse_map(),
            Some(TokenWithSpan { token, span }) => Err(ParseError::UnexpectedToken { token, span }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn op_precedence(op: &Operator) -> u8 {
        match op {
            Operator::Or => 5,
            Operator::And => 10,
            Operator::DoubleEqual
            | Operator::NotEqual
            | Operator::Greater
            | Operator::GreaterEqual
            | Operator::Less
            | Operator::LessEqual => 20,
            Operator::Plus | Operator::Minus => 30,
            Operator::Asterisk | Operator::Slash | Operator::Percent => 40,
            Operator::Not => 50,
            _ => 0,
        }
    }

    fn peek_operator(&mut self) -> Result<Option<Operator>, ParseError> {
        match self.peek()? {
            Some(Token::Operator(op)) => match op {
                Operator::Plus
                | Operator::Minus
                | Operator::Asterisk
                | Operator::Slash
                | Operator::Percent
                | Operator::DoubleEqual
                | Operator::NotEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::Less
                | Operator::LessEqual
                | Operator::And
                | Operator::Or => Ok(Some(op.clone())),
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<AstNode>, ParseError> {
        let mut args = Vec::new();
        if self.consume(&Token::RightParen)? {
            return Ok(args);
        }

        loop {
            if self.consume(&Token::RightParen)? {
                return Ok(args);
            }
            let arg = match self.peek()? {
                Some(Token::Keyword(Keyword::Fn)) => self.parse_function_definition()?,
                _ => self.parse_expression()?,
            };
            args.push(arg);
            match self.peek()? {
                Some(Token::Comma) => {
                    self.next_token()?;
                }
                Some(Token::RightParen) => {
                    self.next_token()?;
                    return Ok(args);
                }
                Some(_) => {
                    let TokenWithSpan { token, span } =
                        self.next_token()?.ok_or(ParseError::UnexpectedEof)?;
                    return Err(ParseError::UnexpectedToken { token, span });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }
    }

    fn peek(&mut self) -> Result<Option<&Token>, ParseError> {
        match self.tokens.peek() {
            Some(Ok(TokenWithSpan { token, .. })) => Ok(Some(token)),
            Some(Err(err)) => Err(ParseError::TokenizationError(err.clone())),
            None => Ok(None),
        }
    }

    fn peek_with_span(&mut self) -> Result<Option<&TokenWithSpan>, ParseError> {
        match self.tokens.peek() {
            Some(Ok(token_with_span)) => Ok(Some(token_with_span)),
            Some(Err(err)) => Err(ParseError::TokenizationError(err.clone())),
            None => Ok(None),
        }
    }

    fn next_token(&mut self) -> Result<Option<TokenWithSpan>, ParseError> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(err)) => Err(ParseError::TokenizationError(err)),
            None => Ok(None),
        }
    }

    fn consume(&mut self, expected: &Token) -> Result<bool, ParseError> {
        match self.peek()? {
            Some(token) if token == expected => {
                self.next_token()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<TokenWithSpan, ParseError> {
        match self.next_token()? {
            Some(token_with_span) if &token_with_span.token == expected => Ok(token_with_span),
            Some(TokenWithSpan { token, span }) => Err(ParseError::UnexpectedToken { token, span }),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}
