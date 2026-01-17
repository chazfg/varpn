use crate::err::Trace;
use std::{cmp::Ordering, vec::Vec};

use crate::err::{VarpnErr, VarpnResult};
mod err;

const T1: &str = "a - b - c";

fn main() -> VarpnResult<()> {
    let tokens = parse_tokens(T1)?;
    let stack = rpn_stack(tokens);
    println!("{stack:?}");
    Ok(())
}
impl From<ControlOp> for OpStackValue {
    fn from(value: ControlOp) -> Self {
        OpStackValue::Control(value)
    }
}
impl From<Operation> for OpStackValue {
    fn from(value: Operation) -> Self {
        OpStackValue::Op(value)
    }
}
//
impl From<&Operation> for OutputItem {
    fn from(value: &Operation) -> Self {
        Self::Op(*value)
    }
}
impl From<Operation> for OutputItem {
    fn from(value: Operation) -> Self {
        Self::Op(value)
    }
}

#[derive(Default, Debug)]
struct OutputStack(Vec<OpStackValue>);
impl OutputStack {
    fn push_op(&mut self, new_op: Operation) -> VarpnResult<Vec<OutputItem>> {
        let mut out_vec = Vec::new();
        let new_op_prec = new_op.precedence();
        if self.0.is_empty() {
            self.0.push(OpStackValue::Op(new_op));
            return Ok(out_vec);
        } else {
            while !self.0.is_empty() {
                match &self.0[self.0.len() - 1] {
                    OpStackValue::Op(operation) => {
                        let old_op_prec = operation.precedence();
                        match new_op_prec.cmp(&old_op_prec) {
                            Ordering::Less => {
                                out_vec.push(
                                    self.0
                                        .pop()
                                        .unwrap()
                                        .op()
                                        .trace(format!("{} op conversion", line!()))?
                                        .into(),
                                );
                                if self.0.is_empty() {
                                    self.0.push(new_op.into());
                                    break;
                                }
                            }
                            Ordering::Equal => {
                                out_vec.push(operation.into());
                                self.0.pop().unwrap();
                                self.0.push(OpStackValue::Op(new_op));
                                break;
                            }
                            Ordering::Greater => {
                                self.0.push(new_op.into());
                                break;
                            }
                        }
                    }

                    OpStackValue::Control(..) => {
                        self.0.push(OpStackValue::Op(new_op));
                        break;
                    }
                }
            }
        }

        Ok(out_vec)
    }

    fn push_control(&mut self, control_op: ControlOp) -> VarpnResult<Vec<OutputItem>> {
        let mut out_vec = vec![];
        match control_op {
            ControlOp::LParen => {
                self.0.push(control_op.into());
                Ok(out_vec)
            }
            ControlOp::RParen => {
                while let Some(op) = self.0.pop()
                    && op != OpStackValue::Control(ControlOp::LParen)
                {
                    out_vec.push(op.op().trace(format!("{} failed op conv", line!()))?.into());
                }
                Ok(out_vec)
            }
        }
    }
}

fn rpn_stack(mut tokens: Vec<Token>) -> VarpnResult<Vec<OutputItem>> {
    let mut op_stack = OutputStack::default();
    // let mut output_val_stack: Vec<OutputValue> = Vec::new();
    let mut output: Vec<OutputItem> = Vec::new();
    tokens.drain(..).try_for_each(|t| -> VarpnResult<()> {
        match t {
            Token::Variable(s) => output.push(OutputItem::Value(OutputValue::Variable(s))),
            Token::Literal(s) => output.push(OutputItem::Value(OutputValue::Literal(s))),
            Token::Op(operation) => {
                output.extend(
                    op_stack
                        .push_op(operation)
                        .trace(format!("{} extend op", line!()))?,
                );
            }
            Token::Control(control_op) => {
                output.extend(
                    op_stack
                        .push_control(control_op)
                        .trace(format!("{} rpn stack", line!()))?,
                );
            }
        }
        Ok(())
    })?;
    while let Some(v) = op_stack.0.pop() {
        if let OpStackValue::Op(x) = v {
            output.push(OutputItem::Op(x));
        }
    }
    Ok(output)
}
fn parse_tokens(s: &str) -> VarpnResult<Vec<Token>> {
    let mut buf = String::new();
    let mut tokens = Vec::new();
    // let mut operator_stack = Vec::new();
    for c in s.chars() {
        match c {
            '+' | '-' | '/' | '*' => {
                if !buf.is_empty() {
                    tokens.push(
                        Token::parse_text(std::mem::take(&mut buf))
                            .trace(format!("{}: parse tokens", line!()))?,
                    );
                }
                tokens.push(Token::Op(Operation::from_char(c)));
            }
            '(' | ')' => {
                if !buf.is_empty() {
                    tokens.push(
                        Token::parse_text(std::mem::take(&mut buf))
                            .trace(format!("{}: parse tokens", line!()))?,
                    );
                }
                tokens.push(Token::Control(ControlOp::from_char(c)));
            }
            ' ' => (),
            _ => {
                buf.push(c);
            }
        }
    }
    if !buf.is_empty() {
        tokens.push(
            Token::parse_text(std::mem::take(&mut buf))
                .trace(format!("{}: parse tokens", line!()))?,
        );
    }
    Ok(tokens)
}

#[derive(Debug)]
enum OutputItem {
    Value(OutputValue),
    Op(Operation),
}

#[derive(Debug)]
enum OutputValue {
    Variable(String),
    Literal(String),
}

#[derive(Debug, PartialEq, Eq)]
enum OpStackValue {
    Op(Operation),
    Control(ControlOp),
}
impl OpStackValue {
    fn op(&self) -> VarpnResult<Operation> {
        match self {
            OpStackValue::Op(operation) => Ok(*operation),
            OpStackValue::Control(..) => {
                Err(VarpnErr::Source(format!("can't make {self:?} an op")))
            }
        }
    }
}

#[derive(Debug)]
enum Token {
    Variable(String),
    Literal(String),
    Op(Operation),
    Control(ControlOp),
}
impl Token {
    fn parse_char(c: char) -> Token {
        Self::Op(Operation::from_char(c))
    }

    fn parse_text(buf: String) -> VarpnResult<Token> {
        assert!(check_valid(&buf), "something wrong {buf}");
        let has_nums = buf.contains(char::is_numeric);
        let has_text = buf.contains(char::is_alphabetic);
        match (has_text, has_nums) {
            (true, true) | (true, false) => Ok(Self::Variable(buf)),
            (false, true) => Ok(Self::Literal(buf)),
            _ => Err(VarpnErr::Source(format!("Parse text failed for {buf}"))),
        }
    }
}

fn check_valid(buf: &str) -> bool {
    buf.as_bytes()
        .iter()
        .all(|c| matches!(c, 48..=52 | 65..=90 | 95 | 46 | 97..=122))
}

#[derive(Debug, PartialEq, Eq)]
enum ControlOp {
    LParen,
    RParen,
}
impl ControlOp {
    fn from_char(c: char) -> ControlOp {
        match c {
            '(' => Self::LParen,
            ')' => Self::RParen,
            _ => panic!(),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug)]
enum Assoc {
    Left,
    Right,
}
impl Operation {
    fn precedence(&self) -> i32 {
        match self {
            Operation::Add | Operation::Subtract => 2,
            Operation::Multiply | Operation::Divide => 3,
        }
    }
    fn associativity(&self) -> Assoc {
        match self {
            Operation::Add | Operation::Subtract | Operation::Multiply | Operation::Divide => {
                Assoc::Left
            }
        }
    }
    fn from_char(c: char) -> Operation {
        match c {
            '+' => Self::Add,
            '-' => Self::Subtract,
            '*' => Self::Multiply,
            '/' => Self::Divide,
            _ => panic!("{c} is not valid operator"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Operation, OutputItem, OutputValue, Token, parse_tokens, rpn_stack};

    fn token_sig(tokens: Vec<Token>) -> Vec<String> {
        tokens
            .into_iter()
            .map(|t| match t {
                Token::Variable(s) => format!("var:{s}"),
                Token::Literal(s) => format!("lit:{s}"),
                Token::Op(op) => format!(
                    "op:{}",
                    match op {
                        Operation::Add => "+",
                        Operation::Subtract => "-",
                        Operation::Multiply => "*",
                        Operation::Divide => "/",
                    }
                ),
                Token::Control(_) => "ctrl".to_string(),
            })
            .collect()
    }

    fn rpn_sig(items: Vec<OutputItem>) -> Vec<String> {
        items
            .into_iter()
            .map(|i| match i {
                OutputItem::Value(OutputValue::Variable(s)) => format!("var:{s}"),
                OutputItem::Value(OutputValue::Literal(s)) => format!("lit:{s}"),
                OutputItem::Op(op) => format!(
                    "op:{}",
                    match op {
                        Operation::Add => "+",
                        Operation::Subtract => "-",
                        Operation::Multiply => "*",
                        Operation::Divide => "/",
                    }
                ),
            })
            .collect()
    }

    #[test]
    fn parse_tokens_matrix() {
        let cases = [
            ("1 + 1", vec!["lit:1", "op:+", "lit:1"]),
            ("a + b", vec!["var:a", "op:+", "var:b"]),
            ("a*b+3", vec!["var:a", "op:*", "var:b", "op:+", "lit:3"]),
            ("X1 + 2", vec!["var:X1", "op:+", "lit:2"]),
            ("_foo - bar", vec!["var:_foo", "op:-", "var:bar"]),
            ("3.14/2", vec!["lit:3.14", "op:/", "lit:2"]),
            ("a1 + b2", vec!["var:a1", "op:+", "var:b2"]),
            (
                "(a + b) * c",
                vec!["ctrl", "var:a", "op:+", "var:b", "ctrl", "op:*", "var:c"],
            ),
        ];

        for (input, expected) in cases {
            let actual = token_sig(parse_tokens(input).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }

    #[test]
    fn rpn_basic_matrix() {
        let cases = [
            ("1 + 1", vec!["lit:1", "lit:1", "op:+"]),
            ("a + b", vec!["var:a", "var:b", "op:+"]),
            ("a * b + 3", vec!["var:a", "var:b", "op:*", "lit:3", "op:+"]),
            ("a + b * 3", vec!["var:a", "var:b", "lit:3", "op:*", "op:+"]),
            ("a - b - c", vec!["var:a", "var:b", "op:-", "var:c", "op:-"]),
            ("a / b * c", vec!["var:a", "var:b", "op:/", "var:c", "op:*"]),
        ];

        for (input, expected) in cases {
            let actual = rpn_sig(rpn_stack(parse_tokens(input).unwrap()).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }

    #[test]
    #[should_panic]
    fn parse_tokens_rejects_invalid_chars() {
        parse_tokens("1$2").unwrap();
    }

    #[test]
    fn rpn_paren_matrix() {
        let cases = [
            ("(1 + 2)", vec!["lit:1", "lit:2", "op:+"]),
            (
                "a * (b + c)",
                vec!["var:a", "var:b", "var:c", "op:+", "op:*"],
            ),
            (
                "(a + b) * (c - d)",
                vec!["var:a", "var:b", "op:+", "var:c", "var:d", "op:-", "op:*"],
            ),
            (
                "a * (b + c * (d - e))",
                vec![
                    "var:a", "var:b", "var:c", "var:d", "var:e", "op:-", "op:*", "op:+", "op:*",
                ],
            ),
        ];

        for (input, expected) in cases {
            let actual = rpn_sig(rpn_stack(parse_tokens(input).unwrap()).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }
}
