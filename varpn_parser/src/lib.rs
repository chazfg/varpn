use std::{cmp::Ordering, vec::Vec};

use crate::err::{Trace, VarpnErr, VarpnResult};
pub mod err;
impl From<Control> for StackEntry {
    fn from(value: Control) -> Self {
        StackEntry::Control(value)
    }
}
impl From<Op> for StackEntry {
    fn from(value: Op) -> Self {
        StackEntry::Op(value)
    }
}
//
impl From<&Op> for RpnItem {
    fn from(value: &Op) -> Self {
        Self::Op(*value)
    }
}
impl From<Op> for RpnItem {
    fn from(value: Op) -> Self {
        Self::Op(value)
    }
}

#[derive(Default, Debug)]
struct OutputStack(Vec<StackEntry>);
impl OutputStack {
    fn _right_assoc_push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        let mut out_vec = Vec::new();
        let new_op_prec = new_op.precedence();
        while !self.0.is_empty() {
            match &self.0[self.0.len() - 1] {
                StackEntry::Func(..) => {
                    self.0.push(new_op.into());
                    break;
                }
                StackEntry::Op(operation) => {
                    let old_op_prec = operation.precedence();
                    match new_op_prec.cmp(&old_op_prec) {
                        Ordering::Less => {
                            out_vec.push(
                                self.0
                                    .pop()
                                    .unwrap()
                                    .op()
                                    .trace(line!(), "op conversion")?
                                    .into(),
                            );
                            if self.0.is_empty() {
                                self.0.push(new_op.into());
                                break;
                            }
                        }
                        Ordering::Equal => {
                            self.0.push(StackEntry::Op(new_op));
                            break;
                        }
                        Ordering::Greater => {
                            self.0.push(new_op.into());
                            break;
                        }
                    }
                }

                StackEntry::Control(..) => {
                    self.0.push(StackEntry::Op(new_op));
                    break;
                }
            }
        }
        Ok(out_vec)
    }
    fn _left_assoc_push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        let mut out_vec = Vec::new();
        let new_op_prec = new_op.precedence();
        while !self.0.is_empty() {
            match &self.0[self.0.len() - 1] {
                StackEntry::Func(..) => {
                    self.0.push(new_op.into());
                    break;
                }
                StackEntry::Op(operation) => {
                    let old_op_prec = operation.precedence();
                    match new_op_prec.cmp(&old_op_prec) {
                        Ordering::Less => {
                            out_vec.push(
                                self.0
                                    .pop()
                                    .unwrap()
                                    .op()
                                    .trace(line!(), "op conversion")?
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
                            self.0.push(StackEntry::Op(new_op));
                            break;
                        }
                        Ordering::Greater => {
                            self.0.push(new_op.into());
                            break;
                        }
                    }
                }

                StackEntry::Control(..) => {
                    self.0.push(StackEntry::Op(new_op));
                    break;
                }
            }
        }
        Ok(out_vec)
    }
    pub fn push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        if self.0.is_empty() {
            self.0.push(StackEntry::Op(new_op));
            Ok(vec![])
        } else {
            match new_op.associativity() {
                Assoc::Left => self._left_assoc_push_op(new_op),
                Assoc::Right => self._right_assoc_push_op(new_op),
            }
        }
    }

    fn push_control(&mut self, control_op: Control) -> VarpnResult<Vec<RpnItem>> {
        let mut out_vec = vec![];
        match control_op {
            Control::LParen => {
                self.0.push(control_op.into());
                Ok(out_vec)
            }
            Control::Comma => {
                while let Some(top) = self.0.last() {
                    match top {
                        StackEntry::Op(op) => {
                            out_vec.push((*op).into());
                            self.0.pop();
                        }
                        StackEntry::Func(..)
                        | StackEntry::Control(Control::LParen)
                        | StackEntry::Control(Control::Comma) => break,
                        StackEntry::Control(Control::RParen) => unreachable!(),
                    }
                }
                self.0.push(StackEntry::Control(Control::Comma));
                Ok(out_vec)
            }
            Control::RParen => {
                let mut comma_count = 0;
                while let Some(popped_op) = self.0.pop()
                // && op != StackEntry::Control(Paren::LParen)
                {
                    match popped_op {
                        StackEntry::Func(func_name) => {
                            out_vec.push(RpnItem::Func(func_name, comma_count + 1));
                            break;
                        }
                        StackEntry::Op(op) => out_vec.push(op.into()),
                        StackEntry::Control(Control::Comma) => comma_count += 1,
                        StackEntry::Control(Control::LParen) => break,
                        StackEntry::Control(Control::RParen) => unreachable!(),
                    }
                }
                Ok(out_vec)
            }
        }
    }

    fn push_func(&mut self, func_name: String) -> VarpnResult<()> {
        self.0.push(StackEntry::Func(func_name));
        Ok(())
    }
}

pub fn rpn_stack(mut tokens: Vec<Lexeme>) -> VarpnResult<Vec<RpnItem>> {
    let mut op_stack = OutputStack::default();
    // let mut output_val_stack: Vec<OutputValue> = Vec::new();
    let mut output: Vec<RpnItem> = Vec::new();
    tokens.drain(..).try_for_each(|t| -> VarpnResult<()> {
        match t {
            Lexeme::Ident(s) => output.push(RpnItem::Value(Atom::Variable(s))),
            Lexeme::Number(s) => output.push(RpnItem::Value(Atom::Literal(s))),
            Lexeme::LastValRef => output.push(RpnItem::Value(Atom::LastValRef)),
            Lexeme::Op(operation) => {
                output.extend(op_stack.push_op(operation).trace(line!(), "extend op")?);
            }
            Lexeme::Control(control_op) => {
                output.extend(
                    op_stack
                        .push_control(control_op)
                        .trace(line!(), "rpn stack")?,
                );
            }
            Lexeme::Function(func_name) => {
                op_stack.push_func(func_name).trace(line!(), "function")?
            }
        }
        Ok(())
    })?;
    while let Some(v) = op_stack.0.pop() {
        if let StackEntry::Op(x) = v {
            output.push(RpnItem::Op(x));
        }
    }
    Ok(output)
}
pub fn parse_tokens(s: &str) -> VarpnResult<Vec<Lexeme>> {
    let mut buf = String::new();
    let mut tokens = Vec::new();
    for c in s.chars() {
        match c {
            '+' | '-' | '/' | '*' | '%' | '^' => {
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_text(std::mem::take(&mut buf))
                            .trace(line!(), "parse tokens")?,
                    );
                }
                tokens.push(Lexeme::Op(Op::from_char(c)));
            }
            '$' => {
                if buf.is_empty() {
                    tokens.push(Lexeme::LastValRef);
                } else {
                    return Err(VarpnErr::new(
                        line!(),
                        "LastValRef ($) not surrounded by whitespace",
                    ));
                }
            }
            '(' => {
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_func(std::mem::take(&mut buf))
                            .trace(line!(), "parse tokens")?,
                    );
                } else {
                    tokens.push(Lexeme::Control(Control::from_char(c)));
                }
            }
            ')' => {
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_text(std::mem::take(&mut buf))
                            .trace(line!(), "parse tokens")?,
                    );
                }
                tokens.push(Lexeme::Control(Control::from_char(c)));
            }
            ',' => {
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_text(std::mem::take(&mut buf))
                            .trace(line!(), "parse tokens")?,
                    );
                }
                tokens.push(Lexeme::Control(Control::from_char(c)));
            }
            ' ' => (),
            _ => {
                buf.push(c);
            }
        }
    }
    if !buf.is_empty() {
        tokens.push(Lexeme::parse_text(std::mem::take(&mut buf)).trace(line!(), "parse tokens")?);
    }
    Ok(tokens)
}

type Arity = u32;
#[derive(Debug)]
pub enum RpnItem {
    Value(Atom),
    Func(String, Arity),
    Op(Op),
}

#[derive(Debug)]
pub enum Atom {
    Variable(String),
    Literal(String),
    LastValRef,
}

#[derive(Debug, PartialEq, Eq)]
enum StackEntry {
    Func(String),
    Op(Op),
    Control(Control),
}
impl StackEntry {
    fn op(&self) -> VarpnResult<Op> {
        match self {
            StackEntry::Op(operation) => Ok(*operation),
            StackEntry::Control(..) | StackEntry::Func(..) => {
                Err(VarpnErr::new(line!(), format!("can't make {self:?} an op")))
            }
        }
    }
}

#[derive(Debug)]
pub enum Lexeme {
    Ident(String),
    Number(String),
    Function(String),
    Op(Op),
    Control(Control),
    LastValRef,
}
impl Lexeme {
    fn parse_text(buf: String) -> VarpnResult<Lexeme> {
        if !check_valid(&buf) {
            return Err(VarpnErr::new(
                line!(),
                format!("{} Invalid token in buf: '{buf}'", line!()),
            ));
        }
        let has_nums = buf.contains(char::is_numeric);
        let has_text = buf.contains(char::is_alphabetic);
        match (has_text, has_nums) {
            (true, true) | (true, false) => Ok(Self::Ident(buf)),
            (false, true) => Ok(Self::Number(buf)),
            _ => Err(VarpnErr::new(
                line!(),
                format!("Parse text failed for {buf}"),
            )),
        }
    }

    fn parse_func(buf: String) -> VarpnResult<Lexeme> {
        if !check_valid(&buf) {
            return Err(VarpnErr::new(
                line!(),
                format!("{} Invalid token in buf: '{buf}'", line!()),
            ));
        }
        Ok(Lexeme::Function(buf))
    }
}

fn check_valid(buf: &str) -> bool {
    buf.as_bytes()
        .iter()
        .all(|c| matches!(c, 48..=57 | 65..=90 | 94..=95 | 46|37 | 97..=122 | 44))
}

#[derive(Debug, PartialEq, Eq)]
pub enum Control {
    Comma,
    LParen,
    RParen,
}
impl Control {
    fn from_char(c: char) -> Control {
        match c {
            '(' => Self::LParen,
            ')' => Self::RParen,
            ',' => Self::Comma,
            _ => panic!(),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
}
#[derive(Debug)]
pub enum Assoc {
    Left,
    Right,
}
impl Op {
    fn precedence(&self) -> i32 {
        match self {
            Op::Add | Op::Subtract => 2,
            Op::Modulo | Op::Multiply | Op::Divide => 3,
            Op::Power => 4,
        }
    }
    fn associativity(&self) -> Assoc {
        match self {
            Op::Power => Assoc::Right,
            Op::Modulo | Op::Add | Op::Subtract | Op::Multiply | Op::Divide => Assoc::Left,
        }
    }
    fn from_char(c: char) -> Op {
        match c {
            '+' => Self::Add,
            '-' => Self::Subtract,
            '*' => Self::Multiply,
            '/' => Self::Divide,
            '%' => Self::Modulo,
            '^' => Self::Power,
            _ => panic!("{c} is not valid operator"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Atom, Control, Lexeme, Op, RpnItem, parse_tokens, rpn_stack};

    fn token_sig(tokens: Vec<Lexeme>) -> Vec<String> {
        tokens
            .into_iter()
            .map(|t| match t {
                Lexeme::Ident(s) => format!("var:{s}"),
                Lexeme::Number(s) => format!("lit:{s}"),
                Lexeme::Op(op) => format!(
                    "op:{}",
                    match op {
                        Op::Add => "+",
                        Op::Subtract => "-",
                        Op::Multiply => "*",
                        Op::Divide => "/",
                        Op::Modulo => "%",
                        Op::Power => "^",
                    }
                ),
                Lexeme::Control(Control::LParen) => "lparen".to_string(),
                Lexeme::Control(Control::RParen) => "rparen".to_string(),
                Lexeme::Control(Control::Comma) => "comma".to_string(),
                Lexeme::LastValRef => "$".to_string(),
                Lexeme::Function(s) => format!("func:{s}"),
            })
            .collect()
    }

    fn rpn_sig(items: Vec<RpnItem>) -> Vec<String> {
        items
            .into_iter()
            .map(|i| match i {
                RpnItem::Value(Atom::Variable(s)) => format!("var:{s}"),
                RpnItem::Value(Atom::Literal(s)) => format!("lit:{s}"),
                RpnItem::Value(Atom::LastValRef) => "$".to_string(),
                RpnItem::Op(op) => format!(
                    "op:{}",
                    match op {
                        Op::Add => "+",
                        Op::Subtract => "-",
                        Op::Multiply => "*",
                        Op::Divide => "/",
                        Op::Modulo => "%",
                        Op::Power => "^",
                    }
                ),
                RpnItem::Func(s, arity) => format!("func:{s}/{arity}"),
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
            ("10 % 3", vec!["lit:10", "op:%", "lit:3"]),
            ("2 ^ 8", vec!["lit:2", "op:^", "lit:8"]),
            ("$ + 1", vec!["$", "op:+", "lit:1"]),
            ("$ + a", vec!["$", "op:+", "var:a"]),
            (
                "(a + b) * c",
                vec![
                    "lparen", "var:a", "op:+", "var:b", "rparen", "op:*", "var:c",
                ],
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
            ("a % b + c", vec!["var:a", "var:b", "op:%", "var:c", "op:+"]),
            ("a ^ b ^ c", vec!["var:a", "var:b", "var:c", "op:^", "op:^"]),
            ("a ^ b * c", vec!["var:a", "var:b", "op:^", "var:c", "op:*"]),
            ("a * b ^ c", vec!["var:a", "var:b", "var:c", "op:^", "op:*"]),
            ("$ + 1", vec!["$", "lit:1", "op:+"]),
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
    #[should_panic]
    fn parse_tokens_rejects_adjacent_last_val_ref() {
        parse_tokens("a$").unwrap();
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
            (
                "a % (b + c)",
                vec!["var:a", "var:b", "var:c", "op:+", "op:%"],
            ),
            (
                "a ^ (b + c)",
                vec!["var:a", "var:b", "var:c", "op:+", "op:^"],
            ),
        ];

        for (input, expected) in cases {
            let actual = rpn_sig(rpn_stack(parse_tokens(input).unwrap()).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }

    #[test]
    fn parse_tokens_function_matrix() {
        let cases = [
            ("foo(a)", vec!["func:foo", "var:a", "rparen"]),
            (
                "sum(1,2)",
                vec!["func:sum", "lit:1", "comma", "lit:2", "rparen"],
            ),
            (
                "avg(1.5,2.5)",
                vec!["func:avg", "lit:1.5", "comma", "lit:2.5", "rparen"],
            ),
            (
                "max(a,b,c)",
                vec![
                    "func:max", "var:a", "comma", "var:b", "comma", "var:c", "rparen",
                ],
            ),
        ];

        for (input, expected) in cases {
            let actual = token_sig(parse_tokens(input).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }

    #[test]
    fn rpn_function_matrix() {
        let cases = [
            ("foo(a)", vec!["var:a", "func:foo/1"]),
            ("sum(1,2)", vec!["lit:1", "lit:2", "func:sum/2"]),
            ("sum(a+b)", vec!["var:a", "var:b", "op:+", "func:sum/1"]),
            (
                "sum(a*b+3)",
                vec!["var:a", "var:b", "op:*", "lit:3", "op:+", "func:sum/1"],
            ),
            (
                "foo(a + bar(b))",
                vec!["var:a", "var:b", "func:bar/1", "op:+", "func:foo/1"],
            ),
            (
                "sum(a+b,c)",
                vec!["var:a", "var:b", "op:+", "var:c", "func:sum/2"],
            ),
        ];

        for (input, expected) in cases {
            let actual = rpn_sig(rpn_stack(parse_tokens(input).unwrap()).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }

    #[test]
    fn rpn_rejects_unmatched_parens() {
        let cases = ["1 + 2)", "(1 + 2", "foo(1", "foo(1,2"];

        for input in cases {
            let tokens = parse_tokens(input).unwrap();
            assert!(
                rpn_stack(tokens).is_err(),
                "expected error for unmatched parens: {input}"
            );
        }
    }
}
