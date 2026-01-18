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

/// CallFrame represents the call of a function
#[derive(Default, Debug)]
struct CallFrame {
    /// Function Name
    name: String,
    /// Count of commas seen in callsite
    comma: u32,
    /// If arg has been seen (for functions with no args)
    saw_arg: bool,
    /// Paren nesting depth
    depth: u32,
}
impl CallFrame {
    pub fn new(name: String) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    fn on_comma(&mut self) {
        self.comma += 1;
        self.saw_arg = false;
    }
}
#[derive(Default, Debug)]
struct OutputStack {
    stack: Vec<StackEntry>,
}

impl OutputStack {
    fn _right_assoc_push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        let mut out_vec = Vec::new();
        let new_op_prec = new_op.precedence();
        while let Some(stack_top) = self.stack.last().as_ref() {
            match stack_top {
                #[cfg(feature = "functions")]
                StackEntry::Func(..) => {
                    self.stack.push(new_op.into());
                    break;
                }
                StackEntry::Op(operation) => {
                    let old_op_prec = operation.precedence();
                    match new_op_prec.cmp(&old_op_prec) {
                        Ordering::Less => {
                            out_vec.push(
                                self.stack
                                    .pop()
                                    .unwrap()
                                    .op()
                                    .trace(line!(), "op conversion")?
                                    .into(),
                            );
                            if self.stack.is_empty() {
                                self.stack.push(new_op.into());
                                break;
                            }
                        }
                        Ordering::Equal => {
                            self.stack.push(StackEntry::Op(new_op));
                            break;
                        }
                        Ordering::Greater => {
                            self.stack.push(new_op.into());
                            break;
                        }
                    }
                }

                StackEntry::Control(..) => {
                    self.stack.push(StackEntry::Op(new_op));
                    break;
                }
            }
        }
        Ok(out_vec)
    }
    fn _left_assoc_push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        let mut out_vec = Vec::new();
        let new_op_prec = new_op.precedence();
        while let Some(stack_top) = self.stack.last() {
            match stack_top {
                #[cfg(feature = "functions")]
                StackEntry::Func(..) => {
                    self.stack.push(new_op.into());
                    break;
                }
                StackEntry::Op(operation) => {
                    let old_op_prec = operation.precedence();
                    match new_op_prec.cmp(&old_op_prec) {
                        Ordering::Less => {
                            out_vec.push(
                                self.stack
                                    .pop()
                                    .unwrap()
                                    .op()
                                    .trace(line!(), "op conversion")?
                                    .into(),
                            );
                            if self.stack.is_empty() {
                                self.stack.push(new_op.into());
                                break;
                            }
                        }
                        Ordering::Equal => {
                            out_vec.push(operation.into());
                            self.stack.pop().unwrap();
                            self.stack.push(StackEntry::Op(new_op));
                            break;
                        }
                        Ordering::Greater => {
                            self.stack.push(new_op.into());
                            break;
                        }
                    }
                }

                StackEntry::Control(..) => {
                    self.stack.push(StackEntry::Op(new_op));
                    break;
                }
            }
        }
        Ok(out_vec)
    }
    pub fn push_op(&mut self, new_op: Op) -> VarpnResult<Vec<RpnItem>> {
        if self.stack.is_empty() {
            self.stack.push(StackEntry::Op(new_op));
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
                self.stack.push(control_op.into());
                Ok(out_vec)
            }
            #[cfg(feature = "functions")]
            Control::Comma => {
                while let Some(top) = self.stack.last() {
                    match top {
                        StackEntry::Op(op) => {
                            out_vec.push((*op).into());
                            self.stack.pop();
                        }

                        StackEntry::Func(..)
                        | StackEntry::Control(Control::LParen)
                        | StackEntry::Control(Control::Comma) => break,
                        StackEntry::Control(Control::RParen) => unreachable!(),
                    }
                }
                self.stack.push(StackEntry::Control(Control::Comma));
                Ok(out_vec)
            }
            Control::RParen => {
                let mut found_lparen = false;
                while let Some(popped_op) = self.stack.pop() {
                    match popped_op {
                        StackEntry::Op(op) => out_vec.push(op.into()),
                        #[cfg(feature = "functions")]
                        StackEntry::Func(..) => {}
                        #[cfg(feature = "functions")]
                        StackEntry::Control(Control::Comma) => {}
                        StackEntry::Control(Control::LParen) => {
                            found_lparen = true;
                            break;
                        }
                        StackEntry::Control(Control::RParen) => unreachable!(),
                    }
                }
                if !found_lparen {
                    return Err(VarpnErr::new(line!(), "Unmatched ')'"));
                }
                #[cfg(feature = "functions")]
                if let Some(StackEntry::Func(..)) = self.stack.last() {
                    self.stack.pop();
                }
                Ok(out_vec)
            }
        }
    }

    #[cfg(feature = "functions")]
    fn push_func(&mut self, func_name: String) -> VarpnResult<()> {
        self.stack.push(StackEntry::Func(func_name));
        Ok(())
    }
}

pub fn rpn_stack(mut tokens: Vec<Lexeme>) -> VarpnResult<Vec<RpnItem>> {
    let mut op_stack = OutputStack::default();
    let mut output: Vec<RpnItem> = Vec::new();
    let mut call_frames: Vec<CallFrame> = Vec::new();
    tokens.drain(..).try_for_each(|t| -> VarpnResult<()> {
        match t {
            Lexeme::Ident(s) => {
                output.push(RpnItem::Value(Atom::Variable(s)));
                if let Some(frame) = call_frames.last_mut() {
                    frame.saw_arg = true;
                }
            }
            Lexeme::Number(s) => {
                output.push(RpnItem::Value(Atom::Literal(s)));
                if let Some(frame) = call_frames.last_mut() {
                    frame.saw_arg = true;
                }
            }
            Lexeme::LastValRef => {
                output.push(RpnItem::Value(Atom::LastValRef));
                if let Some(frame) = call_frames.last_mut() {
                    frame.saw_arg = true;
                }
            }
            Lexeme::Op(operation) => {
                output.extend(op_stack.push_op(operation).trace(line!(), "extend op")?);
            }
            Lexeme::Control(Control::Comma) => {
                output.extend(
                    op_stack
                        .push_control(Control::Comma)
                        .trace(line!(), "rpn stack")?,
                );
                if let Some(frame) = call_frames.last_mut()
                    && frame.depth == 1
                {
                    frame.on_comma();
                }
            }
            Lexeme::Control(Control::RParen) => {
                output.extend(
                    op_stack
                        .push_control(Control::RParen)
                        .trace(line!(), "rpn stack")?,
                );
                if let Some(frame) = call_frames.last_mut() {
                    if frame.depth == 1 {
                        let frame = call_frames.pop().unwrap();
                        let CallFrame {
                            name,
                            comma,
                            saw_arg,
                            ..
                        } = frame;
                        output.push(RpnItem::Func(name, arity(comma, saw_arg)));
                        if let Some(parent) = call_frames.last_mut() {
                            parent.saw_arg = true;
                        }
                    } else {
                        frame.depth -= 1;
                    }
                }
            }
            Lexeme::Control(Control::LParen) => {
                output.extend(
                    op_stack
                        .push_control(Control::LParen)
                        .trace(line!(), "rpn stack")?,
                );
                if let Some(frame) = call_frames.last_mut() {
                    frame.depth += 1;
                }
            }
            #[cfg(feature = "functions")]
            Lexeme::Function(func_name) => {
                call_frames.push(CallFrame::new(func_name.clone()));
                op_stack.push_func(func_name).trace(line!(), "function")?
            }
        }
        Ok(())
    })?;
    while let Some(v) = op_stack.stack.pop() {
        match v {
            StackEntry::Op(x) => output.push(RpnItem::Op(x)),
            StackEntry::Control(Control::LParen) => {
                return Err(VarpnErr::new(line!(), "Unmatched '('"));
            }
            StackEntry::Control(Control::RParen) => unreachable!(),
            #[cfg(feature = "functions")]
            StackEntry::Control(Control::Comma) => {
                return Err(VarpnErr::new(line!(), "Dangling ','"));
            }
            #[cfg(feature = "functions")]
            StackEntry::Func(..) => {
                return Err(VarpnErr::new(line!(), "Unclosed function call"));
            }
        }
    }
    Ok(output)
}

fn arity(comma: u32, saw_arg: bool) -> u32 {
    if comma == 0 && !saw_arg { 0 } else { comma + 1 }
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
                #[cfg(feature = "functions")]
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_func(std::mem::take(&mut buf))
                            .trace(line!(), "parse tokens")?,
                    );
                }
                tokens.push(Lexeme::Control(Control::LParen));
                #[cfg(not(feature = "functions"))]
                if !buf.is_empty() {
                    tokens.push(
                        Lexeme::parse_text(std::mem::take(&mut buf))
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
    #[cfg(feature = "functions")]
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
    #[cfg(feature = "functions")]
    Func(String),
    Op(Op),
    Control(Control),
}
impl StackEntry {
    fn op(&self) -> VarpnResult<Op> {
        match self {
            StackEntry::Op(operation) => Ok(*operation),
            #[cfg(feature = "functions")]
            StackEntry::Func(..) => {
                Err(VarpnErr::new(line!(), format!("can't make {self:?} an op")))
            }

            StackEntry::Control(..) => {
                Err(VarpnErr::new(line!(), format!("can't make {self:?} an op")))
            }
        }
    }
}

#[derive(Debug)]
pub enum Lexeme {
    Ident(String),
    Number(String),
    #[cfg(feature = "functions")]
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

    #[cfg(feature = "functions")]
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
    #[cfg(feature = "functions")]
    Comma,
    LParen,
    RParen,
}
impl Control {
    fn from_char(c: char) -> Control {
        match c {
            '(' => Self::LParen,
            ')' => Self::RParen,
            #[cfg(feature = "functions")]
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
                #[cfg(feature = "functions")]
                Lexeme::Control(Control::Comma) => "comma".to_string(),
                Lexeme::LastValRef => "$".to_string(),
                #[cfg(feature = "functions")]
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
                #[cfg(feature = "functions")]
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
    #[cfg(feature = "functions")]
    #[test]
    fn parse_tokens_function_matrix() {
        let cases = [
            ("foo(a)", vec!["func:foo", "lparen", "var:a", "rparen"]),
            (
                "sum(1,2)",
                vec!["func:sum", "lparen", "lit:1", "comma", "lit:2", "rparen"],
            ),
            (
                "avg(1.5,2.5)",
                vec![
                    "func:avg", "lparen", "lit:1.5", "comma", "lit:2.5", "rparen",
                ],
            ),
            (
                "max(a,b,c)",
                vec![
                    "func:max", "lparen", "var:a", "comma", "var:b", "comma", "var:c", "rparen",
                ],
            ),
        ];

        for (input, expected) in cases {
            let actual = token_sig(parse_tokens(input).unwrap());
            let expected = expected.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }
    #[cfg(feature = "functions")]
    #[test]
    fn rpn_function_matrix() {
        let cases = [
            ("foo()", vec!["func:foo/0"]),
            ("foo(a)", vec!["var:a", "func:foo/1"]),
            ("foo(,)", vec!["func:foo/2"]),
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
