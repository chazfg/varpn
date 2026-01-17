use std::{
    collections::HashMap,
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
    ops::Div,
};

use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use varpn_parser::{Operation, OutputItem, OutputValue, Token, parse_tokens, rpn_stack};
fn calc(line: &str, var_map: &mut CalcState) {
    let rpn_tokens = parse_tokens(line).unwrap();
    let rpn_ops = rpn_stack(rpn_tokens).unwrap();
    let mut calc_stack = Vec::new();
    rpn_ops.iter().for_each(|op| match op {
        OutputItem::Value(OutputValue::Variable(var)) => {
            calc_stack.push(var_map.get_var(var).unwrap());
        }
        OutputItem::Value(OutputValue::Literal(lit)) => calc_stack.push(cast(lit)),
        OutputItem::Value(OutputValue::LastValRef) => calc_stack.push(var_map.last_value.unwrap()),
        OutputItem::Op(op) => {
            let rhs = calc_stack.pop().unwrap();
            let lhs = calc_stack.pop().unwrap();
            calc_stack.push(apply(op, lhs, rhs));
        }
    });
    assert!(calc_stack.len() == 1);
    var_map.set_last_val(calc_stack[0]);
    println!("= {}", calc_stack[0]);
}

fn set_var(line: &str, var_map: &mut CalcState) {
    let splits = line
        .split([' '])
        .filter_map(|s| {
            if s.trim().is_empty() {
                None
            } else {
                Some(s.trim().to_string())
            }
        })
        .collect::<Vec<String>>();
    assert!(splits[0] == "set");
    assert!(splits[2] == "=");

    let var_tok = parse_tokens(&splits[1]).unwrap();
    let val_tok = parse_tokens(&splits[3]).unwrap();

    assert!(var_tok.len() == 1);
    assert!(val_tok.len() == 1);

    let var_name = if let Token::Variable(v) = &var_tok[0] {
        v.to_string()
    } else {
        panic!()
    };
    let var_value = match &val_tok[0] {
        Token::Variable(old_var) => var_map.get_var(old_var).unwrap(),
        Token::Literal(lit) => cast(lit),
        _ => panic!(),
    };
    var_map.set_var(var_name, var_value);
}
enum ReservedWord {
    Set,
    Exit,
}
#[derive(Default, Debug)]
struct CalcState {
    vars: HashMap<String, Value>,
    last_value: Option<Value>,
}
impl CalcState {
    fn get_var(&self, var: &str) -> Option<Value> {
        self.vars.get(var).copied()
    }

    fn set_var(&mut self, var_name: String, var_value: Value) {
        self.vars.insert(var_name, var_value);
    }

    fn set_last_val(&mut self, calc_stack: Value) {
        self.last_value = Some(calc_stack);
    }
}
fn main() {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new().unwrap();
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut calc_state = CalcState::default();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let bare_line = line.trim();
                if !bare_line.is_empty() {
                    rl.add_history_entry(line.as_str()).unwrap();
                    let first_word = bare_line
                        .chars()
                        .take_while(|c| !c.is_whitespace())
                        .collect::<String>();
                    match is_reserved(first_word) {
                        None => calc(line.trim(), &mut calc_state),
                        Some(ReservedWord::Set) => set_var(line.trim(), &mut calc_state),
                        Some(ReservedWord::Exit) => break,
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                // println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                // println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
}

fn is_reserved(first_word: String) -> Option<ReservedWord> {
    match first_word.as_str() {
        "exit" | "quit" => Some(ReservedWord::Exit),
        "set" => Some(ReservedWord::Set),
        _ => None,
    }
}

fn apply(op: &Operation, lhs: Value, rhs: Value) -> Value {
    match op {
        Operation::Add => lhs + rhs,
        Operation::Subtract => lhs - rhs,
        Operation::Multiply => lhs * rhs,
        Operation::Divide => lhs / rhs,
        Operation::Modulo => lhs % rhs,
        Operation::Power => lhs.pow(rhs),
    }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Float(f64),
    Int(i64),
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(n) => write!(f, "{n}"),
            Value::Int(n) => write!(f, "{n}"),
        }
    }
}
impl Value {
    fn try_int(s: &str) -> Result<Value, ParseIntError> {
        s.parse().map(Self::Int)
    }
    fn try_float(s: &str) -> Result<Value, ParseFloatError> {
        s.parse().map(Self::Float)
    }

    fn pow(&self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l.powf(r)),
            (Value::Float(l), Value::Int(r)) => Value::Float(l.powi(r as i32)),
            (Value::Int(l), Value::Float(r)) => Value::Float((*l as f64).powf(r)),
            (Value::Int(l), Value::Int(r)) => Value::Float((*l as f64).powi(r as i32)),
        }
    }
}

fn cast(val: &str) -> Value {
    if val.contains(".") {
        Value::try_float(val).unwrap()
    } else {
        Value::try_int(val).unwrap()
    }
}

impl std::ops::Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l % r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l % r as f64),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 % r),
            (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
        }
    }
}
impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l / r as f64),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 / r),
            (Value::Int(l), Value::Int(r)) => Value::Float(l as f64 / r as f64),
        }
    }
}
impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l * r as f64),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 * r),
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
        }
    }
}
impl std::ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l - r as f64),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 - r),
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
        }
    }
}
impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l + r as f64),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 + r),
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{CalcState, Value, apply, cast};
    use varpn::{OutputItem, OutputValue, parse_tokens, rpn_stack};

    fn eval(input: &str, state: &CalcState) -> Value {
        let rpn_tokens = parse_tokens(input).unwrap();
        let rpn_ops = rpn_stack(rpn_tokens).unwrap();
        let mut stack = Vec::new();

        for op in rpn_ops {
            match op {
                OutputItem::Value(OutputValue::Variable(var)) => {
                    stack.push(state.get_var(&var).unwrap());
                }
                OutputItem::Value(OutputValue::LastValRef) => {
                    stack.push(state.last_value.unwrap());
                }
                OutputItem::Value(OutputValue::Literal(lit)) => stack.push(cast(&lit)),
                OutputItem::Op(op) => {
                    let rhs = stack.pop().unwrap();
                    let lhs = stack.pop().unwrap();
                    stack.push(apply(&op, lhs, rhs));
                }
            }
        }

        assert_eq!(stack.len(), 1);
        stack[0]
    }

    fn eval_no_vars(input: &str) -> Value {
        eval(input, &CalcState::default())
    }

    fn assert_int(actual: Value, expected: i64) {
        match actual {
            Value::Int(n) => assert_eq!(n, expected),
            Value::Float(n) => panic!("expected int {expected}, got float {n}"),
        }
    }

    fn assert_float(actual: Value, expected: f64) {
        match actual {
            Value::Float(n) => assert!((n - expected).abs() < 1e-9),
            Value::Int(n) => panic!("expected float {expected}, got int {n}"),
        }
    }

    #[test]
    fn cast_parses_ints_and_floats() {
        assert_int(eval_no_vars("42"), 42);
        assert_float(eval_no_vars("3.14"), 3.14);
    }

    #[test]
    fn arithmetic_with_precedence() {
        assert_int(eval_no_vars("1 + 2 * 3"), 7);
        assert_int(eval_no_vars("(1 + 2) * 3"), 9);
    }

    #[test]
    fn subtraction_is_left_associative() {
        assert_int(eval_no_vars("10 - 4 - 3"), 3);
    }

    #[test]
    fn integer_division_is_float() {
        assert_float(eval_no_vars("7 / 2"), 3.5);
    }

    #[test]
    fn mixed_numeric_types_promote_to_float() {
        assert_float(eval_no_vars("7.0 / 2"), 3.5);
        assert_float(eval_no_vars("7 / 2.0"), 3.5);
    }

    #[test]
    fn modulo_supports_int_and_float() {
        assert_int(eval_no_vars("5 % 2"), 1);
        assert_float(eval_no_vars("5 % 2.0"), 1.0);
    }

    #[test]
    fn float_arithmetic() {
        assert_float(eval_no_vars("3.5 + 2.25"), 5.75);
        assert_float(eval_no_vars("5.5 * 2"), 11.0);
    }

    #[test]
    fn power_returns_float() {
        assert_float(eval_no_vars("2 ^ 3"), 8.0);
        assert_float(eval_no_vars("9 ^ 0.5"), 3.0);
    }

    #[test]
    fn variables_are_resolved_from_map() {
        let mut state = CalcState::default();
        state.set_var("a".to_string(), Value::Int(3));
        state.set_var("b".to_string(), Value::Float(2.5));
        assert_float(eval("a + b * 2", &state), 8.0);
    }

    #[test]
    fn last_value_reference_uses_dollar_sign() {
        let mut state = CalcState::default();
        state.set_last_val(Value::Int(10));
        assert_int(eval("$ + 2", &state), 12);
    }

    #[test]
    fn calc_state_gets_and_sets_values() {
        let mut state = CalcState::default();
        assert!(state.get_var("missing").is_none());
        state.set_var("x".to_string(), Value::Int(4));
        assert_int(state.get_var("x").unwrap(), 4);
        state.set_last_val(Value::Float(1.25));
        assert_float(state.last_value.unwrap(), 1.25);
    }
}
