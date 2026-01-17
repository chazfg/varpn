use varpn::{err::VarpnResult, parse_tokens, rpn_stack};

const T1: &str = "a - b - c";

fn main() -> VarpnResult<()> {
    let tokens = parse_tokens(T1)?;
    let stack = rpn_stack(tokens);
    println!("{stack:?}");
    Ok(())
}
