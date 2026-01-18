use varpn_parser::{err::VarpnResult, parse_tokens, rpn_stack};

const T1: &str = "sum(1,2)";

fn main() -> VarpnResult<()> {
    let tokens = parse_tokens(T1)?;
    println!("{tokens:?}");
    let stack = rpn_stack(tokens);
    println!("{stack:?}");
    Ok(())
}
