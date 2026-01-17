### Overview

- **Parser**: Converts infix to rpn formatted tokens
- **Calc**: Cli that uses the parser output to calculate things


### Calculator

#### Usage
```bash
# enter the cli
$ varpn_calc
>> 
```

do calcs: $ can reference last value returned 
```bash
>> 1 + 1
= 2
>> $
= 2
>> $ + 17
= 19
```

set vars
```bash
>> set x = 17
>> x
= 17
>> x + 5
= 22
```

#### Operations Available
Add, Subtract, Multiply, Divide, Module, Exponents

#### Numbers Supported
Ints, Floats

#### Reserved Words
- Exit: quit execution
- Set: set var 
