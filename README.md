# Arithmetic Expression Interpreter Comparison
In this study we implement a simple interpreter for arithmetic expressions in several programming languages, and compare their performance.

## Languages Included in Study
- C
- Go
- Haskell
- Java
- Python3
- Rust

## Concrete Grammar of Arithmetic Expressions
Presented in extended Backus-Naur form.

```
addop   =   mulop {('+' | '-') mulop};
mulop   =   unop {('*' | '/') unop};
unop    =   {'-'} term;
term    =   ('(' expr ')') | integer;
integer =   digit{digit};
```

## Notes
- Rust's enum variants made it easier to define tokens
- Go forced me to write more verbose code and to pass error messages up the call stack, but it made it easier to do so