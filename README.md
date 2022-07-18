# Arithmetic Expression Interpreter Comparison
In this study we implement a simple interpreter for arithmetic expressions in several programming languages, and compare their performance.


<!-- TODO: Include version numbers -->
## Languages Included in Study
- C
- C#
- C++
- Go
- Haskell
- Java
- Javascript
- Python3
- Rust
- Typescript

## Concrete Grammar of Arithmetic Expressions
Presented in extended Backus-Naur form.

```
addop   =   mulop {('+' | '-') mulop};
mulop   =   unop {('*' | '/') unop};
unop    =   {'-'} term;
term    =   ('(' expr ')') | integer;
integer =   digit{digit};
```