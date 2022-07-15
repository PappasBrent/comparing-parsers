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
- Rust's enum variants gave me an intuitive way to define tokens, and also the language itself ensured that I would not encounter memory errors
- Go forced me to write more verbose code and to pass error messages up the call stack, but it made it easier to do so.
I still ran into memory errors.
- Python was the easiest to implement since I mostly copied the code from chapter 2.19 of the *Python Cookbook, Third Edition* (Beazley and Jones, 2013).
If I were building my own language though, I probably wouldn't choose Python because of the lack of static typing.
- The Haskell implementation is by far the shortest since it uses parser combinators, but some could see this as a downside since at times the code can be a bit terse.
- The C implementation is the longest, and of course I ran into memory errors while implementing it :)