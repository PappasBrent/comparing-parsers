use std::{io::stdin, str::Chars};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Token {
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    RParen,
    Int(u32),
}

#[derive(Debug)]
struct Lexer<'a> {
    input: Chars<'a>,
    cur: Option<char>,
    next: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a String) -> Self {
        let mut lexer = Self {
            input: input.chars(),
            cur: None,
            next: None,
        };
        //advance twice to 'prime' the lexer
        lexer.advance();
        lexer.advance();

        lexer
    }

    fn advance(&mut self) {
        self.cur = self.next;
        self.next = self.input.next();
    }
}

// TODO: implement Peekable for Lexer?
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.cur {
            let result = match c {
                '+' => Some(Token::Add),
                '-' => Some(Token::Sub),
                '*' => Some(Token::Mul),
                '/' => Some(Token::Div),
                '(' => Some(Token::LParen),
                ')' => Some(Token::RParen),
                d if d.is_ascii_digit() => {
                    let mut n = d.to_digit(10).unwrap();
                    while let Some(d) = self.next {
                        if d.is_ascii_digit() {
                            n *= 10;
                            n += d.to_digit(10).unwrap();
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    Some(Token::Int(n))
                }
                _ => None,
            };
            self.advance();
            result
        } else {
            None
        }
    }
}

struct Interpeter<'a> {
    toks: &'a mut dyn Iterator<Item = Token>,
    cur: Option<Token>,
    next: Option<Token>,
}

impl<'a> Interpeter<'a> {
    fn new(toks: &'a mut dyn Iterator<Item = Token>) -> Self {
        let mut res = Self {
            toks,
            cur: None,
            next: None,
        };
        res.advance();
        res
    }

    fn advance(&mut self) {
        self.cur = self.next;
        self.next = self.toks.next();
    }

    fn accept(&mut self, t: Token) -> bool {
        // this is a little weird, we want to accept this token
        // if it's the same type as the other one, but we don't
        // care about values for int tokens
        if let Some(nt) = self.next {
            match nt {
                Token::Int(_) => match t {
                    Token::Int(_) => {
                        self.advance();
                        true
                    }
                    _ => false,
                },
                t1 if t1 == t => {
                    self.advance();
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    }

    fn expect(&mut self, t: Token) -> Result<(), String> {
        if !self.accept(t) {
            Err(format!("Expected {:#?}", t))
        } else {
            Ok(())
        }
    }

    pub fn interpret(&mut self) -> Result<i32, String> {
        self.addop()
    }

    fn addop(&mut self) -> Result<i32, String> {
        let mut n = self.mulop()?;
        while self.accept(Token::Add) || self.accept(Token::Sub) {
            if self.cur.unwrap() == Token::Add {
                n += self.mulop()?;
            } else if self.cur.unwrap() == Token::Sub {
                n -= self.mulop()?;
            }
        }
        Ok(n)
    }
    fn mulop(&mut self) -> Result<i32, String> {
        let mut n = self.unop()?;
        while self.accept(Token::Mul) || self.accept(Token::Div) {
            if self.cur.unwrap() == Token::Mul {
                n *= self.unop()?;
            } else if self.cur.unwrap() == Token::Div {
                n /= self.unop()?;
            }
        }
        Ok(n)
    }
    fn unop(&mut self) -> Result<i32, String> {
        let mut parity = 1;
        while self.accept(Token::Sub) {
            parity *= -1;
        }
        let n = self.term()?;
        Ok(n * parity)
    }
    fn term(&mut self) -> Result<i32, String> {
        if self.accept(Token::LParen) {
            let n = self.addop()?;
            self.expect(Token::RParen)?;
            Ok(n)
        } else {
            self.expect(Token::Int(0))?;
            match self.cur.unwrap() {
                Token::Int(n) => Ok(n as i32),
                _ => Err("Expected an int".to_owned()),
            }
        }
    }
}

pub fn main() -> Result<(), String> {
    let mut input = String::new();
    while let Ok(_) = stdin().read_line(&mut input) {
        let mut lexer = Lexer::new(&input);
        let mut interpeter = Interpeter::new(&mut lexer);
        let res = interpeter.interpret()?;
        println!("{}", res);
        input.clear();
    }
    Ok(())
}
