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
        // skip whitespace
        while let Some(c) = self.cur {
            if c.is_whitespace() {
                self.advance()
            } else {
                break;
            }
        }
        // match token
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
                _ => panic!("unexpected character: {}", c),
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

    fn accept(&mut self, ot: Option<Token>) -> bool {
        // accept the next token if it's the given variant
        match (self.next, ot) {
            (Some(nt), Some(t)) if std::mem::discriminant(&nt) == std::mem::discriminant(&t) => {
                self.advance();
                true
            }
            (None, None) => true,
            _ => false,
        }
    }

    fn expect(&mut self, t: Option<Token>) -> Result<(), String> {
        if !self.accept(t) {
            Err(format!("Expected {:#?}", t))
        } else {
            Ok(())
        }
    }

    pub fn interpret(&mut self) -> Result<i32, String> {
        let n = self.addop()?;
        self.expect(None)?;
        Ok(n)
    }

    fn addop(&mut self) -> Result<i32, String> {
        let mut n = self.mulop()?;
        while self.accept(Some(Token::Add)) || self.accept(Some(Token::Sub)) {
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
        while self.accept(Some(Token::Mul)) || self.accept(Some(Token::Div)) {
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
        while self.accept(Some(Token::Sub)) {
            parity *= -1;
        }
        let n = self.term()?;
        Ok(n * parity)
    }
    fn term(&mut self) -> Result<i32, String> {
        if self.accept(Some(Token::LParen)) {
            let n = self.addop()?;
            self.expect(Some(Token::RParen))?;
            Ok(n)
        } else {
            self.expect(Some(Token::Int(0)))?;
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
