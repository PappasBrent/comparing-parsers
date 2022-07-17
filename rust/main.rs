use std::io::stdin;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Token {
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    RParen,
    Int(i32),
}

fn lex(s: &str) -> Vec<Token> {
    let mut i = 0;
    let mut result = vec![];
    while i < s.len() {
        let c = s.as_bytes()[i] as char;
        match c {
            ws if ws.is_whitespace() => {}
            '+' => result.push(Token::Add),
            '-' => result.push(Token::Sub),
            '*' => result.push(Token::Mul),
            '/' => result.push(Token::Div),
            '(' => result.push(Token::LParen),
            ')' => result.push(Token::RParen),
            d if d.is_digit(10) => {
                let mut n = d.to_string().parse::<i32>().unwrap();
                while i + 1 < s.len() && (s.as_bytes()[i + 1] as char).is_digit(10) {
                    n *= 10;
                    n += (s.as_bytes()[i + 1] as char)
                        .to_string()
                        .parse::<i32>()
                        .unwrap();
                    i += 1;
                }
                result.push(Token::Int(n))
            }
            _ => panic!("unexpected char: {}", c),
        }
        i += 1;
    }
    result
}

struct Parser<'a> {
    i: i32,
    toks: &'a [Token],
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Parser { i: -1, toks: &[] }
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn accept(&mut self, t: Token) -> bool {
        // accept the next token if it's the given variant
        if self.i + 1 < self.toks.len() as i32
            && std::mem::discriminant(&self.toks[(self.i + 1) as usize])
                == std::mem::discriminant(&t)
        {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, t: Token) -> Result<(), String> {
        if !self.accept(t) {
            Err(format!("expected {:#?}", t))
        } else {
            Ok(())
        }
    }

    fn expect_end(&self) -> Result<(), String> {
        if (self.i + 1) < (self.toks.len() as i32) {
            Err(format!("expected end"))
        } else {
            Ok(())
        }
    }

    pub fn parse(&mut self) -> Result<i32, String> {
        let n = self.addsub()?;
        self.expect_end()?;
        Ok(n)
    }

    fn addsub(&mut self) -> Result<i32, String> {
        let mut n = self.muldiv()?;
        while self.accept(Token::Add) || self.accept(Token::Sub) {
            if self.toks[self.i as usize] == Token::Add {
                n += self.muldiv()?;
            } else if self.toks[self.i as usize] == Token::Sub {
                n -= self.muldiv()?;
            }
        }
        Ok(n)
    }
    fn muldiv(&mut self) -> Result<i32, String> {
        let mut n = self.neg()?;
        while self.accept(Token::Mul) || self.accept(Token::Div) {
            if self.toks[self.i as usize] == Token::Mul {
                n *= self.neg()?;
            } else if self.toks[self.i as usize] == Token::Div {
                n /= self.neg()?;
            }
        }
        Ok(n)
    }
    fn neg(&mut self) -> Result<i32, String> {
        let mut parity = 1;
        while self.accept(Token::Sub) {
            parity *= -1;
        }
        let n = self.parenint()?;
        Ok(n * parity)
    }
    fn parenint(&mut self) -> Result<i32, String> {
        if self.accept(Token::LParen) {
            let n = self.addsub()?;
            self.expect(Token::RParen)?;
            Ok(n)
        } else {
            self.expect(Token::Int(0))?;
            match self.toks[self.i as usize] {
                Token::Int(n) => Ok(n as i32),
                _ => Err("Expected an int".to_owned()),
            }
        }
    }
}

pub fn main() -> Result<(), String> {
    let mut input = String::new();
    while let Ok(n) = stdin().read_line(&mut input) {
        // check for eof
        if n == 0 {
            break;
        }
        let toks = lex(input.as_str());
        let mut p = Parser::new();
        p.toks = toks.as_slice();
        let res = p.parse()?;
        println!("{}", res);
        input.clear();
    }
    Ok(())
}
