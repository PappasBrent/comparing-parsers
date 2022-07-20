package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"unicode"
)

type TokenType int

const (
	Add TokenType = iota
	Sub
	Mul
	Div
	LParen
	RParen
	Int
)

type Token struct {
	Ty TokenType

	// only valid if Ty == Int
	Val int
}

func Lex(s string) ([]Token, error) {

	result := make([]Token, 0)

	i := 0
	for i < len(s) {
		var Ty TokenType
		var Val int

		if unicode.IsSpace(rune(s[i])) {
			i++
			continue
		} else if s[i] == '+' {
			Ty = Add
		} else if s[i] == '-' {
			Ty = Sub
		} else if s[i] == '*' {
			Ty = Mul
		} else if s[i] == '/' {
			Ty = Div
		} else if s[i] == '(' {
			Ty = LParen
		} else if s[i] == ')' {
			Ty = RParen
		} else if unicode.IsNumber(rune(s[i])) {
			Ty = Int
			n, err := strconv.Atoi(string(s[i]))
			if err != nil {
				return nil, err
			}
			for i+1 < len(s) && unicode.IsNumber(rune(s[i+1])) {
				n *= 10
				m, _ := strconv.Atoi(string(s[i+1]))
				n += m
				i += 1
			}
			Val = n
		} else {
			return nil, fmt.Errorf("unexpected char: %c", s[i])
		}

		result = append(result, Token{Ty, Val})
		i++
	}
	return result, nil
}

type Parser struct {
	i      int
	tokens []Token
}

func (p *Parser) Reset() {
	p.i = -1
	p.tokens = make([]Token, 0)
}

func (p *Parser) SetTokens(tokens []Token) { p.tokens = tokens }

func (p *Parser) advance() { p.i++ }

func (p *Parser) accept(ty TokenType) bool {
	if p.i+1 < len(p.tokens) && p.tokens[p.i+1].Ty == ty {
		p.advance()
		return true
	}
	return false
}

func (p *Parser) expect(ty TokenType) error {
	if !p.accept(ty) {
		return fmt.Errorf("expected token %q", ty)
	}
	return nil
}

func (p *Parser) expectEnd() error {
	if p.i+1 < len(p.tokens) {
		return fmt.Errorf("expected end of token stream")
	}
	return nil
}

func (p *Parser) parse() (int, error) {
	n, err := p.addsub()
	if err != nil {
		return 0, err
	}
	if err = p.expectEnd(); err != nil {
		return 0, err
	}
	return n, nil
}

func (p *Parser) addsub() (int, error) {
	n, err := p.muldiv()
	if err != nil {
		return 0, err
	}
	for p.accept(Add) || p.accept(Sub) {
		if p.tokens[p.i].Ty == Add {
			if m, err := p.muldiv(); err == nil {
				n += m
			} else {
				return 0, err
			}
		} else if p.tokens[p.i].Ty == Sub {
			if m, err := p.muldiv(); err == nil {
				n -= m
			} else {
				return 0, err
			}
		}
	}
	return n, nil
}

func (p *Parser) muldiv() (int, error) {
	n, err := p.neg()
	if err != nil {
		return 0, err
	}
	for p.accept(Mul) || p.accept(Div) {
		if p.tokens[p.i].Ty == Mul {
			if m, err := p.neg(); err == nil {
				n *= m
			} else {
				return 0, err
			}
		} else if p.tokens[p.i].Ty == Div {
			if m, err := p.neg(); err == nil {
				n /= m
			} else {
				return 0, err
			}
		}
	}
	return n, nil
}

func (p *Parser) neg() (int, error) {
	parity := 1
	for p.accept(Sub) {
		parity *= -1
	}
	n, err := p.parenint()
	if err != nil {
		return 0, err
	}
	return parity * n, nil
}

func (p *Parser) parenint() (int, error) {
	if p.accept(LParen) {
		n, err := p.addsub()
		if err != nil {
			return 0, err
		}
		if err := p.expect(RParen); err != nil {
			return 0, err
		}
		return n, nil
	} else {
		if err := p.expect(Int); err != nil {
			return 0, err
		}
		return p.tokens[p.i].Val, nil
	}
}

func main() {
	// read from stdin
	var scanner *bufio.Scanner
	if len(os.Args) < 2 {
		scanner = bufio.NewScanner(os.Stdin)
		// read from a file
	} else {
		f, err := os.Open(os.Args[1])
		if err != nil {
			log.Fatalln(err)
		}
		scanner = bufio.NewScanner(f)
	}
	p := Parser{i: 0, tokens: make([]Token, 0)}
	for scanner.Scan() {
		p.Reset()
		tokens, err := Lex(scanner.Text())
		if err != nil {
			log.Fatalln(err)
		}
		p.SetTokens(tokens)
		n, err := p.parse()
		if err != nil {
			log.Fatalln(err)
		}
		fmt.Println(n)
	}
}
