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

type Lexer struct {
	i     int
	input []rune
	cur   rune
	next  rune
}

func NewLexer(input []rune) *Lexer {
	l := Lexer{i: 0, input: input, cur: 0, next: 0}

	// prime the lexer
	l.advance()
	l.advance()
	return &l
}

func (l *Lexer) advance() {
	l.cur = l.next
	if l.i < len(l.input) {
		l.next = l.input[l.i]
		l.i++
	} else {
		l.next = 0
	}
}

func (l *Lexer) Lex() ([]*Token, error) {

	result := make([]*Token, 0)

	for l.cur != 0 {
		var Ty TokenType
		var Val int

		if unicode.IsSpace(l.cur) {
			l.advance()
			continue
		} else if l.cur == '+' {
			Ty = Add
		} else if l.cur == '-' {
			Ty = Sub
		} else if l.cur == '*' {
			Ty = Mul
		} else if l.cur == '/' {
			Ty = Div
		} else if l.cur == '(' {
			Ty = LParen
		} else if l.cur == ')' {
			Ty = RParen
		} else if unicode.IsNumber(l.cur) {
			Ty = Int
			n, err := strconv.Atoi(string(l.cur))
			if err != nil {
				return nil, err
			}
			for unicode.IsNumber(l.next) {
				n *= 10
				m, err := strconv.Atoi(string(l.next))
				if err != nil {
					return nil, err
				}
				n += m
				l.advance()
			}
			Val = n
		} else {
			return nil, fmt.Errorf("unexpected token: %c", l.cur)
		}

		result = append(result, &Token{Ty, Val})
		l.advance()
	}
	return result, nil
}

type Interpreter struct {
	i      int
	tokens []*Token
	cur    *Token
	next   *Token
}

func NewInterpreter(tokens []*Token) *Interpreter {
	inter := Interpreter{tokens: tokens, cur: nil, next: nil}
	// prime the interpreter
	inter.advance()
	return &inter
}

func (inter *Interpreter) advance() {
	inter.cur = inter.next
	if inter.tokens != nil && inter.i < len(inter.tokens) {
		inter.next = (inter.tokens)[inter.i]
		inter.i++
	} else {
		inter.next = nil
	}
}

func (inter *Interpreter) accept(ty TokenType) bool {
	if inter.next != nil && inter.next.Ty == ty {
		inter.advance()
		return true
	}
	return false
}

func (inter *Interpreter) expect(ty TokenType) error {
	if inter.next == nil {
		return fmt.Errorf("expected token %d, got none", ty)
	} else if inter.next.Ty != ty {
		return fmt.Errorf("expected token %d, got %d", ty, inter.next.Ty)
	} else {
		inter.advance()
		return nil
	}
}

func (inter *Interpreter) expectEnd() error {
	if inter.next != nil {
		return fmt.Errorf("expected end, got %d", inter.next.Ty)
	}
	return nil
}

func (inter *Interpreter) interpret() (int, error) {
	n, err := inter.addop()
	if err != nil {
		return 0, err
	}
	if err = inter.expectEnd(); err != nil {
		return 0, err
	}
	return n, nil
}

func (inter *Interpreter) addop() (int, error) {
	n, err := inter.mulop()
	if err != nil {
		return 0, err
	}
	for inter.accept(Add) || inter.accept(Sub) {
		if inter.cur.Ty == Add {
			if m, err := inter.mulop(); err == nil {
				n += m
			} else {
				return 0, err
			}
		} else if inter.cur.Ty == Sub {
			if m, err := inter.mulop(); err == nil {
				n -= m
			} else {
				return 0, err
			}
		}
	}
	return n, nil
}

func (inter *Interpreter) mulop() (int, error) {
	n, err := inter.unop()
	if err != nil {
		return 0, err
	}
	for inter.accept(Mul) || inter.accept(Div) {
		if inter.cur.Ty == Mul {
			if m, err := inter.unop(); err == nil {
				n *= m
			} else {
				return 0, err
			}
		} else if inter.cur.Ty == Div {
			if m, err := inter.unop(); err == nil {
				n /= m
			} else {
				return 0, err
			}
		}
	}
	return n, nil
}

func (inter *Interpreter) unop() (int, error) {
	parity := 1
	for inter.accept(Sub) {
		parity *= -1
	}
	n, err := inter.term()
	if err != nil {
		return 0, err
	}
	return parity * n, nil
}

func (inter *Interpreter) term() (int, error) {
	if inter.accept(LParen) {
		n, err := inter.addop()
		if err != nil {
			return 0, err
		}
		if err := inter.expect(RParen); err != nil {
			return 0, err
		}
		return n, nil
	} else {
		if err := inter.expect(Int); err != nil {
			return 0, err
		}
		return inter.cur.Val, nil
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		l := NewLexer([]rune(scanner.Text()))
		toks, err := l.Lex()
		if err != nil {
			log.Fatalln(err)
		}
		inter := NewInterpreter(toks)
		n, err := inter.interpret()
		if err != nil {
			log.Fatalln(err)
		}
		fmt.Println(n)
	}
}
