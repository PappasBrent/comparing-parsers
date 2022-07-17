import sys
from dataclasses import dataclass
from enum import Enum, auto
from typing import List, Union


class TokenType(Enum):
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    LPAREN = auto()
    RPAREN = auto()
    INT = auto()


@dataclass
class Token:
    ty: TokenType
    val: Union[str, int]


def lex(s: str) -> List[Token]:
    i = 0
    toks = []
    while i < len(s):
        if s[i].isspace():
            pass
        elif s[i] == '+':
            toks.append(Token(TokenType.ADD, '+'))
        elif s[i] == '-':
            toks.append(Token(TokenType.SUB, '-'))
        elif s[i] == '*':
            toks.append(Token(TokenType.MUL, '*'))
        elif s[i] == '/':
            toks.append(Token(TokenType.DIV, '/'))
        elif s[i] == '(':
            toks.append(Token(TokenType.LPAREN, '('))
        elif s[i] == ')':
            toks.append(Token(TokenType.RPAREN, ')'))
        elif s[i].isdigit():
            n = int(s[i])
            while i+1 < len(s) and s[i+1].isdigit():
                n *= 10
                n += int(s[i+1])
                i += 1
            toks.append(Token(TokenType.INT, n))
        else:
            raise SyntaxError(f'unexpected character: {s[i]}')

        i += 1  # advance to next char
    return toks


class Parser:
    def __init__(self) -> None:
        self.i = -1
        self.toks = []

    def set_toks(self, toks: List[Token]) -> None:
        self.toks = toks

    def reset(self):
        self.i = -1
        self.toks = []

    def advance(self):
        self.i += 1

    def accept(self, ty: TokenType) -> bool:
        if self.i+1 < len(self.toks) and self.toks[self.i+1].ty == ty:
            self.advance()
            return True
        return False

    def expect(self, ty: TokenType):
        if not self.accept(ty):
            if self.i < len(self.toks):
                raise SyntaxError(
                    f'expected token type {ty}, got {self.toks[self.i+1].ty}')
            else:
                raise SyntaxError(f'expected token type {ty}, got end')

    def expect_end(self):
        if self.i+1 < len(self.toks):
            raise SyntaxError(
                f'expected end of token stream, instead at index {self.i} of {len(self.toks)}')

    def parse(self) -> int:
        n = self.addsub()
        self.expect_end()
        return n

    def addsub(self) -> int:
        n = self.muldiv()
        while self.accept(TokenType.ADD) or self.accept(TokenType.SUB):
            if TokenType.ADD == self.toks[self.i].ty:
                n += self.muldiv()
            elif TokenType.SUB == self.toks[self.i].ty:
                n -= self.muldiv()
        return n

    def muldiv(self) -> int:
        n = self.neg()
        while self.accept(TokenType.MUL) or self.accept(TokenType.DIV):
            if TokenType.MUL == self.toks[self.i].ty:
                n *= self.neg()
            elif TokenType.DIV == self.toks[self.i].ty:
                # divide in this way to round to zero,
                # python's // operator rounds to negative infinity
                n = int(n / self.neg())
        return n

    def neg(self) -> int:
        parity = 1
        while self.accept(TokenType.SUB):
            parity *= -1
        n = self.parenint()
        return parity * n

    def parenint(self) -> int:
        if self.accept(TokenType.LPAREN):
            n = self.addsub()
            self.expect(TokenType.RPAREN)
            return n
        self.expect(TokenType.INT)
        return self.toks[self.i].val


def main():
    parser = Parser()
    for line in sys.stdin.readlines():
        parser.reset()
        parser.set_toks(lex(line))
        print(parser.parse())


if __name__ == '__main__':
    main()
