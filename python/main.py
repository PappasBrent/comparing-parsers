from dataclasses import dataclass
from enum import Enum, auto
from typing import Generator, Union


class TokenType(Enum):
    PLUS = auto()
    MINUS = auto()
    MULT = auto()
    DIV = auto()
    LPAREN = auto()
    RPAREN = auto()
    INT = auto()


@dataclass
class Token:
    ty: TokenType
    val: Union[str, int]


def lex(s: str) -> Generator[Token, None, None]:

    char_to_type = {
        '+': TokenType.PLUS,
        '-': TokenType.MINUS,
        '*': TokenType.MULT,
        '/': TokenType.DIV,
        '(': TokenType.LPAREN,
        ')': TokenType.RPAREN,
    }

    i = 0
    while i < len(s):
        if s[i].isspace():
            pass
        elif s[i].isdigit():
            n = int(s[i])
            i += 1
            while i < len(s) and s[i].isdigit():
                n *= 10
                n += int(s[i])
                i += 1
            i -= 1  # put the last char back
            yield Token(TokenType.INT, n)
        elif s[i] in '+-*/()':
            yield Token(char_to_type[s[i]], s[i])

        i += 1  # advance to next char


def interpret(toks: Generator[Token, None, None]) -> int:

    cur, next_ = None, None

    def expect(ty: TokenType):
        nonlocal cur, next_
        if next_.ty != ty:
            raise SyntaxError(f"Expected token type '{ty}'")
        advance()

    def advance():
        nonlocal cur, next_
        cur = next_
        next_ = next(toks, None)

    def accept(ty: TokenType):
        nonlocal cur, next_
        if next_ and next_.ty == ty:
            advance()
            return True
        return False

    def addop():
        nonlocal cur, next_
        n = mulop()
        while accept(TokenType.PLUS) or accept(TokenType.MINUS):
            if TokenType.PLUS == cur.ty:
                n += mulop()
            elif TokenType.MINUS == cur.ty:
                n -= mulop()
        return n

    def mulop():
        nonlocal cur, next_
        n = term()
        while accept(TokenType.MULT) or accept(TokenType.DIV):
            if TokenType.MULT == cur.ty:
                n *= term()
            elif TokenType.DIV == cur.ty:
                n //= term()
        return n

    def term():
        nonlocal cur, next_
        if accept(TokenType.LPAREN):
            n = addop()
            expect(TokenType.RPAREN)
            return n
        return unop()

    def unop():
        parity = 1
        while accept(TokenType.MINUS):
            parity *= -1
        expect(TokenType.INT)
        return parity * cur.val

    advance()  # prime cur and next_
    return addop()


def main():
    while True:
        line = input()
        if line.strip() == 'q':
            return

        print(interpret(lex(line)))


if __name__ == '__main__':
    main()
