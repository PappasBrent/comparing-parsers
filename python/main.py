import sys
from dataclasses import dataclass
from enum import Enum, auto
from typing import Generator, Optional, Union


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


def lex(s: str) -> Generator[Token, None, None]:

    char_to_type = {
        '+': TokenType.ADD,
        '-': TokenType.SUB,
        '*': TokenType.MUL,
        '/': TokenType.DIV,
        '(': TokenType.LPAREN,
        ')': TokenType.RPAREN,
    }

    i = 0
    while i < len(s):
        if s[i].isspace():
            pass
        elif s[i] in char_to_type.keys():
            yield Token(char_to_type[s[i]], s[i])
        elif s[i].isdigit():
            n = int(s[i])
            while i+1 < len(s) and s[i+1].isdigit():
                n *= 10
                n += int(s[i+1])
                i += 1
            yield Token(TokenType.INT, n)
        else:
            raise SyntaxError(f'unexpected character: {s[i]}')

        i += 1  # advance to next char


def interpret(toks: Generator[Token, None, None]) -> int:

    cur, next_ = None, None

    def advance():
        nonlocal cur, next_
        cur = next_
        next_ = next(toks, None)

    def accept(ty: Optional[TokenType]):
        nonlocal cur, next_
        if next_ is None and ty is None:
            return True
        elif next_ is not None and next_.ty == ty:
            advance()
            return True
        return False

    def expect(ty: Optional[TokenType]):
        nonlocal cur, next_
        if not accept(ty):
            raise SyntaxError(
                f'expected {ty}, got {next_.ty if next_ is not None else next_}')
        return True

    def interpret():
        n = addop()
        expect(None)
        return n

    def addop():
        nonlocal cur, next_
        n = mulop()
        while accept(TokenType.ADD) or accept(TokenType.SUB):
            if TokenType.ADD == cur.ty:
                n += mulop()
            elif TokenType.SUB == cur.ty:
                n -= mulop()
        return n

    def mulop():
        nonlocal cur, next_
        n = unop()
        while accept(TokenType.MUL) or accept(TokenType.DIV):
            if TokenType.MUL == cur.ty:
                n *= unop()
            elif TokenType.DIV == cur.ty:
                # divide in this way to round to zero,
                # python's // operator rounds to negative infinity
                n = int(n / unop())
        return n

    def unop():
        parity = 1
        while accept(TokenType.SUB):
            parity *= -1
        n = term()
        return parity * n

    def term():
        nonlocal cur, next_
        if accept(TokenType.LPAREN):
            n = addop()
            expect(TokenType.RPAREN)
            return n
        else:
            expect(TokenType.INT)
            return cur.val

    advance()  # prime cur and next_
    return interpret()


def main():
    for line in sys.stdin.readlines():
        try:
            print(interpret(lex(line)))
        except SyntaxError as e:
            print(f'error: {e}')
            return 1


if __name__ == '__main__':
    main()
