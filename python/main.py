from dataclasses import dataclass
from enum import Enum, auto
from typing import Generator, Union


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
                n //= unop()
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
    return addop()


def main():
    while (line := input()):
        print(interpret(lex(line)))


if __name__ == '__main__':
    main()
