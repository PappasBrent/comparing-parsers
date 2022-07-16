#!/usr/bin/python3

import operator
from dataclasses import dataclass
from random import choice, randint
from typing import Callable

op_to_str = {
    operator.add: '+',
    operator.sub: '-',
    operator.mul: '*',
    operator.floordiv: '/',
    operator.neg: '-'
}


@dataclass
class Expr:

    def __str__(self) -> str:
        assert False and "unimplemented"

    def eval(self) -> int:
        assert False and "unimplemented"


@dataclass
class BinExpr(Expr):
    op: Callable[[int, int], int]
    lhs: Expr
    rhs: Expr

    def __str__(self) -> str:
        return f'{self.lhs} {op_to_str[self.op]} {self.rhs}'

    def eval(self) -> int:
        return self.op(self.lhs.eval(), self.rhs.eval())


@dataclass
class UnExpr(Expr):
    op: Callable[[int], int]
    operand: Expr

    def __str__(self) -> str:
        return f'{op_to_str[self.op]}{self.operand}'

    def eval(self) -> int:
        return self.op(self.operand.eval())


@dataclass
class ParenExpr(Expr):
    inner: Expr

    def __str__(self) -> str:
        return f'({self.inner})'

    def eval(self) -> int:
        return self.inner.eval()


@dataclass
class Int(Expr):
    val: int

    def __str__(self) -> str:
        return str(self.val)

    def eval(self) -> int:
        return self.val


def gen_expr(depth: int) -> Expr:
    if depth == 0:
        return Int(randint(1, 100))

    kind = choice([BinExpr, UnExpr, ParenExpr])

    if kind == BinExpr:
        op = choice([operator.add, operator.sub,
                    operator.mul, operator.floordiv])
        lhs = gen_expr(depth-1)
        rhs = gen_expr(depth-1)
        # Ensure that we don't create an expression which divides by zero
        if op == operator.floordiv:
            while rhs.eval() == 0:
                rhs = gen_expr(depth-1)
        return BinExpr(op, lhs, rhs)

    elif kind == UnExpr:
        op = operator.neg
        operand = gen_expr(depth-1)
        return UnExpr(op, operand)

    elif kind == ParenExpr:
        inner = gen_expr(depth-1)
        return ParenExpr(inner)

    assert False and "chose a type not checked for"


def main():
    for _ in range(1000):
        print(gen_expr(5))


if __name__ == '__main__':
    main()
