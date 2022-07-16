#!/usr/bin/python3

import operator
import sys
from dataclasses import dataclass
from random import choice, randint
from typing import Callable


def c_int_div(x: int, y: int) -> int:
    return int(x / y)


op_to_str = {
    operator.add: '+',
    operator.sub: '-',
    operator.mul: '*',
    c_int_div: '/',
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
        # use small numbers to (hopefully) avoid overflow
        # TODO: figure out how to intelligently avoid overflow?
        return ParenExpr(Int(randint(1, 10)))

    kind = choice([BinExpr, UnExpr])

    if kind == BinExpr:
        # don't produce division expressions since different languages
        # round differently
        op = choice([operator.add, operator.sub, operator.mul, c_int_div])
        lhs = gen_expr(depth-1)
        rhs = gen_expr(depth-1)
        # prevent divide-by-zero
        if op is c_int_div:
            while rhs.eval() == 0:
                rhs = gen_expr(depth-1)
        # TODO: prevent overflow
        return ParenExpr(BinExpr(op, lhs, rhs))

    elif kind == UnExpr:
        op = operator.neg
        operand = ParenExpr(gen_expr(depth-1))
        return ParenExpr(UnExpr(op, operand))

    assert False and "chose a type not checked for"


def main():
    n = int(sys.argv[1])
    for _ in range(n):
        print(gen_expr(5))


if __name__ == '__main__':
    main()
