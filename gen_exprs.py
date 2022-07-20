#!/usr/bin/python3

import operator
import sys
from dataclasses import dataclass
from random import choice, randint
from typing import Callable


# performs c-style division, rounding towards zero
def c_int_div(x: int, y: int) -> int:
    return int(x / y)


# mapping from operators to their string representation
op_to_str = {
    operator.add: '+',
    operator.sub: '-',
    operator.mul: '*',
    c_int_div: '/',
    operator.neg: '-'
}


@dataclass
class Expr:
    '''an arithmetic expression'''

    def __str__(self) -> str:
        '''string representation of this expression'''
        assert False and "unimplemented"

    def eval(self) -> int:
        '''what this expression evaluates to'''
        assert False and "unimplemented"


@dataclass
class BinExpr(Expr):
    '''a binary arithmetic expression'''

    op: Callable[[int, int], int]   # binary operator
    lhs: Expr                       # left operand
    rhs: Expr                       # right operand

    def __str__(self) -> str:
        return f'{self.lhs} {op_to_str[self.op]} {self.rhs}'

    def eval(self) -> int:
        return self.op(self.lhs.eval(), self.rhs.eval())


@dataclass
class UnExpr(Expr):
    '''a unary arithmetic expression'''

    op: Callable[[int], int]    # unary operator
    operand: Expr               # operand

    def __str__(self) -> str:
        return f'{op_to_str[self.op]}{self.operand}'

    def eval(self) -> int:
        return self.op(self.operand.eval())


@dataclass
class ParenExpr(Expr):
    '''a parenthesized arithmetic expression'''

    inner: Expr  # the inner expression

    def __str__(self) -> str:
        return f'({self.inner})'

    def eval(self) -> int:
        return self.inner.eval()


@dataclass
class Int(Expr):
    '''an integer literal'''

    val: int    # the value this integer literal represents

    def __str__(self) -> str:
        return str(self.val)

    def eval(self) -> int:
        return self.val


def gen_expr(depth: int) -> Expr:
    '''
    given a maximum nesting depth, recursively generates
    parenthesized nonterminal expressions until the max depth is reached
    '''

    # if we've reached the max depth, emit an integer literal
    if depth == 0:
        return ParenExpr(Int(randint(1, 10)))

    # otherwise, pick a rand nonterminal to emit
    # we do not emit parenthesized expressions because we parenthesize
    # all subexpressions anyway
    kind = choice([BinExpr, UnExpr])

    if kind == BinExpr:
        # generate the operator
        op = choice([operator.add, operator.sub, operator.mul, c_int_div])
        lhs = gen_expr(depth-1)  # generate the LHS
        rhs = gen_expr(depth-1)  # generate the RHS
        # prevent divide-by-zero
        if op is c_int_div:
            # keep regenerating the RHS until it doesn't equal zero
            while rhs.eval() == 0:
                rhs = gen_expr(depth-1)
        return ParenExpr(BinExpr(op, lhs, rhs))

    elif kind == UnExpr:
        op = operator.neg                       # generate the operator
        operand = ParenExpr(gen_expr(depth-1))  # generate the operand
        return ParenExpr(UnExpr(op, operand))

    # shouldn't happen
    assert False and "chose a type not checked for"


def main():
    '''
    prints n expressions, where n is the argument passed
    to gen_exprs.py at the command line
    '''

    n = int(sys.argv[1])
    for _ in range(n):
        e = gen_expr(randint(1, 5))
        # prevent integer overflow
        while not (-2147483648 <= e.eval() <= 2147483647):
            e = gen_expr(1, 5)
        print(e)


if __name__ == '__main__':
    main()
