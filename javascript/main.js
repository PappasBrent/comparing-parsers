const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
});

const TokenType = {
    Add: 0,
    Sub: 1,
    Mul: 2,
    Div: 3,
    LParen: 4,
    RParen: 5,
    Int: 6
};

class Token {
    constructor(ty, val) {
        this.ty = ty;
        this.val = val;
    }
}

function isNumeric(s) {
    return !isNaN(parseFloat(s)) && isFinite(s);
}


function lex(s) {
    let toks = [];
    let i = 0;
    while (i < s.length) {
        if (s[i].trim() == '') { }
        else if (s[i] == '+') { toks.push(new Token(TokenType.Add, 0)); }
        else if (s[i] == '-') { toks.push(new Token(TokenType.Sub, 0)); }
        else if (s[i] == '*') { toks.push(new Token(TokenType.Mul, 0)); }
        else if (s[i] == '/') { toks.push(new Token(TokenType.Div, 0)); }
        else if (s[i] == '(') { toks.push(new Token(TokenType.LParen, 0)); }
        else if (s[i] == ')') { toks.push(new Token(TokenType.RParen, 0)); }
        else if (isNumeric(s[i])) {
            let n = parseInt(s[i]);
            while (isNumeric(s[i + 1])) {
                n *= 10;
                n += parseInt(s[i + 1]);
                i++;
            }
            toks.push(new Token(TokenType.Int, n));
        }
        i++;
    }
    return toks;
}


class Parser {
    constructor() {
        this.i = -1;
        this.toks = null;
    }

    reset() {
        this.i = -1;
        this.toks = null;
    }

    setToks(toks) { this.toks = toks; }

    advance() { this.i++; }
    accept(ty) {
        if (this.i + 1 < this.toks.length && this.toks[this.i + 1].ty === ty) {
            this.advance();
            return true;
        }
        return false;
    }
    expect(ty) {
        if (!this.accept(ty)) {
            throw `unexpected token, expected ${ty}`;
        }
    }
    expectEnd() {
        if (this.i + 1 < this.toks.length) {
            throw 'expected end of token stream';
        }
    }

    parse() {
        const n = this.addsub();
        this.expectEnd();
        return n;
    }

    addsub() {
        let n = this.muldiv();
        while (this.accept(TokenType.Add) || this.accept(TokenType.Sub)) {
            if (this.toks[this.i].ty == TokenType.Add) {
                n += this.muldiv();
            } else if (this.toks[this.i].ty == TokenType.Sub) {
                n -= this.muldiv();
            }
        }
        return n;
    }

    muldiv() {
        let n = this.neg();
        while (this.accept(TokenType.Mul) || this.accept(TokenType.Div)) {
            if (this.toks[this.i].ty == TokenType.Mul) {
                n *= this.neg();
            } else if (this.toks[this.i].ty == TokenType.Div) {
                // use Math.trunc to round toward zero
                n = Math.trunc(n / this.neg());
            }
        }
        return n;
    }

    neg() {
        let parity = 1;
        while (this.accept(TokenType.Sub)) {
            parity *= -1;
        }
        let n = parity * this.parenint();
        // convert -0 to 0
        return n || -0 || 0;
    }

    parenint() {
        if (this.accept(TokenType.LParen)) {
            const n = this.addsub();
            this.expect(TokenType.RParen);
            return n;
        }
        this.expect(TokenType.Int);
        return this.toks[this.i].val;
    }
}

const p = new Parser();
readline.on('line', (line) => {
    p.reset();
    p.setToks(lex(line));
    const n = p.parse();
    console.log(n);
});
