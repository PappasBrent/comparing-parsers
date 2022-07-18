import * as readline from 'readline';

const RL = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

enum TokenType {
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    RParen,
    Int
};

class Token {
    private ty_: TokenType
    private val_: number

    constructor(ty: TokenType, val: number) {
        this.ty_ = ty;
        this.val_ = val;
    }


    public get ty(): TokenType {
        return this.ty_;
    }

    public get val(): number {
        return this.val_;
    }
}

function isNumeric(s: string): boolean {
    return !isNaN(parseFloat(s)) && isFinite(parseFloat(s));
}

function lex(s: string): Token[] {
    let toks: Token[] = [];
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

    private i: number
    private toks_: Token[]

    constructor() {
        this.i = -1;
        this.toks_ = [];
    }

    public reset(): void {
        this.i = -1;
        this.toks_ = [];
    }

    public set toks(toks: Token[]) { this.toks_ = toks; }

    private advance(): void { this.i++; }
    private accept(ty: TokenType): boolean {
        if (this.i + 1 < this.toks_.length && this.toks_[this.i + 1].ty === ty) {
            this.advance();
            return true;
        }
        return false;
    }
    private expect(ty: TokenType): void {
        if (!this.accept(ty)) {
            throw new Error(`unexpected token, expected ${ty}`);
        }
    }
    private expectEnd(): void {
        if (this.i + 1 < this.toks_.length) {
            throw new Error('expected end of token stream');
        }
    }

    public parse(): number {
        const n = this.addsub();
        this.expectEnd();
        return n;
    }

    private addsub(): number {
        let n = this.muldiv();
        while (this.accept(TokenType.Add) || this.accept(TokenType.Sub)) {
            if (this.toks_[this.i].ty == TokenType.Add) {
                n += this.muldiv();
            } else if (this.toks_[this.i].ty == TokenType.Sub) {
                n -= this.muldiv();
            }
        }
        return n;
    }

    private muldiv(): number {
        let n = this.neg();
        while (this.accept(TokenType.Mul) || this.accept(TokenType.Div)) {
            if (this.toks_[this.i].ty == TokenType.Mul) {
                n *= this.neg();
            } else if (this.toks_[this.i].ty == TokenType.Div) {
                // use Math.trunc to round toward zero
                n = Math.trunc(n / this.neg());
            }
        }
        return n;
    }

    private neg(): number {
        let parity = 1;
        while (this.accept(TokenType.Sub)) {
            parity *= -1;
        }
        let n = parity * this.parenint();
        // convert -0 to 0
        return n || -0 || 0;
    }

    private parenint(): number {
        if (this.accept(TokenType.LParen)) {
            const n = this.addsub();
            this.expect(TokenType.RParen);
            return n;
        }
        this.expect(TokenType.Int);
        return this.toks_[this.i].val;
    }
}

const p = new Parser();
RL.on('line', (line) => {
    p.reset();
    p.toks = lex(line);
    const n = p.parse();
    console.log(n);
});