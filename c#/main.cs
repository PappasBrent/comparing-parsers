using System;
using System.IO;
using System.Collections.Generic;

namespace ArithParser
{
    enum TokenType
    {
        ADD, SUB, MUL, DIV, LPAREN, RPAREN, INT
    }

    class Token
    {
        private TokenType ty_;
        private int val_;

        public Token(TokenType ty, int val) {
            ty_ = ty;
            val_ = val;
        }

        public TokenType getTy() {
            return ty_;
        }

        public int getVal() {
            return val_;
        }
    }
    
    class Lexer
    {    
        public static List<Token> lex(string s) {
            List<Token> toks = new List<Token>();
            int i = 0;
            while (i < s.Length)
            {
                if (Char.IsWhiteSpace(s[i])) {}
                else if (s[i] == '+') { toks.Add(new Token(TokenType.ADD, 0)); }
                else if (s[i] == '-') { toks.Add(new Token(TokenType.SUB, 0)); }
                else if (s[i] == '*') { toks.Add(new Token(TokenType.MUL, 0)); }
                else if (s[i] == '/') { toks.Add(new Token(TokenType.DIV, 0)); }
                else if (s[i] == '(') { toks.Add(new Token(TokenType.LPAREN, 0)); }
                else if (s[i] == ')') { toks.Add(new Token(TokenType.RPAREN, 0)); }
                else if (Char.IsDigit(s[i])) {
                    int n = (int)Char.GetNumericValue(s[i]);
                    while (i + 1 < s.Length && Char.IsDigit(s[i + 1]))
                    {
                        n *= 10;
                        n += (int)Char.GetNumericValue(s[i + 1]);
                        i++;
                    }
                    toks.Add(new Token(TokenType.INT, n));
                }

                i++;
            }

            return toks;
        }
    }

    class Parser
    {
        private int i_;
        private List<Token> toks_;

        public Parser() {
            i_ = -1;
            toks_ = null;
        }

        public void reset() {
            i_ = -1;
            toks_ = null;
        }

        public void setToks(List<Token> toks) { toks_ = toks; }

        private void advance() { i_++; }

        private bool accept(TokenType ty) {
            if (i_ + 1 < toks_.Count && toks_[i_ + 1].getTy() == ty) {
                advance();
                return true;
            }
            return false;
        }

        private void expect(TokenType ty) {
            if (!accept(ty)) {
                throw new Exception("unexpected token");
            }
        }

        private void expectEnd() {
            if (i_ + 1 < toks_.Count) {
                throw new Exception("expected end of token stream");
            }
        }

        public int parse() {
            int n = addsub();
            expectEnd();
            return n;
        }

        private int addsub() {
            int n = muldiv();
            while (accept(TokenType.ADD) || accept(TokenType.SUB)) {
                if (toks_[i_].getTy() == TokenType.ADD) {
                    n += muldiv();
                } else if (toks_[i_].getTy() == TokenType.SUB) {
                    n -= muldiv();
                }
            }
            return n;
        }

        private int muldiv() {
            int n = neg();
            while (accept(TokenType.MUL) || accept(TokenType.DIV)) {
                if (toks_[i_].getTy() == TokenType.MUL) {
                    n *= neg();
                } else if (toks_[i_].getTy() == TokenType.DIV) {
                    n /= neg();
                }
            }
            return n;
        }

        private int neg() {
            int parity = 1;
            while (accept(TokenType.SUB))
            {
                parity *= -1;
            }
            return parity * parenint();
        }

        private int parenint() {
            if (accept(TokenType.LPAREN)) {
                int n = addsub();
                expect(TokenType.RPAREN);
                return n;
            }
            expect(TokenType.INT);
            return toks_[i_].getVal();
        }

    }


    class ArithParser {
        static void Main(string[] args) {
            Parser p = new Parser();
            // read from stdin
            if (args.Length == 0) {
                for (string s = Console.ReadLine(); s != null; s = Console.ReadLine())
                {
                    p.reset();
                    List<Token> toks = Lexer.lex(s);
                    p.setToks(toks);
                    int n = p.parse();
                    Console.WriteLine(n);
                }
            } else {
                // read from a file
                using(StreamReader file = new StreamReader(args[0])) {  
                    string line;
                    while ((line = file.ReadLine()) != null) {  
                        p.reset();
                        List<Token> toks = Lexer.lex(line);
                        p.setToks(toks);
                        int n = p.parse();
                        Console.WriteLine(n);
                    }  
                    file.Close();  
                } 
            }
        }
    }
}