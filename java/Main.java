import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;


public class Main {

    static public enum TokenType {
        ADD, SUB, MUL, DIV, LPAREN, RPAREN, INT
    }

    static class Token {
        private TokenType ty_;
        private int val_;

        public Token(TokenType ty, int val) {
            ty_ = ty;
            val_ = val;
        }

        public TokenType getTy() { return ty_; }
        public int getVal() { return val_; }
    }

    static public List<Token> lex(String s) throws Exception {
        List<Token> toks = new ArrayList<Token>();
        int i = 0;

        while (i < s.length()) {
            if (Character.isWhitespace(s.charAt(i))) {}
            else if (s.charAt(i) == '+') { toks.add(new Token(TokenType.ADD, 0)); }
            else if (s.charAt(i) == '-') { toks.add(new Token(TokenType.SUB, 0)); }
            else if (s.charAt(i) == '*') { toks.add(new Token(TokenType.MUL, 0)); }
            else if (s.charAt(i) == '/') { toks.add(new Token(TokenType.DIV, 0)); }
            else if (s.charAt(i) == '(') { toks.add(new Token(TokenType.LPAREN, 0)); }
            else if (s.charAt(i) == ')') { toks.add(new Token(TokenType.RPAREN, 0)); }
            else if (Character.isDigit(s.charAt(i))) {
                int n = Character.getNumericValue(s.charAt(i));
                while (i + 1 < s.length() && Character.isDigit(s.charAt(i + 1))) {
                    n *= 10;
                    n += Character.getNumericValue(s.charAt(i + 1));
                    i++;
                }
                toks.add(new Token(TokenType.INT, n));
            } else {
                throw new Exception("unexpected char");
            }
            i++;
        }

        return toks;
    }

    static class Parser {
        private int i_;
        private List<Token> toks_;

        public Parser() { i_ = -1; toks_ = null; }
        public void setToks(List<Token> toks) { toks_ = toks; }
        public void reset() { i_ = -1; toks_ = null; }

        private void advance() { i_++; }
        private boolean accept(TokenType ty) {
            if (i_ + 1 < toks_.size() && toks_.get(i_ + 1).getTy() == ty) {
                advance();
                return true;
            }
            return false;
        }
        private void expect(TokenType ty) throws Exception {
            if (!accept(ty)) {
                throw new Exception("unexpected token");
            }
        }
        private void expectEnd() throws Exception {
            if (i_ + 1 < toks_.size()) {
                throw new Exception("expected end of token stream");
            }
        }
        
        public int parse() throws Exception {
            int n = addsub();
            expectEnd();
            return n;
        }

        private int addsub() throws Exception {
            int n = muldiv();
            while (accept(TokenType.ADD) || accept(TokenType.SUB)) {
                if (toks_.get(i_).getTy() == TokenType.ADD) {
                    n += muldiv();
                } else if (toks_.get(i_).getTy() == TokenType.SUB) {
                    n -= muldiv();
                }
            }
            return n;
        }

        private int muldiv() throws Exception {
            int n = neg();
            while (accept(TokenType.MUL) || accept(TokenType.DIV)) {
                if (toks_.get(i_).getTy() == TokenType.MUL) {
                    n *= neg();
                } else if (toks_.get(i_).getTy() == TokenType.DIV) {
                    n /= neg();
                }
            }
            return n;
        }

        private int neg() throws Exception {
            int parity = 1;
            while (accept(TokenType.SUB)) {
                parity *= -1;
            }
            return parity * parenint();
        }

        private int parenint() throws Exception {
            if (accept(TokenType.LPAREN)) {
                int n = addsub();
                expect(TokenType.RPAREN);
                return n;
            }
            expect(TokenType.INT);
            return toks_.get(i_).getVal();
        }
    }


    public static void main(String[] args) throws Exception {
        Parser p = new Parser();
        Scanner input = null;
        // read from stdin
        if (args.length == 0) {
            input = new Scanner(System.in);
        }
        // read from file
        else {
            input = new Scanner(new File(args[0]));
        }
        while (input.hasNextLine()) {
            p.reset();
            p.setToks(lex(input.nextLine()));
            int n = p.parse();
            System.out.println(n);
        }
    }
}