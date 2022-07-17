#include <string>
#include <iostream>
#include <vector>
#include <assert.h>
#include <ctype.h>

enum TokenType
{
    ADD,
    SUB,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    INT,
};

struct Token
{
    TokenType ty;

    // only valid if ty == INT
    int val;
};

int ctoi(char c) { return c - '0'; }

std::vector<Token> lex(std::string s)
{
    std::vector<Token> toks;
    std::size_t i = 0;
    while (i < s.length())
    {
        if (isspace(s[i]))
        {
        }
        else if (s[i] == '+')
        {
            toks.push_back(Token{.ty = ADD});
        }
        else if (s[i] == '-')
        {
            toks.push_back(Token{.ty = SUB});
        }
        else if (s[i] == '*')
        {
            toks.push_back(Token{.ty = MUL});
        }
        else if (s[i] == '/')
        {
            toks.push_back(Token{.ty = DIV});
        }
        else if (s[i] == '(')
        {
            toks.push_back(Token{.ty = LPAREN});
        }
        else if (s[i] == ')')
        {
            toks.push_back(Token{.ty = RPAREN});
        }
        else if (isdigit(s[i]))
        {
            int n = ctoi(s[i]);
            while (i + 1 < s.length() && isdigit(s[i + 1]))
            {
                n *= 10;
                n += ctoi(s[i + 1]);
                i++;
            }
            toks.push_back(Token{.ty = INT, .val = n});
        }
        else
        {
            assert(!"unexpected char");
        }
        i++;
    }
    return toks;
}

class Parser
{
private:
    int i_;
    std::vector<Token> &toks_;

public:
    Parser(std::vector<Token> &);

    void advance();
    bool accept(TokenType ty);
    void expect(TokenType ty);
    void expectEnd();

    int parse();
    int addsub();
    int muldiv();
    int neg();
    int parenint();
};

Parser::Parser(std::vector<Token> &toks) : i_(-1), toks_(toks) {}

void Parser::advance() { i_++; }
bool Parser::accept(TokenType ty)
{
    if (i_ + 1 < toks_.size() && toks_[i_ + 1].ty == ty)
    {
        advance();
        return true;
    }
    return false;
}

void Parser::expect(TokenType ty)
{
    if (!accept(ty))
    {
        assert(!"unexpected token");
    }
}

void Parser::expectEnd()
{
    if (i_ + 1 < toks_.size())
    {
        assert(!"expected end of token stream");
    }
}

int Parser::parse()
{
    int n = addsub();
    expectEnd();
    return n;
}

int Parser::addsub()
{
    int n = muldiv();
    while (accept(ADD) || accept(SUB))
    {
        if (toks_[i_].ty == ADD)
        {
            n += muldiv();
        }
        else if (toks_[i_].ty == SUB)
        {
            n -= muldiv();
        }
    }
    return n;
}

int Parser::muldiv()
{
    int n = neg();
    while (accept(MUL) || accept(DIV))
    {
        if (toks_[i_].ty == MUL)
        {
            n *= neg();
        }
        else if (toks_[i_].ty == DIV)
        {
            n /= neg();
        }
    }
    return n;
}

int Parser::neg()
{
    int parity = 1;
    while (accept(SUB))
    {
        parity *= -1;
    }
    return parity * parenint();
}

int Parser::parenint()
{
    if (accept(LPAREN))
    {
        int n = addsub();
        expect(RPAREN);
        return n;
    }
    expect(INT);
    return toks_[i_].val;
}

int main(int argc, char const *argv[])
{
    std::string line;
    while (std::getline(std::cin, line))
    {
        auto toks = lex(line);
        Parser p(toks);
        int n = p.parse();
        std::cout << n << "\n";
    }

    return 0;
}
