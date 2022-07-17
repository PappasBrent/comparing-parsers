#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

enum Constants
{
    BUFFER_LEN = 256,
    INIT_TOKEN_CAP = 256,
};

enum TokenType
{
    ADD,
    SUB,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    INT
};

struct Token
{
    enum TokenType ty;

    // only valid if ty == INT
    int val;
};

// a dynamically-sized list of tokens
struct TokenList
{
    size_t size;
    size_t capacity;
    struct Token *list;
};

struct TokenList *newTokenList(size_t capacity)
{
    struct TokenList *tl = malloc(sizeof(struct TokenList));
    // assert(tl != NULL && "token list allocation failed");

    tl->size = 0;
    tl->capacity = capacity;
    tl->list = malloc(tl->capacity * sizeof(struct Token));
    // assert(tl->list != NULL && "token list list allocation failed");

    return tl;
}

void freeTokenList(struct TokenList *tl)
{
    // assert(tl != NULL && "tried to free a NULL token list");
    free(tl->list);
    free(tl);
}

void addToken(struct TokenList *tl, struct Token t)
{
    // assert(tl != NULL && "tried to add to a NULL token list");

    while (tl->size >= tl->capacity)
    {
        if (tl->capacity == 0)
        {
            tl->capacity = 1;
        }
        tl->capacity *= 2;
        tl->list = realloc(tl->list, sizeof(struct Token) * tl->capacity);
    }

    tl->list[tl->size] = t;
    tl->size++;
}

int ctoi(char c)
{
    return c - '0';
}

struct TokenList *lex(char *s)
{
    // assert(lexer != NULL && "tried to lex with a NULL lexer");
    struct TokenList *tl = newTokenList(INIT_TOKEN_CAP);
    int i = 0;
    int len = strlen(s);

    while (i < len)
    {
        if (s[i] == '+')
        {
            addToken(tl, (struct Token){.ty = ADD, .val = 0});
        }
        else if (s[i] == '-')
        {
            addToken(tl, (struct Token){.ty = SUB, .val = 0});
        }
        else if (s[i] == '*')
        {
            addToken(tl, (struct Token){.ty = MUL, .val = 0});
        }
        else if (s[i] == '/')
        {
            addToken(tl, (struct Token){.ty = DIV, .val = 0});
        }
        else if (s[i] == '(')
        {
            addToken(tl, (struct Token){.ty = LPAREN, .val = 0});
        }
        else if (s[i] == ')')
        {
            addToken(tl, (struct Token){.ty = RPAREN, .val = 0});
        }
        else if (isdigit(s[i]))
        {
            int n = ctoi(s[i]);
            while (i + 1 < len && isdigit(s[i + 1]))
            {
                n *= 10;
                n += ctoi(s[i + 1]);
                i += 1;
            }
            addToken(tl, (struct Token){.ty = INT, .val = n});
        }
        else if (isspace(s[i]))
        {
            // do nothing
        }
        else
        {
            assert(!"unexpected token");
        }
        i += 1;
    }
    return tl;
}

struct Parser
{
    struct TokenList *tl;
    size_t i;
};

void parser_advance(struct Parser *p)
{
    // assert(p != NULL && "tried to advance a NULL parser");
    // assert(p->tl != NULL && "parser token list is NULL");
    // assert(p->tl->list != NULL && "parser token list list is NULL");
    p->i++;
}

int parser_accept(struct Parser *p, enum TokenType ty)
{
    // assert(p != NULL && "tried to accept from a NULL parser");

    if ((p->i + 1 < p->tl->size) &&
        (p->tl->list[p->i + 1].ty == ty))
    {
        parser_advance(p);
        return 1;
    }
    return 0;
}

void parser_expect_end(struct Parser *p)
{
    // assert(p != NULL && "tried to expect end from a NULL parser");
    assert(p->i + 1 >= p->tl->size);
}

void parser_expect(struct Parser *p, enum TokenType ty)
{
    // assert(p != NULL && "tried to expect from a NULL parser");

    if (!parser_accept(p, ty))
    {
        assert(!"expect failed");
    }
}

void resetParser(struct Parser *p)
{
    p->i = -1;
    p->tl = NULL;
}

int addsub(struct Parser *p);
int parenint(struct Parser *p)
{
    if (parser_accept(p, LPAREN))
    {
        int n = addsub(p);
        parser_expect(p, RPAREN);
        return n;
    }
    parser_expect(p, INT);
    return p->tl->list[p->i].val;
}

int neg(struct Parser *p)
{
    enum TokenType accepted = SUB;
    int parity = 1;
    while (parser_accept(p, accepted))
    {
        parity *= -1;
    }
    return parity * parenint(p);
}

int muldiv(struct Parser *p)
{
    int n = neg(p);
    while (parser_accept(p, MUL) ||
           parser_accept(p, DIV))
    {
        if (p->tl->list[p->i].ty == MUL)
        {
            n *= neg(p);
        }
        else if (p->tl->list[p->i].ty == DIV)
        {
            n /= neg(p);
        }
    }
    return n;
}

int addsub(struct Parser *p)
{
    int n = muldiv(p);
    while (parser_accept(p, ADD) ||
           parser_accept(p, SUB))
    {
        if (p->tl->list[p->i].ty == ADD)
        {
            n += muldiv(p);
        }
        else if (p->tl->list[p->i].ty == SUB)
        {
            n -= muldiv(p);
        }
    }
    return n;
}

int parse(struct Parser *p)
{
    int n = addsub(p);
    parser_expect_end(p);
    return n;
}

int main(int argc, char const *argv[])
{
    char buffer[BUFFER_LEN] = {'\0'};
    struct Parser p = {.i = -1, .tl = NULL};

    while (fgets(buffer, BUFFER_LEN, stdin))
    {
        resetParser(&p);
        struct TokenList *tl = lex(buffer);
        p.tl = tl;

        int result = parse(&p);
        printf("%d\n", result);

        freeTokenList(tl);

        for (size_t i = 0; i < BUFFER_LEN; i++)
        {
            buffer[i] = '\0';
        }
    }

    return 0;
}
