#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

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

struct Token *newSymToken(enum TokenType ty)
{
    struct Token *t = calloc(1, sizeof(struct Token));
    t->ty = ty;
    t->val = 0;
    return t;
}

struct Token *newIntToken(int val)
{
    struct Token *t = calloc(1, sizeof(struct Token));
    t->ty = INT;
    t->val = val;
    return t;
}

struct Lexer
{
    char *input;
    char cur;
    char next;
};

int lexer_advance(struct Lexer *lexer)
{
    if (lexer == NULL)
    {
        return 0;
    }

    if (lexer->input != NULL)
    {
        lexer->cur = lexer->next;
        lexer->next = *(lexer->input++);
        return 1;
    }
    return 0;
}

struct Lexer *newLexer(char *input)
{
    struct Lexer *lexer = malloc(sizeof(struct Lexer));
    lexer->input = input;
    lexer->cur = '\0';
    lexer->next = '\0';

    // prime the lexer
    lexer_advance(lexer);
    lexer_advance(lexer);

    return lexer;
}

struct TokenList
{
    size_t size;
    size_t capacity;
    struct Token **list;
};

struct TokenList *newTokenList(size_t capacity)
{
    struct TokenList *tl = calloc(1, sizeof(struct TokenList));
    tl->size = 0;
    tl->capacity = capacity;
    tl->list = calloc(tl->capacity, sizeof(struct Token *));
    return tl;
}

void freeTokenList(struct TokenList *tl)
{
    if (tl == NULL)
    {
        return;
    }

    for (size_t i = 0; i < tl->size; i++)
    {
        free(tl->list[i]);
    }

    free(tl->list);
    free(tl);
}

void addToken(struct TokenList *tl, struct Token *t)
{
    if (tl == NULL)
    {
        return;
    }

    while (tl->size >= tl->capacity)
    {
        if (tl->capacity == 0)
        {
            tl->capacity = 1;
        }
        tl->capacity *= 2;
        tl->list = realloc(tl->list, sizeof(struct Token *) * tl->capacity);
    }

    tl->list[tl->size] = t;
    tl->size++;
}

int ctoi(char c)
{
    return c - '0';
}

struct TokenList *lex(struct Lexer *lexer)
{
    if (lexer == NULL)
    {
        return NULL;
    }

    struct TokenList *tl = newTokenList(INIT_TOKEN_CAP);

    while (lexer->cur != '\0')
    {
        if (lexer->cur == '+')
        {
            addToken(tl, newSymToken(ADD));
        }
        else if (lexer->cur == '-')
        {
            addToken(tl, newSymToken(SUB));
        }
        else if (lexer->cur == '*')
        {
            addToken(tl, newSymToken(MUL));
        }
        else if (lexer->cur == '/')
        {
            addToken(tl, newSymToken(DIV));
        }
        else if (lexer->cur == '(')
        {
            addToken(tl, newSymToken(LPAREN));
        }
        else if (lexer->cur == ')')
        {
            addToken(tl, newSymToken(RPAREN));
        }
        else if (isdigit(lexer->cur))
        {
            int n = ctoi(lexer->cur);
            while (isdigit(lexer->next))
            {
                n *= 10;
                n += ctoi(lexer->next);
                lexer_advance(lexer);
            }
            addToken(tl, newIntToken(n));
        }
        else if (isspace(lexer->cur))
        {
            // do nothing
        }
        else
        {
            // TODO: throw an error here
        }
        lexer_advance(lexer);
    }
    return tl;
}

struct Interpreter
{
    struct TokenList *tl;
    size_t i;
    struct Token *cur;
    struct Token *next;
};

int interpreter_advance(struct Interpreter *inter)
{
    if (inter == NULL)
    {
        return 0;
    }

    if (inter->tl && inter->tl->list && (inter->i < inter->tl->size))
    {
        // advance token
        inter->cur = inter->next;
        inter->next = inter->tl->list[inter->i];
        inter->i++;
        return 1;
    }
    else
    {
        // no more tokens left?
        inter->cur = inter->next;
        inter->next = NULL;
        return 0;
    }

    return 0;
}

int interpreter_accept(struct Interpreter *inter, enum TokenType ty)
{
    if (inter && inter->next && inter->next->ty == ty)
    {
        return interpreter_advance(inter);
    }
    return 0;
}

struct Interpreter *newInterpreter(struct TokenList *tl)
{
    struct Interpreter *inter = calloc(1, sizeof(struct Interpreter));
    inter->i = 0;
    inter->cur = NULL;
    inter->next = NULL;
    inter->tl = tl;

    // prime the interpreter
    interpreter_advance(inter);

    return inter;
}

int addop(struct Interpreter *inter);
int term(struct Interpreter *inter)
{
    if (interpreter_accept(inter, LPAREN))
    {
        int n = addop(inter);
        // TODO: create interpreter_expect function
        interpreter_accept(inter, RPAREN); // eat right parenthesis
        return n;
    }
    // TODO: throw error if not int
    interpreter_accept(inter, INT);
    return inter->cur->val;
}

int unop(struct Interpreter *inter)
{
    int parity = 1;
    while (interpreter_accept(inter, SUB))
    {
        parity *= -1;
    }
    return parity * term(inter);
}

int mulop(struct Interpreter *inter)
{
    int n = unop(inter);
    while (interpreter_accept(inter, MUL) || interpreter_accept(inter, DIV))
    {
        if (inter->cur->ty == MUL)
        {
            n *= unop(inter);
        }
        else if (inter->cur->ty == DIV)
        {
            n /= unop(inter);
        }
    }
    return n;
}

int addop(struct Interpreter *inter)
{
    int n = mulop(inter);
    while (interpreter_accept(inter, ADD) || interpreter_accept(inter, SUB))
    {
        if (inter->cur->ty == ADD)
        {
            n += mulop(inter);
        }
        else if (inter->cur->ty == SUB)
        {
            n -= mulop(inter);
        }
    }
    return n;
}

int interpret(struct Interpreter *inter)
{
    return addop(inter);
}

int main(int argc, char const *argv[])
{
    char buffer[BUFFER_LEN] = {'\0'};

    while (fgets(buffer, BUFFER_LEN, stdin))
    {
        struct Lexer *lexer = newLexer(buffer);
        struct TokenList *tl = lex(lexer);
        struct Interpreter *inter = newInterpreter(tl);

        int result = interpret(inter);
        printf("%d\n", result);

        free(inter);
        freeTokenList(tl);
        free(lexer);

        for (size_t i = 0; i < BUFFER_LEN; i++)
        {
            buffer[i] = '\0';
        }
    }

    return 0;
}
