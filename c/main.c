#include <assert.h>
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
    struct Token *t = malloc(sizeof(struct Token));
    assert(t != NULL && "sym token allocation failed");

    t->ty = ty;
    t->val = 0;
    return t;
}

struct Token *newIntToken(int val)
{
    struct Token *t = malloc(sizeof(struct Token));
    assert(t != NULL && "int token allocation failed");

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

// advances the lexer's input, consuming it in the process
int lexer_advance(struct Lexer *lexer)
{
    assert(lexer != NULL && "lexer is NULL\n");

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
    assert(lexer != NULL && "lexer allocation failed");

    lexer->input = input;
    lexer->cur = '\0';
    lexer->next = '\0';

    // prime the lexer
    lexer_advance(lexer);
    lexer_advance(lexer);

    return lexer;
}

// a dynamically-sized list of tokens
struct TokenList
{
    size_t size;
    size_t capacity;
    struct Token **list;
};

struct TokenList *newTokenList(size_t capacity)
{
    struct TokenList *tl = malloc(sizeof(struct TokenList));
    assert(tl != NULL && "token list allocation failed");

    tl->size = 0;
    tl->capacity = capacity;
    tl->list = calloc(tl->capacity, sizeof(struct Token *));
    assert(tl->list != NULL && "token list list allocation failed");

    return tl;
}

void freeTokenList(struct TokenList *tl)
{
    assert(tl != NULL && "tried to free a NULL token list");

    for (size_t i = 0; i < tl->size; i++)
    {
        free(tl->list[i]);
    }

    free(tl->list);
    free(tl);
}

void addToken(struct TokenList *tl, struct Token *t)
{
    assert(tl != NULL && "tried to add to a NULL token list");

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
    assert(lexer != NULL && "tried to lex with a NULL lexer");

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
            assert(!"unexpected token");
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

void interpreter_advance(struct Interpreter *inter)
{
    assert(inter != NULL && "tried to advance a NULL interpreter");
    assert(inter->tl != NULL && "interpreter token list is NULL");
    assert(inter->tl->list != NULL && "interpreter token list list is NULL");

    inter->cur = inter->next;
    if (inter->i < inter->tl->size)
    {
        // advance token
        inter->next = inter->tl->list[inter->i];
        inter->i++;
    }
    else
    {
        // no more tokens left?
        inter->next = NULL;
    }
}

int interpreter_accept(struct Interpreter *inter, enum TokenType *ty)
{
    assert(inter != NULL && "tried to accept from a NULL interpreter");

    if (inter->next == NULL && ty == NULL)
    {
        // are we checking that we've reached the end of the input?
        return 1;
    }
    else if (inter->next && ty && inter->next->ty == *ty)
    {
        // are we checking that the next token has the expected type?
        interpreter_advance(inter);
        return 1;
    }
    return 0;
}

void interpreter_expect(struct Interpreter *inter, enum TokenType *ty)
{
    assert(inter != NULL && "tried to expect from a NULL interpreter");

    if (!interpreter_accept(inter, ty))
    {
        assert(!"expect failed");
    }
}

struct Interpreter *newInterpreter(struct TokenList *tl)
{
    struct Interpreter *inter = malloc(sizeof(struct Interpreter));
    assert(inter != NULL && "interpreter allocation failed");

    inter->i = 0;
    inter->cur = NULL;
    inter->next = NULL;
    inter->tl = tl;

    // prime the interpreter
    interpreter_advance(inter);

    return inter;
}

int addop(struct Interpreter *inter);
int factor(struct Interpreter *inter)
{
    enum TokenType expected = LPAREN;
    if (interpreter_accept(inter, &expected))
    {
        int n = addop(inter);
        expected = RPAREN;
        interpreter_expect(inter, &expected);
        return n;
    }
    expected = INT;
    interpreter_expect(inter, &expected);
    return inter->cur->val;
}

int unop(struct Interpreter *inter)
{
    enum TokenType accepted = SUB;
    int parity = 1;
    while (interpreter_accept(inter, &accepted))
    {
        parity *= -1;
    }
    return parity * factor(inter);
}

int mulop(struct Interpreter *inter)
{
    enum TokenType accepted1 = MUL;
    enum TokenType accepted2 = DIV;
    int n = unop(inter);
    while (interpreter_accept(inter, &accepted1) ||
           interpreter_accept(inter, &accepted2))
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
    enum TokenType accepted1 = ADD;
    enum TokenType accepted2 = SUB;
    int n = mulop(inter);
    while (interpreter_accept(inter, &accepted1) ||
           interpreter_accept(inter, &accepted2))
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
    int n = addop(inter);
    interpreter_expect(inter, NULL);
    return n;
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
