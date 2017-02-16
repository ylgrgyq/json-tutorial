#include "leptjson.h"
#include <assert.h>  /* assert() */
#include <stdlib.h>  /* NULL, malloc(), realloc(), free(), strtod() */
#include <errno.h>   /* errno, ERANGE */
#include <math.h>    /* HUGE_VAL */
#include <string.h>  /* memcpy */

#define EXPECT(c, ch)       do { assert(*c->json == (ch)); c->json++; } while(0)
#define ISDIGIT(ch)         ((ch) >= '0' && (ch) <= '9')
#define ISDIGIT1TO9(ch)     ((ch) >= '1' && (ch) <= '9')
#define PUSHC(c, ch) do {*(char*)lept_context_push(c, sizeof(char)) = ch;} while(0)

#ifndef LEPT_PARSE_STACK_INIT_SIZE
#define LEPT_PARSE_STACK_INIT_SIZE 256
#endif

typedef struct {
    const char* json;
    char* stack;
    size_t top, size;
}lept_context;

static void* lept_context_push(lept_context* c, size_t size) {
    void* ret;
    assert (size > 0);

    if (c->top + size > c->size) {
        if (c->size == 0) {
            c->size = LEPT_PARSE_STACK_INIT_SIZE;
        }

        while (c->top + size >= c->size) {
            c->size += c->size >> 1;
        }

        c->stack = (char*)realloc(c->stack, c->size);
    }

    ret = c->stack + c->top;
    c->top += size;
    return ret;
}

static void* lept_context_pop(lept_context* c, size_t size) {
    assert (size >= 0);
    c->top -= size;
    return c->stack + c->top;
}

void lept_free(lept_value* v) {
    assert(v != NULL);
    if (v->type == LEPT_STRING) {
        free(v->u.s.s);
    }
    v->type = LEPT_NULL;
}

lept_type lept_get_type(const lept_value* v) {
    assert(v != NULL);
    return v->type;
}

double lept_get_number(const lept_value* v) {
    assert(v != NULL && v->type == LEPT_NUMBER);
    return v->u.n;
}

int lept_get_boolean(const lept_value* v) {
    assert(v != NULL && (v->type == LEPT_TRUE || v->type == LEPT_FALSE));

    return v->type == LEPT_TRUE ? 1 : 0;
}

void lept_set_boolean(lept_value* v, int b) {
    assert(v != NULL);

    lept_free(v);
    v->type = b == 0 ? LEPT_FALSE : LEPT_TRUE;
}

void lept_set_number(lept_value* v, double n) {
    assert(v != NULL);

    lept_free(v);
    v->u.n = n;
    v->type = LEPT_NUMBER;
}

void lept_set_string(lept_value* v, const char* str, size_t len) {
    assert(v != NULL && (str != NULL || len == 0));

    lept_free(v);
    v->u.s.s = (char*)malloc(len + 1);
    memcpy(v->u.s.s, str, len);
    v->u.s.s[len] = '\0';
    v->u.s.len = len;
    v->type = LEPT_STRING;
}

char* lept_get_string(const lept_value* v) {
    assert(v != NULL && v->type == LEPT_STRING);
    return v->u.s.s;
}

int lept_get_string_length(const lept_value* v) {
    assert(v != NULL && v->type == LEPT_STRING);
    return v->u.s.len;
}

static void lept_parse_whitespace(lept_context* c) {
    const char *p = c->json;
    while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')
        p++;
    c->json = p;
}

static int lept_parse_literal(lept_context* c, lept_value* v, char* literal, int t){
    size_t i;
    EXPECT(c, literal[0]);

    for (i = 1; i < strlen(literal); ++i) {
        if (c->json[i - 1] != literal[i]) {
            return LEPT_PARSE_INVALID_VALUE;
        }
    }
    
    c->json += i - 1; 
    v->type = t;
    return LEPT_PARSE_OK;
}

static int lept_parse_number(lept_context* c, lept_value* v) {
    const char* p = c->json;
    double n;
    if (*p == '-') ++p;
    if (*p == '0') ++p;
    else {
        if (ISDIGIT1TO9(*p)) for (++p; ISDIGIT(*p); ++p);
        else return LEPT_PARSE_INVALID_VALUE;
    }

    if (*p == '.') {
        ++p;
        if (ISDIGIT(*p)) for (++p; ISDIGIT(*p); ++p);
        else return LEPT_PARSE_INVALID_VALUE;
    }

    if (*p == 'e' || *p == 'E') {
        ++p;
        if (*p == '+' || *p == '-') ++p;
        if (ISDIGIT(*p)) for (++p; ISDIGIT(*p); ++p);
        else return LEPT_PARSE_INVALID_VALUE;
    } 

    errno = 0;
    n = strtod(c->json, NULL);
    if (errno == ERANGE && (n == HUGE_VAL || n == -HUGE_VAL)) {
        return LEPT_PARSE_NUMBER_TOO_BIG;
    }
    c->json = p;
    lept_set_number(v, n);
    
    return LEPT_PARSE_OK;
}

static int lept_parse_string(lept_context* c, lept_value* v) {
    const char* p;
    size_t head = c->top, size;
    EXPECT(c, '"');
    
    p = c->json;
    for (;;) {
        char ch = *p++;
        
        switch (ch) {
            case '\"': 
                size = c->top - head;
                lept_set_string(v, lept_context_pop(c, size), size);
                c->json = p;
                return LEPT_PARSE_OK;
            case '\0': 
                c->top = head;
                return LEPT_PARSE_MISS_QUOTATION_MARK;
            case '\\':
                switch (*p++) {
                    case '\"': PUSHC(c, '\"'); break;
                    case '\\': PUSHC(c, '\\'); break;
                    case 'n': PUSHC(c, '\n'); break;
                    case 'b': PUSHC(c, '\b'); break;
                    case 'r': PUSHC(c, '\r'); break;
                    case 't': PUSHC(c, '\t'); break;
                    case 'f': PUSHC(c, '\f'); break;
                    case '/': PUSHC(c, '/'); break;
                    default: 
                        c->top = head;
                        return LEPT_PARSE_INVALID_STRING_ESCAPE;
                }
                break;
            default: 
                if ((unsigned char)ch < 0x20) { 
                    c->top = head;
                    return LEPT_PARSE_INVALID_STRING_CHAR;
                }
                PUSHC(c, ch);
        }
    }
}

static int lept_parse_value(lept_context* c, lept_value* v) {
    switch (*c->json) {
        case 't':  return lept_parse_literal(c, v, "true", LEPT_TRUE);
        case 'f':  return lept_parse_literal(c, v, "false", LEPT_FALSE);
        case 'n':  return lept_parse_literal(c, v, "null", LEPT_NULL);
        case '"': return lept_parse_string(c, v);
        default:   return lept_parse_number(c, v);
        case '\0': return LEPT_PARSE_EXPECT_VALUE;
    }
}

int lept_parse(lept_value* v, const char* json) {
    int ret;
    lept_context c;
    c.stack = NULL;
    c.top = c.size = 0;
    
    assert(v != NULL);
    c.json = json;
    lept_init(v);
    lept_parse_whitespace(&c);
    if ((ret = lept_parse_value(&c, v)) == LEPT_PARSE_OK) {
        lept_parse_whitespace(&c);
        if (*c.json != '\0') {
            v->type = LEPT_NULL;
            ret = LEPT_PARSE_ROOT_NOT_SINGULAR;
        }
    }

    assert(c.top == 0);
    free(c.stack);
    return ret;
}
