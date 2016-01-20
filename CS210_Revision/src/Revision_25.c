#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define MAX_STK_SIZE 100

struct Stack
{
    int head;
    int size;
    char *stack;
};

typedef struct Stack Stack;

Stack *stack_init(int size)
{
    Stack *s = malloc(sizeof(Stack));
    assert(s != NULL);

    s->size = size;
    s->stack = malloc(size * sizeof(char));
    assert(s->stack != NULL);

    s->head = -1;
    return s;
}

void stack_destruct(Stack *s)
{
    free(s->stack);
    free(s);
}

void stack_realloc(Stack *s)
{
    s->size *= 2;
    char *new_stack = realloc(s->stack, s->size * sizeof(char));
    s->stack = new_stack;
    assert(s->stack != NULL);
}

void push(char c, Stack *s)
{
    if ((s->size - 1) <= s->head)
        stack_realloc(s);

    s->stack[++s->head] = c;
}

char pop(Stack *s)
{
    assert(s->head != -1);
    return s->stack[s->head--];
}

bool is_empty(Stack *s)
{
    return s->head == -1;
}

/* ------------------- */

bool is_palin(char *str)
{
    Stack *rts = stack_init(MAX_STK_SIZE);
    for (int i = 0; i < strlen(str); i++) {
        push(str[i], rts);
    }

    int i = 0; bool b = true;
    while (!is_empty(rts)) {
        if (pop(rts) != str[i++])
            b = false; break;
    }

    stack_destruct(rts);
    return b;
}

void format_string(char *s)
{
    // Removing whitespace
    char *i = s;
    char *j = s;
    while (*j) {
        *i = *j++;
        if(*i != ' ') i++;
    }
    *i = 0;

    // Convert to lower case
    for ( ; *s; s++) *s = tolower(*s);
}

int main(int argc, char *argv[])
{
    char *s = malloc(MAX_STK_SIZE * sizeof(char));
    fgets(s, MAX_STK_SIZE, stdin);
    format_string(s);

    if (is_palin(s)) printf("TRUE\n");
    else printf("FALSE\n");

    free(s);
}
