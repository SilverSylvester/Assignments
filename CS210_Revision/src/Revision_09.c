#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#define MAX_SIZE 20

struct Queue
{
    int first;
    int last;
    char *queue[MAX_SIZE];
}; typedef struct Queue Queue;

Queue *queue_init()
{
    Queue *q = malloc(sizeof(Queue));
    assert(q != NULL);

    q->first = 0;
    q->last = -1;
    return q;
}

void enq(char *str, Queue *q)
{
    assert(q->last < MAX_SIZE);
    q->queue[++q->last] = str;
}

char *deq(Queue *q)
{
    assert(q->last > -1);
    return q->queue[q->first++];
}

char *peek_first(Queue *q)
{
    return q->queue[q->first];
}

char *peek_last(Queue *q)
{
    return q->queue[q->last];
}

bool is_empty(Queue *q)
{
    return q->last < q->first;
}

int main(int argc, char *argv[])
{
    Queue *q = queue_init();
    char *cmd, *in;
    
    for (int i = 0; i < 20; i++) {
        cmd = malloc(7);
        scanf("%s", cmd);
        if (strcmp("INSERT", cmd) == 0) {
            in = malloc(21);
            scanf("%s", in);
            enq(in, q);
            /* Problem was this line: `free(in)`.
             * Since I was passing the queue a *pointer* to
             * the data stored at `in`, when free(in) was
             * called, it was destroying that reference,
             * bascially obliterating the data. Calling
             * free was unnecessary anyway since it's freed
             * on free(q) (I think). */
        }
        else if (strcmp("REMOVE", cmd) == 0
                && !is_empty(q)) {
            deq(q);
        }
        free(cmd);
    }

    if (is_empty(q)) printf("empty\n");
    else printf("%s\n", deq(q));
    
    free(q);

    return 0;
}

