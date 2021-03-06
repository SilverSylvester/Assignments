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

void queue_destruct(Queue *q)
{
    for (int i = 0; i < MAX_SIZE; i++) {
        free(q->queue[i]);
    }
    free(q);
}

int main(int argc, char *argv[])
{
    Queue *q = queue_init();
    char *cmd, *in;
    
    for (int i = 0; i < 20; i++) {
        cmd = calloc(7, sizeof(char));
        scanf("%s", cmd);
        if (strcmp("INSERT", cmd) == 0) {
            in = calloc(21, sizeof(char));
            scanf("%s", in);
            enq(in, q);
        }
        else if (strcmp("REMOVE", cmd) == 0
                && !is_empty(q)) {
            deq(q);
        }
        free(cmd);
    }

    if (is_empty(q)) printf("empty\n");
    else printf("%s\n", deq(q));
    
    queue_destruct(q);

    return 0;
}

