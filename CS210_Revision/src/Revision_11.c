#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

struct Link
{
    char *data;
    struct Link *next;
};

typedef struct Link Link;

Link *link_init()
{
    Link *link = malloc(sizeof(Link));
    assert(link != NULL);

    link->next = NULL;
    link->data = malloc(10 * sizeof(char));
}

int main(int argc, char *argv[])
{
    Link *list = link_init();
    int n; scanf("%d", &n); n--;

    Link *c = list; // Current pos in list

    /* Storing input to links */
    scanf("%s", c->data);
    while (n--) {
        c->next = link_init();
        c = c->next;
        scanf("%s", c->data);
    }

    c = list; // Resetting current to start
    while (c != NULL) {
        printf("%s\n", c->data);
        c = c->next;
    }

    return 0;
}

