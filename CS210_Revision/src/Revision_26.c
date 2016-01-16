#include <stdio.h>
#include <stdlib.h>

int comparison(const void *a, const void *b)
{
    return (*(int *) b - *(int *) a);
}

int main(int argc, char *argv[])
{
    int n; scanf("%d", &n);
    int *ns = malloc(n * sizeof(int));
    
    for (int i = 0; i < n; i++) {
        scanf("%d", &ns[i]);
    }
    
    int x; scanf("%d", &x);
    
    qsort(ns, n, sizeof(*ns), comparison);
    
    printf("%d\n", ns[x - 1]);
    free(ns);
}

