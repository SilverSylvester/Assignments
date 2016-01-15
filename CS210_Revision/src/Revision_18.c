#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STRLEN 25

void merge(char **ss, char **ls, char **rs, int lc, int rc)
{
    int i = 0, j = 0, k = 0;

    while (i < lc && j < rc) {
        if (strlen(ls[i]) == strlen(rs[j])) {
            if (strcmp(ls[i], rs[j]) < 0) {
                strcpy(ss[k++], ls[i++]);
            }
            else {
                strcpy(ss[k++], rs[j++]);
            }
        }
        else if (strlen(ls[i]) < strlen(rs[j])) {
            strcpy(ss[k++], ls[i++]);
        }
        else {
            strcpy(ss[k++], rs[j++]);
        }
    }
    while (i < lc) strcpy(ss[k++], ls[i++]);
    while (j < rc) strcpy(ss[k++], rs[j++]);
}

void mergesort(char **ss, int len)
{
    if (len < 2) return;
    int mid = len / 2;

    char **ls = malloc(mid * sizeof(char *));
    char **rs = malloc((len - mid) * sizeof(char *));

    for (int i = 0; i < mid; i++) {
        ls[i] = malloc(MAX_STRLEN * sizeof(char));
        strcpy(ls[i], ss[i]);
    }
    
    for (int i = mid; i < len; i++) {
        rs[i - mid] = malloc(MAX_STRLEN * sizeof(char));
        strcpy(rs[i - mid], ss[i]);
    }

    mergesort(ls, mid); mergesort(rs, len - mid);
    merge(ss, ls, rs, mid, len - mid);

    for (int i = 0; i < mid; i++) free(ls[i]);
    for (int i = 0; i < (len - mid); i++) free(rs[i]);
    free(ls); free(rs);
}

int main(int argc, char *argv[])
{
    int n; scanf("%d", &n);
    char **ss = malloc(n * sizeof(char *));

    for (int i = 0; i < n; i++) {
        ss[i] = malloc(MAX_STRLEN * sizeof(char));
        scanf("%s", ss[i]);
    }

    mergesort(ss, n);

    for (int i = 0; i < n; i++) {
        printf("%s\n", ss[i]);
    }

    for (int i = 0; i < n; i++) free(ss[i]);
    free(ss);
}

