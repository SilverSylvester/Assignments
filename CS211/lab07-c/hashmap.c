#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

// 433099 for load factor greater than 0.5
// 240607 for load factor greater than 0.9
#define HT_MAX_SIZE 433099

// For murmur3 hashing function
#define SEED 65535

typedef unsigned long long u64;
typedef unsigned int u32;

static int collisions = 0;

/* ------------------------- */
//  HASHTABLE IMPLEMENTATION //
/* ------------------------- */

typedef struct hashtable {
    // Array of strings
    char **table;
    size_t size;
} hashtable;

hashtable *ht_init(size_t size) {
    printf("Initialising hashtable ... ");
    hashtable *ht = malloc(sizeof(hashtable));
    ht->size = size;
    ht->table = malloc(size * sizeof(char *));
    for (int i = 0; i < size; i++) {
        ht->table[i] = NULL; //malloc((30 + 1) * sizeof(char));
    }
    printf("Done.\n");
    return ht;
}

void ht_destroy(hashtable *ht) {
    for (int i = 0; i < ht->size; i++) {
        free(ht->table[i]);
    }
    free(ht->table);
    free(ht);
}

/* ----------------- */
// HASHING FUNCTIONS //
/* ----------------- */

// Mix and match these functions as much as you like.

/**
 * Unsigned 64 bit FNV-1a hashing function.
 */
u64 fnv1a(char *s) {
    u64 hash = 0xcbf29ce484222325;
    for (size_t i = 0; i < strlen(s); i++) {
        hash ^= s[i];
        hash *= 0x100000001b3;
    }
    return hash;
}

/**
 * Unsigned 32 bit FNV-1a hashing function.
 */
u32 fnv1a32(char *s) {
    u32 hash = 0x811c9dc5;
    for (size_t i = 0; i < strlen(s); i++) {
        hash ^= s[i];
        hash *= 0x01000193;
    }
    return hash;
}

u32 djb2(char *s) {
    u32 hash = 5381;
    int c;
    while (c = *s++)
        hash = ((hash << 5) + hash) + c;
    return hash;
}

#define ROTATE32(x, y) ((x << y) | (x >> (32 - y)))
u32 murmur3(char *s) {
    u32 c1 = 0xcc9e2d51;
    u32 c2 = 0x1b873593;
    u32 r1 = 15;
    u32 r2 = 13;
    u32 m = 5;
    u32 n = 0xe6546b64;
    u32 hash = SEED;
    u32 len = strlen(s);

    int nblocks = len / 4;
    u32 *blocks = (u32 *) s;
    u32 k;
    for (int i = 0; i < nblocks; i++) {
        k = blocks[i];
        k *= c1;
        k = ROTATE32(k, r1);
        k *= c2;

        hash ^= k;
        hash = ROTATE32(hash, r2) * m + n;
    }

    u32 *tail = (u32 *) (s + nblocks * 4);
    u32 k1 = 0;

    switch (len & 3) {
        case 3:
            k1 ^= tail[2] << 16;
        case 2:
            k1 ^= tail[1] << 8;
        case 1:
            k1 ^= tail[0];
            k1 *= c1;
            k1 = ROTATE32(k1, r1);
            k1 *= c2;
            hash ^= k1;
    }

    hash ^= len;
    hash ^= (hash >> 16);
    hash *= 0x85ebca6b;
    hash ^= (hash >> 13);
    hash *= 0xc2b2ae35;
    hash ^= (hash >> 16);
    return hash;
}
#undef ROTATE32

u32 oaat(char *s) {
    u32 h = 0;

    for (int i = 0; i < strlen(s); i++) {
        h += s[i];
        h += (h << 10);
        h ^= (h >> 6);
    }
    
    h += (h << 3);
    h ^= (h >> 11);
    h += (h << 15);
    return h;
}

/* -------------- */
// LINEAR PROBING //
/* -------------- */

/** 
 * Insert item into hashmap via linear probing.
 * Best results so far are with the 32 bit FNV-1a
 * hashing algorithm.
 */
int insert_lp(hashtable *ht, char *val) {
    u32 key = fnv1a32(val) % ht->size;
    bool wrapping = false;
    while (ht->table[key] != NULL) {
        collisions++;
        if (++key == ht->size) {
            if (wrapping) {
                printf("Hashmap full!\n");
                return 0;
            }
            key = 0;
            wrapping = true;
        }
    }
    ht->table[key] = malloc(30);
    strcpy(ht->table[key], val);
    return -1;
}

/** 
 * Find value in hashmap via linear probing.
 */
int find_lp(hashtable *ht, char *val) {
    u32 key = fnv1a32(val) % ht->size;
    printf("Original key: %u\n", key);
    while (ht->table[key] != NULL) {
        if (strcmp(val, ht->table[key]) == 0) {
            printf("Found \"%s\" with key %u\n", val, key);
            return 1;
        }
        if (++key == ht->size)
            key = 0;
    }
    return 0;
}

/* ----------------- */
// QUADRATIC PROBING //
/* ----------------- */

/** 
 * Insert item into hashmap via quadratic probing.
 * Best results so far are with the 32 bit FNV-1a
 * hashing algorithm.
 */
int insert_qp(hashtable *ht, char *val) {
    u32 key = fnv1a32(val) % ht->size;
    for (int i = 1; ht->table[key] != NULL; i++) {
        key = (key + i*i) % ht->size;
        collisions++;
    }

    ht->table[key] = malloc(30);
    strcpy(ht->table[key], val);
    return 1;
}

/** 
 * Find value in hashmap via quadratic probing.
 */
int find_qp(hashtable *ht, char *val) {
    u32 key = fnv1a32(val) % ht->size;
    printf("Original key: %u\n", key);
    for (int i = 1; ht->table[key] != NULL; i++) {
        if (strcmp(val, ht->table[key]) == 0) {
            printf("Found \"%s\" with key %u\n", val, key);
            return 1;
        }
        key = (key + i*i) % ht->size;
    }
    return 0;
}

// -------------- //
/* DOUBLE HASHING */
// -------------- //

/**
 * This is so far the optimal algorithm as far as
 * speed and lack of collisions is concerned:
 *
 * Compute key using one-at-a-time algorithm, compute
 * step using FNV-1a 32 bit hash.
 */
int insert_dh(hashtable *ht, char *val) {
    u32 key = oaat(val) % ht->size;
    u32 step = fnv1a32(val) % (ht->size - 1) + 1;
    while (ht->table[key] != NULL) {
        collisions++;
        key = (key + step) % ht->size;
    }
    ht->table[key] = malloc(30);
    strcpy(ht->table[key], val);
    return 1;
}

int find_dh(hashtable *ht, char *val) {
    u32 key = oaat(val) % ht->size;
    u32 step = fnv1a32(val) % (ht->size - 1) + 1;
    printf("Original key: %u\n", key);
    while (ht->table[key] != NULL) {
        if (strcmp(val, ht->table[key]) == 0) {
            printf("Found \"%s\" with key %u\n", val, key);
            return 1;
        }
        key = (key + step) % ht->size;
    }
    return 0;
}

/**
 * Trims newline characters from string.
 */
char *trim(char *s) {
    int i = strlen(s) - 1;
    if ((i > 0) && (s[i] == '\n'))
        s[i] = '\0';
    return s;
}

int main(int argc, char **argv) {

    // Initialise hashtable with size HT_MAX_SIZE
    // as defined above.
    hashtable *ht = ht_init(HT_MAX_SIZE);

    FILE *fp;
    char line[30];

    fp = fopen("/home/conor/dictionary.txt", "r");
    if (fp == NULL) {
        printf("File read failed\n");
        exit(1);
    }

prompt:
    printf("Linear probing, quadratic probing, or double hashing? [l|q|d]: ");
    char r = getchar();
    char word[30];
    clock_t st, tt;

    switch (r) {
        case 'l':
            st = clock();
            while (fgets(line, sizeof(line), fp)) {
                insert_lp(ht, trim(line));
            }
            tt = clock() - st;

            double lp_time = tt * 1000 / CLOCKS_PER_SEC;
            printf("Time to fill hashtable: %.3f s\n", lp_time / 1000);
            printf("Number of collisions: %d\n", collisions);

            printf("Enter a word to search: ");
            scanf("%s", word);

            if (find_lp(ht, word) == 0)
                printf("Could not find \"%s\"\n", word);

            break;

        case 'q':
            st = clock();
            while (fgets(line, sizeof(line), fp)) {
                insert_qp(ht, trim(line));
            }
            tt = clock() - st;

            double qp_time = tt * 1000 / CLOCKS_PER_SEC;
            printf("Time to fill hashtable: %.3f s\n", qp_time / 1000);
            printf("Number of collisions: %d\n", collisions);

            printf("Enter a word to search: ");
            scanf("%s", word);

            if (find_qp(ht, word) == 0)
                printf("Could not find \"%s\"\n", word);

            break;

        case 'd':
            st = clock();
            int i = 0;
            while (fgets(line, sizeof(line), fp)) {
                insert_dh(ht, trim(line));
            }
            tt = clock() - st;

            double dh_time = tt * 1000 / CLOCKS_PER_SEC;
            printf("Time to fill hashtable: %.3f s\n", dh_time / 1000);
            printf("Number of collisions: %d\n", collisions);

            printf("Enter a word to search: ");
            scanf("%s", word);

            if (find_dh(ht, word) == 0)
                printf("Could not find \"%s\"\n", word);

            break;

        default:
            printf("Not a valid answer.\n");
            goto prompt;
    }

    fclose(fp);
    ht_destroy(ht);
}

