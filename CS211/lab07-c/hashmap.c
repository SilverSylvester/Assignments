#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

// TODO: Implement double hashing
// Also find a better way to write these functions.

// As of yet unimplemented option.
// Default is true.
#define ALLOW_DUPES true

#define HT_MAX_SIZE 400009

typedef unsigned long long u64;
typedef unsigned int u32;

static int collisions = 0;

/* ---------------- */
//  HASHTABLE IMPL  //
/* ---------------- */

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

// Pick the best one out of these. For the current data,
// FNV-1a-32 is the best so far.

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
    u64 hash = 0x811c9dc5;
    for (size_t i = 0; i < strlen(s); i++) {
        hash ^= s[i];
        hash *= 0x01000193;
    }
    return hash;
}

/* -------------- */
// LINEAR PROBING //
/* -------------- */

/** 
 * Insert item into hashmap via linear probing using
 * the FNV-1a hashing function.
 */
int insert_lp_fnv1a(hashtable *ht, char *val) {
    u64 key = fnv1a(val) % ht->size;
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
 * Find value in hashmap via linear probing using
 * the FNV-1a hashing function
 */
int find_lp_fnv1a(hashtable *ht, char *val) {
    u64 key = fnv1a(val) % ht->size;
    printf("Original key: %llu\n", key);
    while (ht->table[key] != NULL) {
        if (strcmp(val, ht->table[key]) == 0) {
            printf("Found \"%s\" with key %llu\n", val, key);
            return 1;
        }
        if (++key == ht->size)
            key = 0;
    }
    return 0;
}

/** 
 * Insert item into hashmap via linear probing using
 * FNV-1a 32 bit hash.
 */
int insert_lp_fnv1a32(hashtable *ht, char *val) {
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
 * Find value in hashmap via linear probing using
 * FNV-1a 32 bit hash.
 */
int find_lp_fnv1a32(hashtable *ht, char *val) {
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
 * Insert item into hashmap via quadratic probing using
 * FNV-1a hash.
 */
int insert_qp_fnv1a(hashtable *ht, char *val) {
    u64 key = fnv1a(val) % ht->size;
    for (int i = 1; ht->table[key] != NULL; i++) {
        key = (key + i*i) % ht->size;
        collisions++;
    }

    ht->table[key] = malloc(30);
    strcpy(ht->table[key], val);
    return 1;
}

/** 
 * Find value in hashmap via quadratic probing using
 * FNV-1a hash.
 */
int find_qp_fnv1a(hashtable *ht, char *val) {
    u64 key = fnv1a(val) % ht->size;
    printf("Original key: %llu\n", key);
    for (int i = 1; ht->table[key] != NULL; i++) {
        if (strcmp(val, ht->table[key]) == 0) {
            printf("Found \"%s\" with key %llu\n", val, key);
            return 1;
        }
        key = (key + i*i) % ht->size;
    }
    return 0;
}

/** 
 * Insert item into hashmap via quadratic probing using
 * FNV-1a 32 bit hash.
 */
int insert_qp_fnv1a32(hashtable *ht, char *val) {
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
 * Find value in hashmap via quadratic probing using
 * FNV-1a 32 bit hash.
 */
int find_qp_fnv1a32(hashtable *ht, char *val) {
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

/**
 * Trims newline characters from string.
 */
char *trim(char *s) {
    int i = strlen(s) - 1;
    if ((i > 0) && (s[i] == '\n'))
        s[i] = '\0';
    return s;
}

/**
 * Example: zymome collides once on quadratic probing.
 *          ______ collides ____ on linear probing.
 */
int main(int argc, char **argv) {
    
    // Initialise hashtable with size HT_MAX_SIZE
    hashtable *ht = ht_init(HT_MAX_SIZE);

    FILE *fp;
    char line[30];

    fp = fopen("/home/conor/dictionary.txt", "r");
    if (fp == NULL) {
        printf("File read failed\n");
        exit(1);
    }

prompt:
    printf("Linear probing or quadratic probing? [l|q]: ");
    char r = getchar();
    char word[30];
    clock_t st, tt;

    switch (r) {
        case 'l':
            st = clock();
            while (fgets(line, sizeof(line), fp)) {
                insert_lp_fnv1a(ht, trim(line));
            }
            tt = clock() - st;

            double lp_time = tt * 1000 / CLOCKS_PER_SEC;
            printf("Time to fill hashtable: %.3f s\n", lp_time / 1000);

            printf("Number of collisions: %d\n", collisions);
            printf("Enter a word to search: ");
            scanf("%s", word);

            if (find_qp_fnv1a(ht, word) == 0)
                printf("Could not find \"%s\"\n", word);
            
            break;
        
        case 'q':
            st = clock();
            while (fgets(line, sizeof(line), fp)) {
                insert_qp_fnv1a32(ht, trim(line));
            }
            tt = clock() - st;

            double qp_time = tt * 1000 / CLOCKS_PER_SEC;
            printf("Time to fill hashtable: %.3f s\n", qp_time / 1000);
            
            printf("Number of collisions: %d\n", collisions);
            printf("Enter a word to search: ");
            scanf("%s", word);

            if (find_qp_fnv1a32(ht, word) == 0)
                printf("Could not find \"%s\"\n", word);
            
            break;

        default:
            printf("Not a valid answer.\n");
            goto prompt;
    }

    fclose(fp);
    ht_destroy(ht);
}

