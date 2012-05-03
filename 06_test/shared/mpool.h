#ifndef MPOOL_H
#define MPOOL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MPOOL_POOL_SIZ   (64 * 1024)
#define MPOOL_ALIGN_SIZE (8)

#define MPOOL_MALLOC(p, siz, type)                    \
    do {                                        \
        if (((p) = (type*)malloc(siz)) == NULL) {      \
            fprintf(stderr, "malloc failed");   \
            exit(-1);                           \
        }                                       \
    } while(false)

#define MPOOL_FREE(p)                                   \
    do {                                                \
        if (p != NULL) {                                \
            free(p);                                    \
            (p) = NULL;                                 \
        }                                               \
    } while(false)
      
/**
 * memory pool structure
 */
typedef struct mpool_pool_t {
    void                *pool;     // memory pool field
    struct mpool_pool_t *next;     // next memory pool's pointer
} mpool_pool_t;

typedef struct mpool_t {
    mpool_pool_t *head;       // memory pool's head
    void         *begin;      // data for internal conduct
    size_t        usiz;       // used pool size of current pool
    size_t        msiz;       // max pool size of current pool
    mpool_pool_t *mpool;      // memory pool
} mpool_t;

mpool_t *mpool_create (size_t siz);
void *mpool_alloc(size_t siz, mpool_t *pool);
void mpool_destroy (mpool_t *pool);

#endif


