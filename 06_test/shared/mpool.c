#include "mpool.h"

/**
 * private function
 */
static inline void mpool_extend(mpool_pool_t *p, size_t siz, mpool_t *pool);
static inline size_t mpool_align(size_t siz);
static inline size_t mpool_decide_create_siz(size_t siz);

/**
 * create memory pool
 */
mpool_t *mpool_create (size_t siz) {
    mpool_t *pool;
    siz = mpool_decide_create_siz(siz);
    MPOOL_MALLOC(pool,              sizeof(*pool), mpool_t );
    MPOOL_MALLOC(pool->mpool,       sizeof(*pool->mpool), mpool_pool_t );
    MPOOL_MALLOC(pool->mpool->pool, siz, void );
    memset(pool->mpool->pool, 0, siz);
    
    if (!pool->mpool || !pool->mpool->pool) {
        return NULL;
    }
    
    pool->mpool->next = NULL;

    pool->begin = pool->mpool->pool;
    pool->head  = pool->mpool;
    pool->usiz  = 0;
    pool->msiz  = siz;
    
    return pool;
}

/**
 * allocate memory from memory pool
 */
void *mpool_alloc(size_t siz, mpool_t *pool) {
    mpool_pool_t **p = &pool->mpool;
    mpool_pool_t *pp = *p;
    size_t usiz = mpool_align(pool->usiz + siz);
    size_t msiz = pool->msiz;
    void     *d = pool->begin;
    if (usiz > msiz) {
        mpool_extend(pp, usiz * 2 + 1, pool);
        pool->usiz = 0;
        pool->msiz = usiz * 2;
        d = pool->begin;
        pool->begin += mpool_align(siz);
        *p = pp->next;
    } else {
        pool->usiz = usiz;
        pool->begin += mpool_align(siz);
    }
    
    return d;
}

/**
 * release all memory pool
 */
void mpool_destroy (mpool_t *pool) {
    for (mpool_pool_t *p=pool->head;p!=NULL;) {
        mpool_pool_t *current = p;
        mpool_pool_t *next    = p->next;
        MPOOL_FREE(current->pool);
        MPOOL_FREE(current);
        p = next;
    }
    MPOOL_FREE(pool);
}

/* following is private function */ 

/**
 * extend memory pool
 */
static inline void mpool_extend(mpool_pool_t *p, size_t siz, mpool_t *pool) {
    siz = mpool_decide_create_siz(siz);
    mpool_pool_t *pp;
    MPOOL_MALLOC(pp, sizeof(*pp), mpool_pool_t );
    MPOOL_MALLOC(pp->pool, siz, void );
    memset(pp->pool, 0, siz);
    
    pp->next = NULL;

    p->next = pp;

    pool->begin = pp->pool;
}

/**
 * align byte boundary
 */
static inline size_t mpool_align(size_t siz) {
    return (siz + (MPOOL_ALIGN_SIZE - 1)) & ~(MPOOL_ALIGN_SIZE - 1);
}

/**
 * decide mpool size
 */
static inline size_t mpool_decide_create_siz(size_t siz) {
    return siz <= 0 ? MPOOL_POOL_SIZ : mpool_align(siz);
}

