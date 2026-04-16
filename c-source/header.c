#define NULL ((void*)0)
#define REGION_SIZE (0x4000)
#define TLAMBDA (0)
#define TMACHINE (1)


//
typedef unsigned long int size_t;

void kmemcpy(void* s1, void* s2, size_t n) {
    for (size_t i = 0; i < n; i++) {
        ((char*)s1)[i] = ((char*)s2)[i];
    }
}
void kmemset(void* s, char c, size_t n) {
    for (size_t i = 0; i < n; i++) {
        ((char*)s)[i] = c;
    }
}

//
typedef struct _CLOSURE {
    struct _CLOSURE* (*fn)(struct _CLOSURE* this, struct _CLOSURE* args);
    struct _ENVIRONMENT* env;
    long value;
    int type;
} CLOSURE;
typedef struct _ENVIRONMENT {
    struct _CLOSURE* value;
    struct _ENVIRONMENT* next;
} ENVIRONMENT;

extern char region[REGION_SIZE];
extern unsigned int rptr;
void* kmalloc(size_t size) {
    unsigned int ret = rptr;
    rptr += size;
    return &region[ret];
}
void kfreal() {
    kmemset(region, 0, REGION_SIZE);
    rptr = 0;
}

ENVIRONMENT* extenv(ENVIRONMENT* env, CLOSURE* args) {
    if (env->next != NULL) {
        extenv(env->next, args);
        return env;
    }
    else {
        env->next = kmalloc(sizeof(ENVIRONMENT));
        env->next->value = args;
        return NULL;
    }
}
CLOSURE* ccreat(CLOSURE* (*c)(CLOSURE* this, CLOSURE* arg), ENVIRONMENT* nenv) {
    CLOSURE* ret = kmalloc(sizeof(CLOSURE));
    ret->fn = c;
    ret->env = nenv;
    ret->value = 0;
    ret->type = TLAMBDA;
    return ret;
}
CLOSURE* apply(CLOSURE* arg0, CLOSURE* arg1) {
    return arg0->fn(arg0, arg1);
}
CLOSURE* lookup(ENVIRONMENT* env, unsigned long depth) {
    if (depth > 0) {
        return lookup(env->next, depth);
    }
    else {
        return env->value;
    }
}
char region[0x4000] = {0, };
unsigned int rptr = 0;