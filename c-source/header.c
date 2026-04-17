#define NULL ((void*)0)
#define REGION_SIZE (0x4000)

#define DECLAREF0(a, b)\
CLOSURE* a(CLOSURE* this, CLOSURE* arg) {\
    ENVIRONMENT* nenv = extenv(this->env, arg);\
    return ccreat(b, nenv);\
}
#define DECLAREF1(a, b)\
CLOSURE* a(CLOSURE* this, CLOSURE* arg) {\
    ENVIRONMENT* nenv = extenv(this->env, arg);\
    b\
}
#define DECLAREV(a, b) CLOSURE* a = b;
#define CCREAT(a, b) ccreat(a, b)
#define APPLY(a, b) apply(a, b)
#define LOOKUP(a) lookup(nenv, a)
#define RETURN(a) return a;
#define ARGC argc
#define ARGV(a) argv[a]
#define SIZE1 volatile unsigned char
#define SIZE2 volatile unsigned short int
#define SIZE4 volatile unsgined int
#define SIZE8 volatile unsigned long int
#define READSIZE1(a) *(SIZE1 *)a
#define READSIZE2(a) *(SIZE2 *)a
#define READSIZE4(a) *(SIZE4 *)a
#define READSIZE8(a) *(SIZE8 *)a
#define WRITESIZE1(a, b) *(SIZE1 *)a = b
#define WRITESIZE2(a, b) *(SIZE2 *)a = b
#define WRITESIZE4(a, b) *(SIZE4 *)a = b
#define WRITESIZE8(a, b) *(SIZE8 *)a = b
#define WRAPPER(a) int main(int argc, char** argv) {a}
#define DECLAREXT(a, b) extern long int a b;
#define EXTCALL(a, b) a b
#define DECLARES(a, b) char a[b] = {0, };
#define GETS(a) &a[0]


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
char region[REGION_SIZE] = {0, };
unsigned int rptr = 0;
