#define __LAMPHEADER_NULL ((void*)0)
#define __LAMPHEADER_REGION_SIZE (0x4000)

#define __LAMPHEADER_DECLAREF(a) __LAMPHEADER_CLOSURE* a(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg);
#define __LAMPHEADER_DEFINEF0(a, b)\
__LAMPHEADER_CLOSURE* a(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {\
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);\
    return __LAMPHEADER_ccreat(b, (long)__LAMPHEADER_nenv, 0);\
}
#define __LAMPHEADER_DEFINEF1(a, b)\
__LAMPHEADER_CLOSURE* a(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {\
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);\
    b\
}
#define __LAMPHEADER_DEFINEV(a, b) __LAMPHEADER_CLOSURE* a = b;
#define __LAMPHEADER_CCREAT(a, b, c) __LAMPHEADER_ccreat(a, (long)b, c)
#define __LAMPHEADER_APPLY(a, b) __LAMPHEADER_apply((__LAMPHEADER_CLOSURE*)a, (__LAMPHEADER_CLOSURE*)b)
#define __LAMPHEADER_LOOKUP(a) __LAMPHEADER_lookup(__LAMPHEADER_nenv, a)
#define __LAMPHEADER_RETURN(a) return a;
#define __LAMPHEADER_ARGC argc
#define __LAMPHEADER_ARGV(a) (long)argv[a]
#define __LAMPHEADER_READSIZE1(a) *(char*)a
#define __LAMPHEADER_READSIZE2(a) *(short*)a
#define __LAMPHEADER_READSIZE4(a) *(int*)a
#define __LAMPHEADER_READSIZE8(a) *(long*)a
#define __LAMPHEADER_WRITESIZE1(a, b) *(char*)a = b
#define __LAMPHEADER_WRITESIZE2(a, b) *(short*)a = b
#define __LAMPHEADER_WRITESIZE4(a, b) *(int*)a = b
#define __LAMPHEADER_WRITESIZE8(a, b) *(long*)a = b
#define __LAMPHEADER_WRAPPER(a) int main(int argc, char** argv) {__LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_NULL; a;}
#define __LAMPHEADER_DECLAREXT(a, b) extern long a b;
#define __LAMPHEADER_EXTCALL(a, b) a b
#define __LAMPHEADER_DEFINES(a, b) char a[b] = {0, };
#define __LAMPHEADER_GETS(a) __LAMPHEADER_wrap((long)&a[0])
#define __LAMPHEADER_DEFINEC(a, b) long a = (long)b;
#define __LAMPHEADER_CHURCH(a) __LAMPHEADER_church_to_native((__LAMPHEADER_CLOSURE*)a)
#define __LAMPHEADER_IFTHENELSE(a, b, c) (a ? b : c)
#define __LAMPHEADER_CHECKTF(a) __LAMPHEADER_check_tf(a)
#define __LAMPHEADER_CHECKERR if(__LAMPHEADER_iserr){return -1;}
#define __LAMPHEADER_WRAP(a) __LAMPHEADER_wrap(a)
#define __LAMPHEADER_UNWRAP(a) __LAMPHEADER_unwrap(a)


typedef struct __LAMPHEADER__CLOSURE {
    struct __LAMPHEADER__CLOSURE* (*fn)(struct __LAMPHEADER__CLOSURE*, struct __LAMPHEADER__CLOSURE*);
    union {
        struct __LAMPHEADER__ENVIRONMENT* env;
        long n;
    } data;
    char isint;
} __LAMPHEADER_CLOSURE;
typedef struct __LAMPHEADER__ENVIRONMENT {
    struct __LAMPHEADER__CLOSURE* value;
    struct __LAMPHEADER__ENVIRONMENT* next;
} __LAMPHEADER_ENVIRONMENT;

char __LAMPHEADER_region[__LAMPHEADER_REGION_SIZE] = {0, };
unsigned int __LAMPHEADER_rptr = 0;
char __LAMPHEADER_iserr = 0;

void __LAMPHEADER_memcpy(void* __LAMPHEADER_s1, void* __LAMPHEADER_s2, long __LAMPHEADER_n) {
    for (long __LAMPHEADER_i = 0; __LAMPHEADER_i < __LAMPHEADER_n; __LAMPHEADER_i++) {
        ((char*)__LAMPHEADER_s1)[__LAMPHEADER_i] = ((char*)__LAMPHEADER_s2)[__LAMPHEADER_i];
    }
}
void __LAMPHEADER_memset(void* __LAMPHEADER_s, char __LAMPHEADER_c, long __LAMPHEADER_n) {
    for (long __LAMPHEADER_i = 0; __LAMPHEADER_i < __LAMPHEADER_n; __LAMPHEADER_i++) {
        ((char*)__LAMPHEADER_s)[__LAMPHEADER_i] = __LAMPHEADER_c;
    }
}
void* __LAMPHEADER_malloc(long __LAMPHEADER_size) {
    unsigned int __LAMPHEADER_ret = __LAMPHEADER_rptr;
    __LAMPHEADER_rptr += __LAMPHEADER_size;
    return &__LAMPHEADER_region[__LAMPHEADER_ret];
}
void __LAMPHEADER_freal() {
    __LAMPHEADER_memset(__LAMPHEADER_region, 0, __LAMPHEADER_REGION_SIZE);
    __LAMPHEADER_rptr = 0;
}
__LAMPHEADER_ENVIRONMENT* __LAMPHEADER_extenv(__LAMPHEADER_ENVIRONMENT* __LAMPHEADER_env, __LAMPHEADER_CLOSURE* __LAMPHEADER_args) {
    if (__LAMPHEADER_env == __LAMPHEADER_NULL) {
        __LAMPHEADER_env = __LAMPHEADER_malloc(sizeof(__LAMPHEADER_ENVIRONMENT));
        __LAMPHEADER_env->next = __LAMPHEADER_NULL;
        __LAMPHEADER_env->value = __LAMPHEADER_args;
        return __LAMPHEADER_env;
    }
    else {
        __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_malloc(sizeof(__LAMPHEADER_ENVIRONMENT));
        __LAMPHEADER_nenv->value = __LAMPHEADER_args;
        __LAMPHEADER_nenv->next = __LAMPHEADER_env;
        return __LAMPHEADER_nenv;
    }
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_self(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_ccreat(__LAMPHEADER_CLOSURE* (*__LAMPHEADER_c)(__LAMPHEADER_CLOSURE*, __LAMPHEADER_CLOSURE*), long __LAMPHEADER_data, char __LAMPHEADER_isint) {
    __LAMPHEADER_CLOSURE* __LAMPHEADER_ret = __LAMPHEADER_malloc(sizeof(__LAMPHEADER_CLOSURE));
    __LAMPHEADER_ret->fn = __LAMPHEADER_c;
    if (__LAMPHEADER_isint) {
        __LAMPHEADER_ret->data.n = (long)__LAMPHEADER_data;
    }
    else {
        __LAMPHEADER_ret->data.env = (__LAMPHEADER_ENVIRONMENT*)__LAMPHEADER_data;
    }
    __LAMPHEADER_ret->isint = __LAMPHEADER_isint;
    return __LAMPHEADER_ret;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_apply(__LAMPHEADER_CLOSURE* __LAMPHEADER_arg0, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg1) {
    if (__LAMPHEADER_arg0->isint && !__LAMPHEADER_arg1->isint) {
        __LAMPHEADER_iserr = 1;
        __LAMPHEADER_CLOSURE* __LAMPHEADER_ret = __LAMPHEADER_ccreat(__LAMPHEADER_self, (long)__LAMPHEADER_NULL, 0);
        return __LAMPHEADER_ret->fn(__LAMPHEADER_ret, __LAMPHEADER_arg1);
    }
    else return __LAMPHEADER_arg0->fn(__LAMPHEADER_arg0, __LAMPHEADER_arg1);
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_lookup(__LAMPHEADER_ENVIRONMENT* __LAMPHEADER_env, unsigned long __LAMPHEADER_depth) {
    if (__LAMPHEADER_depth > 0) {
        return __LAMPHEADER_lookup(__LAMPHEADER_env->next, __LAMPHEADER_depth - 1);
    }
    else {
        return __LAMPHEADER_env->value;
    }

}
__LAMPHEADER_CLOSURE* __LAMPHEADER_church(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    (__LAMPHEADER_this->data.n)++;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_church_to_native(__LAMPHEADER_CLOSURE* __LAMPHEADER_num) {
    if (__LAMPHEADER_num->isint) {
        return __LAMPHEADER_num;
    }
    else {
        unsigned int __LAMPHEADER_init = __LAMPHEADER_rptr;
        __LAMPHEADER_CLOSURE* __LAMPHEADER_counter = __LAMPHEADER_ccreat(__LAMPHEADER_church, 0, 0);
        __LAMPHEADER_CLOSURE* __LAMPHEADER_dummy = __LAMPHEADER_ccreat(__LAMPHEADER_self, (long)__LAMPHEADER_NULL, 0);
        long __LAMPHEADER_ret = __LAMPHEADER_APPLY(__LAMPHEADER_APPLY(__LAMPHEADER_num, __LAMPHEADER_counter), __LAMPHEADER_dummy)->data.n;
        __LAMPHEADER_rptr = __LAMPHEADER_init;
        return __LAMPHEADER_CCREAT(__LAMPHEADER_NULL, __LAMPHEADER_ret, 1);
    }
}
char __LAMPHEADER_check_tf(__LAMPHEADER_CLOSURE* __LAMPHEADER_cond) {
    if (__LAMPHEADER_cond->isint) {
        return __LAMPHEADER_cond->data.n;
    }
    else {
        unsigned int __LAMPHEADER_init = __LAMPHEADER_rptr;
        __LAMPHEADER_CLOSURE* __LAMPHEADER_ctrue = __LAMPHEADER_ccreat(__LAMPHEADER_self, 1, 0);
        __LAMPHEADER_CLOSURE* __LAMPHEADER_cfalse = __LAMPHEADER_ccreat(__LAMPHEADER_self, 0, 0);
        char __LAMPHEADER_ret = (char)__LAMPHEADER_APPLY(__LAMPHEADER_APPLY(__LAMPHEADER_cond, __LAMPHEADER_ctrue), __LAMPHEADER_cfalse)->data.n;
        __LAMPHEADER_rptr = __LAMPHEADER_init;
        if (__LAMPHEADER_ret != 0 && __LAMPHEADER_ret != 1) {
            __LAMPHEADER_iserr = 1;
            return -1;
        }
        else return __LAMPHEADER_ret;
    }
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_add(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_this->data.n += __LAMPHEADER_arg->data.n;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_sub(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_this->data.n -= __LAMPHEADER_arg->data.n;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_mul(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_this->data.n *= __LAMPHEADER_arg->data.n;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_div(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_this->data.n /= __LAMPHEADER_arg->data.n;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_mod(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_this->data.n %= __LAMPHEADER_arg->data.n;
    return __LAMPHEADER_this;
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_wrap(long __LAMPHEADER_num) {
    return __LAMPHEADER_CCREAT(__LAMPHEADER_NULL, __LAMPHEADER_num, 1);
}
long __LAMPHEADER_unwrap(__LAMPHEADER_CLOSURE* __LAMPHEADER_num) {
    if (!__LAMPHEADER_num->isint) {
        __LAMPHEADER_iserr = 1;
        return -1;
    }
    else {
        return __LAMPHEADER_num->data.n;
    }
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_read1(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);
    long __LAMPHEADER_v0 = __LAMPHEADER_UNWRAP(__LAMPHEADER_LOOKUP(0));
    long __LAMPHEADER_v1 = __LAMPHEADER_UNWRAP(__LAMPHEADER_LOOKUP(1));
    long __LAMPHEADER_ret = 0;
    switch (__LAMPHEADER_v0) {
        case 1: __LAMPHEADER_ret = __LAMPHEADER_READSIZE1(__LAMPHEADER_v1); break;
        case 2: __LAMPHEADER_ret = __LAMPHEADER_READSIZE2(__LAMPHEADER_v1); break;
        case 4: __LAMPHEADER_ret = __LAMPHEADER_READSIZE4(__LAMPHEADER_v1); break;
        case 8: __LAMPHEADER_ret = __LAMPHEADER_READSIZE8(__LAMPHEADER_v1); break;
        default: {
            __LAMPHEADER_iserr = 1;
            __LAMPHEADER_ret = -1;
        }
    }
    return __LAMPHEADER_WRAP(__LAMPHEADER_ret);
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_read0(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);
    return __LAMPHEADER_CCREAT(__LAMPHEADER_read1, __LAMPHEADER_nenv, 0);
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_write2(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);
    long __LAMPHEADER_v0 = __LAMPHEADER_UNWRAP(__LAMPHEADER_LOOKUP(0));
    long __LAMPHEADER_v1 = __LAMPHEADER_UNWRAP(__LAMPHEADER_LOOKUP(1));
    long __LAMPHEADER_v2 = __LAMPHEADER_UNWRAP(__LAMPHEADER_LOOKUP(2));
    long __LAMPHEADER_ret = 0;
    switch (__LAMPHEADER_v0) {
        case 1: __LAMPHEADER_WRITESIZE1(__LAMPHEADER_v2, __LAMPHEADER_v1); break;
        case 2: __LAMPHEADER_WRITESIZE2(__LAMPHEADER_v2, __LAMPHEADER_v1); break;
        case 4: __LAMPHEADER_WRITESIZE4(__LAMPHEADER_v2, __LAMPHEADER_v1); break;
        case 8: __LAMPHEADER_WRITESIZE8(__LAMPHEADER_v2, __LAMPHEADER_v1); break;
        default: {
            __LAMPHEADER_iserr = 1;
            __LAMPHEADER_ret = -1;
        }
    }
    return __LAMPHEADER_WRAP(__LAMPHEADER_ret);
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_write1(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);
    return __LAMPHEADER_CCREAT(__LAMPHEADER_write2, __LAMPHEADER_nenv, 0);
}
__LAMPHEADER_CLOSURE* __LAMPHEADER_write0(__LAMPHEADER_CLOSURE* __LAMPHEADER_this, __LAMPHEADER_CLOSURE* __LAMPHEADER_arg) {
    __LAMPHEADER_ENVIRONMENT* __LAMPHEADER_nenv = __LAMPHEADER_extenv(__LAMPHEADER_this->data.env, __LAMPHEADER_arg);
    return __LAMPHEADER_CCREAT(__LAMPHEADER_write1, __LAMPHEADER_nenv, 0);
}