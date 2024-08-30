// Requires bdw-gc (aka libgc) to be installed
#include <gc.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define FINO_TYPE(inner_type, name)\
    typedef struct {\
        inner_type value;\
    } name;\
    name* name##_new(inner_type v) {\
        name* box = GC_MALLOC(sizeof(name));\
        box->value = v;\
        return box;\
    }\
    inner_type name##_get(name* box) {\
        return box->value;\
    }

#define FINO_ARITH(fino_type)\
    fino_type* fino_type##_add(fino_type* a, fino_type* b) {\
        return fino_type##_new(a->value + b->value);\
    }\
    fino_type* fino_type##_sub(fino_type* a, fino_type* b) {\
        return fino_type##_new(a->value - b->value);\
    }\
    fino_type* fino_type##_mul(fino_type* a, fino_type* b) {\
        return fino_type##_new(a->value * b->value);\
    }\
    fino_type* fino_type##_div(fino_type* a, fino_type* b) {\
        return fino_type##_new(a->value / b->value);\
    }

#define FINO_PRINT(fino_type, format)\
    void* fino_type##_print(fino_type* box) {\
        printf(format, box->value);\
        return NULL;\
    }

void* _fino_unit_val = NULL;

FINO_TYPE(int8_t, _fino_bool);
FINO_TYPE(int8_t, _fino_char);
FINO_TYPE(int32_t, _fino_int);
FINO_TYPE(float, _fino_float);

FINO_ARITH(_fino_int);
FINO_ARITH(_fino_float);

FINO_PRINT(_fino_char, "%c");
FINO_PRINT(_fino_int, "%d");
FINO_PRINT(_fino_float, "%f");
