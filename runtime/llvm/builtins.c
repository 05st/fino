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
        name* box = GC_malloc(sizeof(name));\
        box->value = v;\
        return box;\
    }\

#define FINO_NUMERICAL(fino_type)\
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
    }\
    _fino_bool* fino_type##_grt(fino_type* a, fino_type* b) {\
        return _fino_bool_new(a->value > b->value);\
    }\
    _fino_bool* fino_type##_lst(fino_type* a, fino_type* b) {\
        return _fino_bool_new(a->value < b->value);\
    }\
    _fino_bool* fino_type##_geq(fino_type* a, fino_type* b) {\
        return _fino_bool_new(a->value >= b->value);\
    }\
    _fino_bool* fino_type##_leq(fino_type* a, fino_type* b) {\
        return _fino_bool_new(a->value <= b->value);\
    }

#define FINO_PRINT(fino_type, format)\
    void* fino_type##_print(fino_type* box) {\
        printf(format, box->value);\
        return NULL;\
    }

void* _fino_unit_val = NULL;

FINO_TYPE(int8_t, _fino_bool);
FINO_TYPE(int8_t, _fino_char);
FINO_TYPE(int64_t, _fino_int);
FINO_TYPE(double, _fino_float);

FINO_NUMERICAL(_fino_int);
FINO_NUMERICAL(_fino_float);

FINO_PRINT(_fino_char, "%c");
FINO_PRINT(_fino_int, "%lld");
FINO_PRINT(_fino_float, "%f");

// Boolean operations
// Special unbox function for bools only
int8_t _fino_bool_get(_fino_bool* box) {
    return box->value;
}
_fino_bool* _fino_bool_and(_fino_bool* a, _fino_bool* b) {
    return _fino_bool_new(a->value && b->value);
}
_fino_bool* _fino_bool_or(_fino_bool* a, _fino_bool* b) {
    return _fino_bool_new(a->value || b->value);
}

// String
typedef struct {
    char* buffer;
    int64_t length;
} _fino_string;
_fino_string* _fino_string_new(char* s, int64_t l) {
    _fino_string* res = GC_malloc(sizeof(_fino_string));
    res->buffer = s;
    res->length = l;
    return res;
}
void* _fino_string_print(_fino_string* str) {
    printf("%s", str->buffer);
    return NULL;
}
void* _fino_string_concat(_fino_string* a, _fino_string* b) {
    int64_t res_len = a->length + b->length;
    char* res_buf = GC_malloc(res_len + 1); // Add one for null character
    strcpy(res_buf, a->buffer);
    strcat(res_buf, b->buffer);
    return _fino_string_new(res_buf, res_len);
}

// User input
_fino_string* _fino_readline() {
    char line[1024];
    scanf("%1023[^\n]", line);
    int64_t len = strlen(line);
    char* buffer = GC_malloc(len + 1);
    strcpy(buffer, line);
    return _fino_string_new(buffer, len);
}
