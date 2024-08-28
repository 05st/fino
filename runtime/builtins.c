// Requires bdw-gc (aka libgc) to be installed
#include <gc.h>
#include <stdint.h>
#include <string.h>

///////////////
// FINO UNIT //
///////////////

void* _fino_unit_val = NULL;

///////////////
// FINO CHAR //
///////////////

typedef struct {
    int8_t value;
} _fino_char;

_fino_char* _fino_char_new(int8_t c) {
    _fino_char* box = GC_MALLOC(sizeof(_fino_char));
    box->value = c;
    return box;
}

int8_t _fino_char_get(_fino_char* box) {
    return box->value;
}

//////////////
// FINO INT //
//////////////

typedef struct {
    int32_t value;
} _fino_int;

_fino_int* _fino_int_new(int32_t i) {
    _fino_int* box = GC_MALLOC(sizeof(_fino_int));
    box->value = i;
    return box;
}

int32_t _fino_int_get(_fino_int* box) {
    return box->value;
}

_fino_int* _fino_int_add(_fino_int* lhs, _fino_int* rhs) {
    return _fino_int_new(lhs->value + rhs->value);
}

_fino_int* _fino_int_sub(_fino_int* lhs, _fino_int* rhs) {
    return _fino_int_new(lhs->value - rhs->value);
}

//////////////
// FINO STR //
//////////////

typedef struct {
    int8_t* buffer;
    int32_t length;
} _fino_str;

// Since all data is immutable in fino, we don't have to copy the contents
_fino_str* _fino_str_new(int8_t* init, int32_t length) {
    _fino_str* str = GC_MALLOC(sizeof(_fino_str));
    str->buffer = init;
    str->length = length;
    return str;
}

// Since all data is immutable in fino, we don't have to copy the contents
_fino_str* _fino_str_clone(_fino_str* orig) {
    _fino_str* str = GC_MALLOC(sizeof(_fino_str));
    str->buffer = orig->buffer;
    str->length = orig->length;
    return str;
}

_fino_str* _fino_str_append(_fino_str* str, _fino_char* c) {
    _fino_str* new_str = GC_MALLOC(sizeof(_fino_str));
    new_str->length = str->length + 1;
    new_str->buffer = GC_MALLOC(new_str->length);
    memcpy(new_str->buffer, str->buffer, str->length);
    new_str->buffer[str->length] = _fino_char_get(c);
    return new_str;
}

_fino_str* _fino_str_concat(_fino_str* lhs, _fino_str* rhs) {
    _fino_str* new_str = GC_MALLOC(sizeof(_fino_str));
    new_str->length = lhs->length + rhs->length;
    new_str->buffer = GC_MALLOC(new_str->length);
    memcpy(new_str->buffer, lhs->buffer, lhs->length);
    memcpy(new_str->buffer + lhs->length, rhs->buffer, rhs->length);
    return new_str;
}
