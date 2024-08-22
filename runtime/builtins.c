// Requires bdw-gc (aka libgc) to be installed
#include <gc.h>
#include <stdint.h>
#include <string.h>

///////////////
// FINO UNIT //
///////////////

void* fino__unit_val = NULL;

///////////////
// FINO CHAR //
///////////////

typedef struct {
    int8_t value;
} fino__char;

fino__char* fino__char_new(int8_t c) {
    fino__char* box = GC_MALLOC(sizeof(fino__char));
    box->value = c;
    return box;
}

int8_t fino__char_get(fino__char* box) {
    return box->value;
}

//////////////
// FINO INT //
//////////////

typedef struct {
    int32_t value;
} fino__int;

fino__int* fino__int_new(int32_t i) {
    fino__int* box = GC_MALLOC(sizeof(fino__int));
    box->value = i;
    return box;
}

int32_t fino__int_get(fino__int* box) {
    return box->value;
}

fino__int* fino__int_add(fino__int* lhs, fino__int* rhs) {
    return fino__int_new(lhs->value + rhs->value);
}

fino__int* fino__int_sub(fino__int* lhs, fino__int* rhs) {
    return fino__int_new(lhs->value - rhs->value);
}

//////////////
// FINO STR //
//////////////

typedef struct {
    int8_t* buffer;
    int32_t length;
} fino__str;

// Since all data is immutable in fino, we don't have to copy the contents
fino__str* fino__str_new(int8_t* init, int32_t length) {
    fino__str* str = GC_MALLOC(sizeof(fino__str));
    str->buffer = init;
    str->length = length;
    return str;
}

// Since all data is immutable in fino, we don't have to copy the contents
fino__str* fino__str_clone(fino__str* orig) {
    fino__str* str = GC_MALLOC(sizeof(fino__str));
    str->buffer = orig->buffer;
    str->length = orig->length;
    return str;
}

fino__str* fino__str_append(fino__str* str, fino__char* c) {
    fino__str* new_str = GC_MALLOC(sizeof(fino__str));
    new_str->length = str->length + 1;
    new_str->buffer = GC_MALLOC(new_str->length);
    memcpy(new_str->buffer, str->buffer, str->length);
    new_str->buffer[str->length] = fino__char_get(c);
    return new_str;
}

fino__str* fino__str_concat(fino__str* lhs, fino__str* rhs) {
    fino__str* new_str = GC_MALLOC(sizeof(fino__str));
    new_str->length = lhs->length + rhs->length;
    new_str->buffer = GC_MALLOC(new_str->length);
    memcpy(new_str->buffer, lhs->buffer, lhs->length);
    memcpy(new_str->buffer + lhs->length, rhs->buffer, rhs->length);
    return new_str;
}
