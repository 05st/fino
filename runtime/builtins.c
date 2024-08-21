// Requires bdw-gc (aka libgc) to be installed
#include <gc.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    int32_t value;
} fino__int;

inline fino__int* fino__int_box(int32_t i) {
    fino__int* box = GC_malloc(sizeof(fino__int));
    box->value = i;
    return box;
}

inline int32_t fino__int_unbox(fino__int* box) {
    return box->value;
}

typedef struct {
    int8_t value;
} fino__char;

inline fino__char* fino__char_box(int8_t c) {
    fino__char* box = GC_malloc(sizeof(fino__char));
    box->value = c;
    return box;
}

inline int8_t fino__char_unbox(fino__char* box) {
    return box->value;
}

typedef struct {
    int8_t* buffer;
    int32_t length;
    int32_t max_length;
    int32_t factor;
} fino__str;

fino__str* fino__str_new() {
    fino__str* str = GC_malloc(sizeof(fino__str));
    str->buffer = NULL;
    str->length = 0;
    str->max_length = 0;
    str->factor = 16;
    return str;
}

void fino__str_resize(fino__str* str, int32_t size) {
    int8_t* new_buffer = GC_malloc(size);
    memcpy(new_buffer, str->buffer, str->length);
    GC_free(str->buffer);
    str->buffer = new_buffer;
    str->max_length = size;
}

void fino__str_append_prim(fino__str* str, int8_t c) {
    if (str->length == str->max_length) {
        fino__str_resize(str, str->max_length + str->factor);
    }
    str->buffer[str->length++] = c;
}

inline void fino__str_append(fino__str* str, fino__char* c) {
    fino__str_append_prim(str, c->value);
}
