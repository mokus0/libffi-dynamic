#ifdef __APPLE__
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif

#include <HsFFI.h>

int defaultABI() {
	return FFI_DEFAULT_ABI;
}

size_t sizeOfCIF() {
    return sizeof(ffi_cif);
}

size_t sizeOfClosure() {
    return sizeof(ffi_closure);
}

HsBool typeIsStruct(ffi_type *type) {
    return (type->type == FFI_TYPE_STRUCT)
        ? HS_BOOL_TRUE
        : HS_BOOL_FALSE;
}