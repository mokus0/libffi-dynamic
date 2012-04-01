#ifdef __APPLE__
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif

int defaultABI() {
	return FFI_DEFAULT_ABI;
}

size_t sizeOfCIF() {
    return sizeof(ffi_cif);
}

size_t sizeOfClosure() {
    return sizeof(ffi_closure);
}

