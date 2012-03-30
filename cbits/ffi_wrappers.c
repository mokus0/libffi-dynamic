#ifdef __APPLE__
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif

int getDefaultABI() {
	return FFI_DEFAULT_ABI;
}

int getSizeOfCIF() {
	return (int) sizeof(ffi_cif);
}