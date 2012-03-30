#include <ffi/ffi.h>

int getDefaultABI() {
	return FFI_DEFAULT_ABI;
}

int getSizeOfCIF() {
	return (int) sizeof(ffi_cif);
}