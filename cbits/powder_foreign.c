/* Calling C for the machines written in Haskell: a call of whatever type
 * the program says, made with libffi.
 *
 * An argument or a result is said by a letter: c C s S I U L Q for the
 * integers, signed and not, of 8, 16, 32 and 64 bits; F and D for float
 * and double; P for a pointer; and V for no result. An argument comes as
 * a 64-bit integer or a double, whichever its letter is of, and a result
 * goes back the same way.
 */
#if defined(__has_include) && __has_include(<ffi/ffi.h>)
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif
#include <errno.h>
#include <stdint.h>
#include <string.h>

/* What errno was after the last call: nothing the machine does between
 * two calls of the program's is to change what the program sees. */
static __thread int kept_errno = 0;

int powder_errno_get(void) { return kept_errno; }

void powder_errno_set(int value) { kept_errno = value; }

static ffi_type *type_of(char kind) {
  switch (kind) {
    case 'c': return &ffi_type_sint8;
    case 'C': return &ffi_type_uint8;
    case 's': return &ffi_type_sint16;
    case 'S': return &ffi_type_uint16;
    case 'I': return &ffi_type_sint32;
    case 'U': return &ffi_type_uint32;
    case 'L': return &ffi_type_sint64;
    case 'Q': return &ffi_type_uint64;
    case 'F': return &ffi_type_float;
    case 'D': return &ffi_type_double;
    case 'P': return &ffi_type_pointer;
    case 'V': return &ffi_type_void;
    default: return 0;
  }
}

typedef union {
  int8_t c; uint8_t C; int16_t s; uint16_t S; int32_t I; uint32_t U;
  int64_t L; uint64_t Q; float F; double D; void *P;
  ffi_arg unsigned_result; ffi_sarg signed_result;
} slot;

/* Call a function. Gives 0, or -1 if the type cannot be called. */
int powder_foreign_call(void (*function)(void), const char *shape, char result,
                        const int64_t *whole, const double *real,
                        int64_t *whole_out, double *real_out) {
  size_t count = strlen(shape);
  ffi_type *types[count + 1];
  void *values[count + 1];
  slot slots[count + 1];
  for (size_t i = 0; i < count; i++) {
    types[i] = type_of(shape[i]);
    values[i] = &slots[i];
    if (types[i] == 0 || shape[i] == 'V') return -1;
    switch (shape[i]) {
      case 'c': slots[i].c = (int8_t)whole[i]; break;
      case 'C': slots[i].C = (uint8_t)whole[i]; break;
      case 's': slots[i].s = (int16_t)whole[i]; break;
      case 'S': slots[i].S = (uint16_t)whole[i]; break;
      case 'I': slots[i].I = (int32_t)whole[i]; break;
      case 'U': slots[i].U = (uint32_t)whole[i]; break;
      case 'L': slots[i].L = whole[i]; break;
      case 'Q': slots[i].Q = (uint64_t)whole[i]; break;
      case 'F': slots[i].F = (float)real[i]; break;
      case 'D': slots[i].D = real[i]; break;
      default: slots[i].P = (void *)(intptr_t)whole[i]; break;
    }
  }
  ffi_type *gives = type_of(result);
  ffi_cif cif;
  if (gives == 0 || ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned)count, gives, types) != FFI_OK) return -1;
  slot out;
  memset(&out, 0, sizeof out);
  errno = kept_errno;
  ffi_call(&cif, function, &out, values);
  kept_errno = errno;
  switch (result) {
    case 'D': *real_out = out.D; break;
    case 'F': *real_out = out.F; break;
    case 'V': break;
    case 'L': *whole_out = out.L; break;
    case 'Q': *whole_out = (int64_t)out.Q; break;
    case 'P': *whole_out = (int64_t)(intptr_t)out.P; break;
    /* A narrow result comes back widened to a whole word. */
    case 'c': case 's': case 'I': *whole_out = (int64_t)out.signed_result; break;
    default: *whole_out = (int64_t)out.unsigned_result; break;
  }
  return 0;
}
