#include <caml/mlvalues.h>

CAMLprim value get_size_t_size ( value v) {
  return Val_int(sizeof(size_t));
}

CAMLprim value get_int_size ( value v ) {
  return Val_int(sizeof(int));
}
