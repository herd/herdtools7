
/* Generic type of symbolic value generated from the final state of a litmus
 * test or the FAR_EL1 register in case of a fault.
 *
 * This type register all the data used for the pretty printing and checks of
 * the final values like the `id` of the variable, it's `offset` and the raw
 * pointer for a future usage in presence of a non-canonical MTE or PAC field
 */
typedef struct {
  intmax_t* raw; // raw pointer value
  uintptr_t offset; // offset between the raw value and the symbolic value
  int id; // id of the symbolic value
} symb_t;

static symb_t symbolic_of_id(int id) {
  symb_t ret = { .id = id, .offset = 0 };
  return ret;
}

static symb_t unknown_symbolic() {
  return symbolic_of_id(DATA_SYMB_ID_UNKNOWN);
}

static int symbolic_equal(symb_t s1, symb_t s2) {
  return s1.id == s2.id && s1.offset == s2.offset;
}

static void pp_symbolic(symb_t s) {
  printf("%s", data_symb_name[s.id]);

  if (s.offset != 0)
    printf("+%ld", s.offset);
}

