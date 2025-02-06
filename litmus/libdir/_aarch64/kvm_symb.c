
/* Generic type of symbolic value generated from the final state of a litmus
 * test or the FAR_EL1 register in case of a fault.
 */
typedef struct {
  uintptr_t offset; // offset between the raw value and the symbolic value
  int id; // id of the symbolic value
} symb_t;

static symb_t symbolic_of_id(int id) {
  symb_t ret = { .id = id, .offset = 0 };
  return ret;
}

static int symbolic_equal(symb_t s1, symb_t s2) {
  return s1.id == s2.id && s1.offset == s2.offset;
}

// Pretty print a symbolic data, only print the
static const char* pp_symbolic(symb_t s) {
  char* buffer = (char*)malloc(sizeof(char) * 1024);
  if (s.offset == 0) snprintf(buffer, 1024, "%s", data_symb_name[s.id]);
  else snprintf(buffer, 1024, "%s+%ld", data_symb_name[s.id], s.offset);
  return buffer;
}

