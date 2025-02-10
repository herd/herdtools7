
/* Generic type of symbolic value generated from the final state of a litmus
 * test or the FAR_EL1 register in case of a fault.
 */
typedef struct {
  uintptr_t offset; // offset between the raw value and the symbolic value
  int id; // id of the symbolic value
} symb_t;

inline static symb_t symbolic_of_id(int id) {
  symb_t ret = { .id = id, .offset = 0 };
  return ret;
}

inline static int symbolic_equal(symb_t s1, symb_t s2) {
  return s1.id == s2.id && s1.offset == s2.offset;
}

#define SYMBOLIC_BUFFER_SIZE 1024
static char symbolic_buffer[SYMBOLIC_BUFFER_SIZE];

// Pretty print a symbolic value and return it as a global string:
// Always use the result immediately after calling this function
// because calling it twice overwrite the result of the first call
inline static const char* pp_symbolic(symb_t s) {
  char* buffer = &symbolic_buffer[0];
  int len;
  if (s.offset == 0)
    len = snprintf(buffer, SYMBOLIC_BUFFER_SIZE, "%s", data_symb_name[s.id]);
  else
    len = snprintf(buffer, SYMBOLIC_BUFFER_SIZE, "%s+%ld", data_symb_name[s.id], s.offset);

  if (len < 0 || len >= SYMBOLIC_BUFFER_SIZE)
    fatal("invalid variable length");

  return buffer;
}

