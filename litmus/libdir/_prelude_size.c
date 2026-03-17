#include <_find_ins.h>
#include <_prelude_size.h>
ins_t nop;

size_t prelude_size(ins_t *p) { return find_ins(nop,p,0); }
