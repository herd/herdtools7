#ifndef _PRELUDE_SIZE_H
#define _PRELUDE_SIZE_H
#include <instruction.h>
#include <stddef.h>
extern ins_t nop; /* shared by ALL test cases */

size_t prelude_size(ins_t *p);
#endif
