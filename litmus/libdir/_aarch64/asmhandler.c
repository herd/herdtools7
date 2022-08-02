static void exceptions_init_test(void *p) {
  asm __volatile__ (
"msr vbar_el1,%0\n\t"
"isb\n"
:
: "r" (p)
);
}
