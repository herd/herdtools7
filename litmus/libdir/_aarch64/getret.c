static ins_t getret(void) {
  ins_t *x1;
  ins_t r;
  asm __volatile__ (
  "adr %[x1],0f\n\t"
  "ldr %w[x2],[%[x1]]\n\t"
  "b 1f\n"
  "0:\n\t"
  "ret\n"
  "1:\n"
:[x1] "=&r" (x1),[x2] "=&r" (r)
:
: "cc","memory"
);
  return r;
}
