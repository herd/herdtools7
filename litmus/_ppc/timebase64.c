inline static tb_t read_timebase(void) {
  tb_t r;
  asm __volatile__ ("mftb %[r1]" :[r1] "=r" (r) : : "memory");
  return r;
}
