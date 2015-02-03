inline static tb_t read_timebase(void) {
  uint32_t a,d; ;
  asm __volatile__ ("rdtsc" : "=a" (a), "=d" (d)) ;
  return ((tb_t)a) | (((tb_t)d)<<32);
}
