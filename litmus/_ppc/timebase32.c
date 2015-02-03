inline static tb_t read_timebase(void) {
  tb_t r;
  uint32_t r1,r2,r3;
asm __volatile__ (
"0:\n\t"
"mftbu %[r1]\n\t"
"mftb %[r2]\n\t"
"mftbu %[r3]\n\t"
"cmpw %[r1],%[r3]\n\t"
"bne 0b\n\t"
:[r1] "=r" (r1), [r2] "=r" (r2), [r3] "=r" (r3)
: :"memory" );
  r = r2;
  r |= ((tb_t)r1) << 32;
  return r;
}

