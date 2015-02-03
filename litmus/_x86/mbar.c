inline static void mbar(void) {
  asm __volatile__ ("mfence" ::: "memory");
}
