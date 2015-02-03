inline static void mbar(void) {
  asm __volatile__ ("dsb sy" ::: "memory");
}
