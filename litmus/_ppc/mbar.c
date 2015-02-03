inline static void mbar(void) {
  asm __volatile__ ("sync" ::: "memory");
}
