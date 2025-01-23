// read the SCTLR_EL1 status register
uint64_t read_sctlr_el1() {
  uint64_t ret;
  asm __volatile__("mrs %[ret], SCTLR_EL1": [ret] "=r" (ret));
  return ret;
}

void init_pauth_key_ia() {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APIAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIAKeyLo_EL1, %[y]":: [y] "r" (y));
}

void init_pauth_key_ib() {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APIBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIBKeyLo_EL1, %[y]":: [y] "r" (y));
}

void init_pauth_key_da() {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APDAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDAKeyLo_EL1, %[y]":: [y] "r" (y));
}

void init_pauth_key_db() {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APDBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDBKeyLo_EL1, %[y]":: [y] "r" (y));
}

// update the SCTLR_EL1 status register
void write_sctlr_el1(uint64_t x) {
  asm __volatile__("msr SCTLR_EL1, %[x]":: [x] "r" (x));
}

// Initialize pointer authentication
void init_pauth() {
  uint64_t enIA = 1ULL << 31;
  uint64_t enIB = 1ULL << 30;
  uint64_t enDA = 1ULL << 27;
  uint64_t enDB = 1ULL << 13;
  write_sctlr_el1(enIA | enIB | enDA | enDB | read_sctlr_el1());
  init_pauth_key_ia();
  init_pauth_key_ib();
  init_pauth_key_da();
  init_pauth_key_db();
}
