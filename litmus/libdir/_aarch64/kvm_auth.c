// read the SCTLR_EL1 status register
static uint64_t read_sctlr_el1() {
  uint64_t ret;
  asm __volatile__("mrs %[ret], SCTLR_EL1": [ret] "=r" (ret));
  return ret;
}

static void init_pauth_key_ia() {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APIAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIAKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_ib() {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APIBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIBKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_da() {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APDAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDAKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_db() {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APDBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDBKeyLo_EL1, %[y]":: [y] "r" (y));
}

// update the SCTLR_EL1 status register
static void write_sctlr_el1(uint64_t x) {
  asm __volatile__("msr SCTLR_EL1, %[x]":: [x] "r" (x));
}

// Initialize pointer authentication
static void init_pauth() {
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

static intmax_t* strip_pauth(intmax_t* addr) {
  asm __volatile__("xpacd %[addr]" : [addr] "+r" (addr));
  return addr;
}

// Check if `FEAT_Pauth2` is implemented
static int check_pac_variant(char* tname) {
  uint64_t isar1;
  asm volatile("mrs %[isar1], ID_AA64ISAR1_EL1": [isar1] "=r" (isar1));
  uint64_t isar1_api = (isar1 >> 8) & 0b1111;

  switch (isar1_api) {
    case 0b0100:
    case 0b0011:
    case 0b0101:
      return 1;
    default:
      printf("Test %s, PAC not available on this system\n", tname);
      return 0;
  }
}

// Check if `FEAT_FPAC` is implemented iff `present`:
// FPAC change the way `aut*` is executed in case of failure
static int check_fpac_variant(char* tname, int present) {
  uint64_t isar1;
  asm volatile("mrs %[isar1], ID_AA64ISAR1_EL1": [isar1] "=r" (isar1));
  uint64_t isar1_api = (isar1 >> 8) & 0b1111;

  switch (isar1_api) {
    case 0b0100:
    case 0b0101:
      if (!present)
        printf("Test %s, FPAC is implemented on this system\n", tname);
      return present;
    default:
      if (present)
        printf("Test %s, FPAC not implemented on this system\n", tname);
      return !present;
  }
}
