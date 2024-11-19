// Check if `FEAT_CONSTPACFIELD` is implemented
static int check_const_pac_field_variant(char* tname) {
  uint64_t isar2;
  asm volatile("mrs %[isar2], ID_AA64ISAR2_EL1": [isar2] "=r" (isar2));
  uint64_t isar2_pac = (isar2 >> 24) & 0b1111;

  switch (isar2_pac) {
    case 0b0001:
      return 1;
    default:
      printf("Test %s, CONSTPACFIELD not available on this system\n", tname);
      return 0;
  }
}
