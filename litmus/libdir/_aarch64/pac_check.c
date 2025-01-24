/**********************************/
/* Check PAC implemented features */
/**********************************/

// Check any implemented PAC
static int pac_check(char *tname) {
  uint64_t isar1;
  asm volatile("mrs %x0, id_aa64isar1_el1": "=r" (isar1));
  uint64_t isar1_api = (isar1 >> 8) & 0b1111;
  if (isar1_api == 0) {
    printf("Test %s, PAC not available on this system\n",tname);
    return 0;
  }
  return 1;
}
