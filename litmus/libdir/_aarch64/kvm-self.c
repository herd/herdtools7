static void litmus_icache_sync(unsigned long vaddr, unsigned long vaddr_end)
{
  while (vaddr < vaddr_end) {
    selfbar((void *)vaddr);
    vaddr += cache_line_size;
  }
}

static size_t code_size(ins_t *p,int skip) {
  return (find_ins(getret(), p, skip) + 1) * sizeof(ins_t);
}

static void litmus_pte_unset_el0(unsigned long vaddr, unsigned long vaddr_end)
{
  while (vaddr < vaddr_end) {
    pteval_t *pte = litmus_tr_pte((void *)vaddr);
    litmus_set_pte_safe((void *)vaddr, pte, *pte & ~msk_el0);
    vaddr += LITMUS_PAGE_SIZE;
  }
  asm __volatile__ (
    "dsb ish\n\t"
    "isb"
    ::: "memory"
  );
}

static void code_init(void *code, void *src, size_t sz)
{
  size_t sz_pages = (sz - 1) / LITMUS_PAGE_SIZE + 1;
  memcpy(code, src, sz);
  litmus_icache_sync((unsigned long)code, (unsigned long)code + sz);
  /* Armv8 requires that code shared between EL1 and EL0 is read-only.
   * We need write access and therefore, we have to ensure that the
   * the memory we use for code won't be accessible from EL0 */
  litmus_pte_unset_el0((unsigned long)code, (unsigned long)code + sz_pages);
}
