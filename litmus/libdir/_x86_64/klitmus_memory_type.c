/**********************/
/*  IA32 memory types */
/**********************/

/* Those x86 specific prototypes are not available in linux-headers package */

int set_memory_uc(unsigned long addr,int numpages);
int set_memory_wc(unsigned long addr,int numpages);
int set_memory_wt(unsigned long addr,int numpages);
int set_memory_wp(unsigned long addr,int numpages);
int set_memory_wb(unsigned long addr,int numpages);

typedef enum { PAT_UC, PAT_WC, PAT_WT, PAT_WP, PAT_WB, } pat_t ;

static size_t npages(size_t sz) {
  size_t r=0 ; size_t max=PAGE_SIZE;
  while (sz > max) {
    max *= 2; r++;
  }
  return r;
}

static int klitmus_set_memory(pat_t pat, unsigned long addr, int numpages) {
  switch (pat) {
  case PAT_UC:
    return set_memory_uc(addr, numpages);
  case PAT_WC:
    return set_memory_wc(addr, numpages);
  case PAT_WT:
    return set_memory_wt(addr, numpages);
  case PAT_WP:
    return set_memory_wp(addr, numpages);
  case PAT_WB:
    return set_memory_wb(addr, numpages);
  default:
    return -1; /* Should not happen */
  }
}

static void klitmus_free_pat(void *p,size_t sz) {
  int np = npages(sz);
  (void)set_memory_wb((unsigned long)p,1 << np);
  free_pages((unsigned long)p,np);
}

static void *klitmus_alloc_pat(pat_t pat, size_t sz) {
  int np = npages(sz);
  void *r = (void *)__get_free_pages(GFP_KERNEL,np);
  if (!r) return r;
  if (klitmus_set_memory(pat,(unsigned long)r,1 << np)) {
    klitmus_free_pat(r,sz);
    return NULL;
  }
  return r;
}
