/**********************/
/*  IA32 memory types */
/**********************/

#include <linux/mm.h>

/* Those x86 specific prototypes were not found in linux-headers package */
int set_memory_uc(unsigned long addr,int numpages);
int set_memory_wc(unsigned long addr,int numpages);
int set_memory_wb(unsigned long addr,int numpages);
int set_pages_array_wt(struct page **pages,int numpages);

typedef enum { PAT_UC, PAT_WC, PAT_WT, PAT_WP, PAT_WB, } pat_t ;

static size_t opages(size_t sz) {
  size_t r=0 ; size_t max=PAGE_SIZE;
  while (sz > max) {
    max *= 2; r++;
  }
  return r;
}

static int klitmus_set_memory(pat_t pat, unsigned long addr, int numpages) {
  switch (pat) {
  case PAT_UC:
    return set_memory_uc(addr,numpages);
  case PAT_WC:
    return set_memory_wc(addr,numpages);
  case PAT_WT:
    {
      struct page **pages = kmalloc_array(sizeof(*pages),numpages,GFP_KERNEL);
      if (!pages) return -1;
      for (int k=0 ; k < numpages ; k++) {
        pages[k] = virt_to_page(addr);
        addr += PAGE_SIZE;
      }
      int ret = set_pages_array_wt(pages,numpages);
      kfree(pages);
      return ret ;
    }
  case PAT_WB:
    return set_memory_wb(addr,numpages);
  case PAT_WP:
  default:
    return -1; /* Should not happen */
  }
}

static void klitmus_free_pat(void *p,size_t sz) {
  int op = opages(sz);
  (void)set_memory_wb((unsigned long)p,1 << op);
  free_pages((unsigned long)p,op);
}

static void *klitmus_alloc_pat(pat_t pat, size_t sz) {
  int op = opages(sz);
  void *r = (void *)__get_free_pages(GFP_KERNEL,op);
  if (!r) return r;
  if (klitmus_set_memory(pat,(unsigned long)r,1 << op)) {
    klitmus_free_pat(r,sz);
    return NULL;
  }
  return r;
}
