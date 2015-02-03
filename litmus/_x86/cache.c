inline static void cache_flush(void *p) {
  asm __volatile__ ("clflush 0(%[p])" :: [p] "r" (p) : "memory");
}


inline static void cache_touch(void *p) {
  asm __volatile__ ("prefetcht0 0(%[p])" :: [p] "r" (p) : "memory");
}


inline static void cache_touch_store(void *p) {
/* Did not find how to announce intention to store for x86 */
  asm __volatile__ ("prefetcht0 0(%[p])" :: [p] "r" (p) : "memory");
}

