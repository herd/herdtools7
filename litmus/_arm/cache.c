/* No cache flush for ARM */
inline static void cache_flush(void *p) { }

inline static void cache_touch(void *p) {
  asm __volatile__ ("pld [%[p]]" :: [p] "r" (p) : "memory");
}

#ifdef HAS_PLDW
inline static void cache_touch_store(void *p) {
/* to get pldw: -mcpu=cortex-a9 -marm */
  asm __volatile__ ("pldw [%[p]]" :: [p] "r" (p) : "memory");
}
#else
inline static void cache_touch_store(void *p) { cache_touch(p); }
#endif
