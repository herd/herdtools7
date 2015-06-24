/* Cache flush for aarch64 ?? */
inline static void cache_flush(void *p) {
  asm __volatile__ ("dc civac,%[p]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch(void *p) {
  asm __volatile__ ("prfm pldl1keep,[%[p]]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch_store(void *p) {
  asm __volatile__ ("prfm pstl1keep,[%[p]]" :: [p] "r" (p) : "memory");
}
