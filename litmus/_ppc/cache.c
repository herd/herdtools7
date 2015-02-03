inline static void cache_flush(void *p) {
asm __volatile__ ("dcbf 0,%[p]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch(void *p) {
asm __volatile__ ("dcbt 0,%[p]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch_store(void *p) {
asm __volatile__ ("dcbtst 0,%[p]" :: [p] "r" (p) : "memory");
}
