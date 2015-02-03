/**********************/
/* User level barrier */
/**********************/


typedef struct {
  volatile int c,sense;
  int n ;
} sense_t;

static void barrier_init (sense_t *p,int n) {
  p->n = p->c = n;
  p->sense = 0;
}

static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1,r3;
asm __volatile__ (  
"dmb\n\t"
"0:\n\t"
"ldrex %[r1],[%[c]]\n\t"
"sub %[r1],%[r1],#1\n\t"
"strex %[r3],%[r1],[%[c]]\n\t"
"cmp %[r3],#0\n\t"
"bne 0b\n\t"
"cmp %[r1],#0\n\t"
"bne 1f\n\t"
"dmb\n\t"
"str %[n],[%[c]]\n\t"
"dmb\n\t"
"mvns %[r1],%[ms]\n\t"
"str %[r1],[%[s]]\n\t"
"dsb\n\t"
"b 2f\n\t"
"1:\n\t"
"ldr %[r1],[%[s]]\n\t"
"cmp %[r1],%[ms]\n\t"
"beq 1b\n\t"
"2:\n\t"
"dmb\n\t"
: [r1] "=&r" (r1), [r3] "=&r" (r3)
: [c] "r" (&p->c), [s] "r" (&p->sense), [ms] "r" (sense), [n] "r" (p->n)
: "r0","memory") ;
}
