/**********************/
/* User level barrier */
/**********************/


typedef struct {
  volatile int c,sense;
} sense_t;

static void barrier_init (sense_t *p) {
  p->c = N;
  p->sense = 0;
}

static void barrier_wait(sense_t *p, int *mySense) {
  int r1,r2;
asm __volatile__ (  
"sync\n\t"
"lwz %[r2],0(%[ms])\n\t"
"sync\n\t"
"LBW0:\n\t"
"lwarx %[r1],0,%[c]\n\t"
"subi %[r1],%[r1],1\n\t"
"stwcx. %[r1],0,%[c]\n\t"
"bne LBW0\n\t"
"cmpwi %[r1],0\n\t"
"bne LBW1\n\t"
"sync\n\t"
"li %[r1],%[n]\n\t"
"stw %[r1],0(%[c])\n\t"
"sync\n\t"
"not %[r1],%[r2]\n\t"
"stw %[r1],0(%[s])\n\t"
"b LBW2\n\t"
"LBW1:\n\t"
"sync\n\t"
"lwz %[r1],0(%[s])\n\t"
"cmpw %[r1],%[r2]\n\t"
"beq LBW1\n\t"
"LBW2:\n\t"
"sync\n\t"
"not %[r1],%[r2]\n\t"
"stw %[r1],0(%[ms])\n\t"
"sync\n\t"
: [r1] "=&r" (r1), [r2] "=&r" (r2)
: [c] "r" (&p->c), [s] "r" (&p->sense), [ms] "r" (mySense), [n] "n" (N)
: "r0","memory") ;
}
