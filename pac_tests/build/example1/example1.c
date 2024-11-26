/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* This C source is a product of litmus7 and includes source that is        */
/* governed by the CeCILL-B license.                                        */
/****************************************************************************/
/* Parameters */
#define OUT 1
#define SIZE_OF_TEST 5000
#define NUMBER_OF_RUN 200
#define AVAIL 4
#define N 2
#define NVARS 2
#define NEXE 2
#define NTHREADS 4
#define NOCCS 1
/* Includes */
#ifdef __ARM_NEON
#include <arm_neon.h>
#define cast(v)			\
({					\
    typeof(v) __in = (v);		\
    int32x4_t __out;			\
    asm("" : "=w"(__out) : "0"(__in));	\
     __out;				\
})
#endif /* __ARM_NEON */
#ifdef __ARM_FEATURE_SVE
#include <arm_sve.h>
#endif /* __ARM_FEATURE_SVE */
#ifdef __ARM_FEATURE_SME
#include <arm_sme.h>
#endif
#define STATS 1
#define KVM 1
#include <libcflat.h>
#include "kvm-headers.h"
#include "utils.h"

typedef uint32_t count_t;
#define PCTR PRIu32


typedef uint32_t ins_t; /* Type of instructions */

/***************************/
/* Get instruction opcodes */
/***************************/

static ins_t getnop(void) {
  ins_t *x1;
  ins_t r;
  asm __volatile__ (
    "adr %[x1],0f\n\t"
    "ldr %w[x2],[%[x1]]\n\t"
    "b 1f\n"
    "0:\n\t"
    "nop\n"
    "1:\n"
    :[x1] "=&r" (x1),[x2] "=&r" (r)
    :
    : "cc", "memory"
);
  return r;
}


static ins_t nop;


// Find index of some instruction in code, skipping 'skip' occurrences
static size_t find_ins(ins_t opcode,ins_t *p,int skip) {
  ins_t *q = p;

  for  ( ; *q != opcode || (skip-- > 0); q++);
  return q-p ;
}

/* Cache flush/fetch instructions */
#define CACHE_FLUSH 1
inline static void cache_flush(void *p) {
#ifdef CACHE_FLUSH
  asm __volatile__ ("dc civac,%[p]" :: [p] "r" (p) : "memory");
#endif
}

inline static void cache_touch(void *p) {
  asm __volatile__ ("prfm pldl1keep,[%[p]]" :: [p] "r" (p) : "memory");
}

#ifdef CACHE_TOUCH_STORE
inline static void cache_touch_store(void *p) {
  asm __volatile__ ("prfm pstl1keep,[%[p]]" :: [p] "r" (p) : "memory");
}
#endif



typedef struct {
  volatile int c,sense;
  int n ;
} sense_t;

static void barrier_init (sense_t *p,int n) {
  p->n = p->c = n;
  p->sense = 0;
}

/*
barrier_wait:
	ldr	w2, [x0,4]
.L28:
	ldaxr	w1, [x0]
	sub	w1, w1, #1
	stlxr	w3, w1, [x0]
	cbnz	w3, .L28
	cmp	w1, wzr
	ble	.L24
.L26:
	ldr	w1, [x0,4]
	cmp	w1, w2
	beq	.L26
	dmb	ish
.L24:
	ldr	w1, [x0,8]
	str	w1, [x0]
	mov	w1, 1
	dmb	ish
	sub	w2, w1, w2
	str	w2, [x0,4]
	dmb	ish
	ret
*/
static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1,r3 ;
asm __volatile (
"dmb sy\n"
"0:\n\t"
"ldxr %w[r1],%[c]\n\t"
"sub %w[r1],%w[r1],#1\n\t"
"stxr %w[r3],%w[r1],%[c]\n\t"
"cbnz %w[r3],0b\n\t"
"cmp %w[r1],wzr\n\t"
"ble 2f\n\t"
"dmb sy\n"
"1:\n\t"
"ldr %w[r1],%[s]\n\t"
"cmp %w[r1],%w[ms]\n\t"
"beq 1b\n\t"
"b 3f\n"
"2:\n\t"
"dmb sy\n\t"
"ldr %w[r1],%[n]\n\t"
"str %w[r1],%[c]\n\t"
"mov %w[r3],#1\n\t"
"dmb sy\n\t"
"sub %w[ms],%w[r3],%w[ms]\n\t"
"str %w[ms],%[s]\n"
"3:\n\t"
"dsb sy\n\t"
: [r1] "=&r" (r1), [r3] "=&r" (r3)
: [c] "m" (p->c), [s] "m" (p->sense), [ms] "r" (sense), [n] "m" (p->n)
: "memory") ;
}
#if 0
/* C version */
static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  __sync_synchronize() ;
  int rem = __sync_add_and_fetch(&p->c,-1) ;
  if (rem > 0) {
    while (p->sense == sense) ;
    __sync_synchronize() ;
  } else {
    p->c = p->n ;
    __sync_synchronize() ;
    p->sense = 1-sense ;
    __sync_synchronize() ;
  }
}
#endif
/************/
/* Topology */
/************/

/*
 Topology: {{{0}, {1}, {2}, {3}}}
*/

static const int inst[] = {
// [[0],[1]]
1, 1, 0, 0,
};

static const int role[] = {
// [[0],[1]]
1, 0, 1, 0,
};

static const char *group[] = {
"[[0],[1]]",
};

#define SCANSZ 1
#define SCANLINE 4

typedef struct {
  ins_t *ret[N];
} labels_t;

/************/
/* Outcomes */
/************/
#define SOME_VARS 1
typedef struct {
  intmax_t *y,*x;
  pteval_t *pte_y,saved_pte_y,*pte_x,saved_pte_x;
  labels_t labels;
} vars_t;


typedef struct {
  int out_1_x0;
  int out_1_x2;
} log_t;

/* Dump of outcome */
static void pp_log(FILE *chan,log_t *p) {
  printf("1:X0=%d;",p->out_1_x0);
  printf(" 1:X2=%d;",p->out_1_x2);
}

/* Equality of outcomes */
static int eq_log(log_t *p,log_t *q) {
  return
    p->out_1_x0 == q->out_1_x0 &&
    p->out_1_x2 == q->out_1_x2 &&
    1;
}

/* Fault Handling */
#define HAVE_FAULT_HANDLER 1

typedef struct { int instance,proc; } who_t;

static count_t nfaults[NTHREADS];
static who_t whoami[AVAIL];

typedef uint32_t ins_t; /* Type of instructions */

#define PRECISE 1

static vars_t *vars_ptr[NEXE];
static inline int log_fault(int proc, int instr_symb, int data_symb, int ftype)
{
  return 0;
}


static void record_fault(who_t *w, unsigned long pc, unsigned long esr) {
#ifdef SEE_FAULTS
  th_faults_info_t *th_flts = &th_faults[w->instance][w->proc];
  labels_t *lbls = &vars_ptr[w->instance]->labels;
  fault_info_t flt;
  unsigned long far;

  flt.instr_symb = get_instr_symb_id(lbls, (ins_t *)pc);
  if (get_far(esr, &far)) {
    flt.data_symb = idx_addr((intmax_t *)far, vars_ptr[w->instance]);
  } else {
    flt.data_symb = DATA_SYMB_ID_UNKNOWN;
  }
  flt.type = get_fault_type(esr);

  if (!log_fault(w->proc, flt.instr_symb, flt.data_symb, flt.type)) {
    return;
  }

  if (exists_fault(th_flts, flt.instr_symb, flt.data_symb, flt.type))
    return;

  if (th_flts->n < MAX_FAULTS_PER_THREAD) {
    th_flts->faults[th_flts->n++] = flt;
  }
#endif
}

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info();
  who_t *w = &whoami[ti->cpu];
  atomic_inc_fetch(&nfaults[w->proc]);

  record_fault(w, regs->pc, esr);
#ifdef PRECISE
  labels_t *lbls = &vars_ptr[w->instance]->labels;
  regs->pc = (u64)lbls->ret[w->proc];
#else
#ifdef FAULT_SKIP
  regs->pc += 4;
#endif
#endif
}

static void install_fault_handler(int cpu) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1, fault_handler);
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_UNKNOWN, fault_handler);
#ifdef USER_MODE
  struct thread_info *ti = thread_info_sp(user_stack[cpu]);
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_DABT_EL0] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_UNKNOWN] = fault_handler;
#endif
}

static void pp_faults(void) {
  count_t total=0;
  for (int k=0 ; k < NTHREADS; k++) { total += nfaults[k]; }
  if (total > 0) {
    printf("Faults example %"PCTR"",total);
    for (int k = 0 ; k < NTHREADS ; k++) {
      count_t c = nfaults[k];
      if (c > 0) printf(" P%d:%"PCTR"",k,c);
    }
    printf("\n");
  }
}

static void exceptions_init_test(void *p) {
  asm __volatile__ (
"msr vbar_el1,%0\n\t"
"isb\n"
:
: "r" (p)
);
}

static void set_fault_vector(int role) { }

inline static int final_cond(log_t *p) {
  switch (p->out_1_x0) {
  case 1:
    switch (p->out_1_x2) {
    case 0:
      return 1;
    default:
      return 0;
    }
  default:
    return 0;
  }
}

inline static int final_ok(int cond) {
  return cond;
}


/**************/
/* Parameters */
/**************/

typedef enum { cignore, cflush, ctouch, cmax, } dir_t;

typedef struct {
  int part;
  int c_0_x,c_0_y,c_1_x,c_1_y;
} param_t;

static param_t param = {-1, -1, -1, -1, -1,};

static int id(int x) { return x; }

static parse_param_t parse[] = {
  {"part",&param.part,id,SCANSZ},
  {"c_0_x",&param.c_0_x,id,cmax},
  {"c_0_y",&param.c_0_y,id,cmax},
  {"c_1_x",&param.c_1_x,id,cmax},
  {"c_1_y",&param.c_1_y,id,cmax},
};

#define PARSESZ (sizeof(parse)/sizeof(parse[0]))

static void pp_param(FILE *out,param_t *p) {
  printf("{part=%d, c_0_x=%d, c_0_y=%d, c_1_x=%d, c_1_y=%d}",p->part,p->c_0_x,p->c_0_y,p->c_1_x,p->c_1_y);
}

typedef struct {
  count_t groups[SCANSZ];
  count_t vars;
  count_t delays;
  count_t dirs[cmax][cmax][cmax][cmax];
} stats_t;

#define HASHSZ 19


/* Notice: this file contains public domain code by Bob Jenkins */

typedef struct {
  log_t key ;
#ifdef STATS
  param_t p ;
#endif
  count_t c ;
  int ok ;
} entry_t ;

static void pp_entry(FILE *out,entry_t *p, int verbose, const char **group) ;

typedef struct {
  int nhash ;
  entry_t t[HASHSZ] ;
} hash_t ;

static void pp_hash(FILE *fp,hash_t *t,int verbose,const char **group) {
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0) {
      pp_entry(fp,p,verbose,group) ;
    }
  }
}

#if 0
static void pp_hash_ok(FILE *fp,hash_t *t,char **group) {
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0 && p->ok) pp_entry(fp,p,1,group) ;
  }
}
#endif

static void log_init(log_t *p) {
  uint32_t *q = (uint32_t *)p ;

  for (int k = sizeof(log_t)/sizeof(uint32_t) ; k > 0 ; k--)
    *q++ = -1 ;
}

static void hash_init(hash_t *t) {
  t->nhash = 0 ;
  for (int k = 0 ; k < HASHSZ ; k++) {
    t->t[k].c = 0 ;
    log_init(&t->t[k].key) ;
  }
}

/*
 Bob Jenkins hashword function, (Public domain)
 See <http://burtleburtle.net/bob/hash/doobs.html>
*/

#define rot(x,k) (((x)<<(k)) | ((x)>>(32-(k))))

#define mix(a,b,c) \
{ \
  a -= c;  a ^= rot(c, 4);  c += b; \
  b -= a;  b ^= rot(a, 6);  a += c; \
  c -= b;  c ^= rot(b, 8);  b += a; \
  a -= c;  a ^= rot(c,16);  c += b; \
  b -= a;  b ^= rot(a,19);  a += c; \
  c -= b;  c ^= rot(b, 4);  b += a; \
}

#define final(a,b,c) \
{ \
  c ^= b; c -= rot(b,14); \
  a ^= c; a -= rot(c,11); \
  b ^= a; b -= rot(a,25); \
  c ^= b; c -= rot(b,16); \
  a ^= c; a -= rot(c,4);  \
  b ^= a; b -= rot(a,14); \
  c ^= b; c -= rot(b,24); \
}

static uint32_t hashword(
const uint32_t *k,                   /* the key, an array of uint32_t values */
size_t          length)              /* the length of the key, in uint32_ts */
{
  uint32_t a,b,c;

  /* Set up the internal state */
  a = b = c = 0xdeadbeef ;

  /*------------------------------------------------- handle most of the key */
  while (length > 3)
  {
    a += k[0];
    b += k[1];
    c += k[2];
    mix(a,b,c);
    length -= 3;
    k += 3;
  }

  /*------------------------------------------- handle the last 3 uint32_t's */
  switch(length)                     /* all the case statements fall through */
  {
  case 3 : c+=k[2];
  case 2 : b+=k[1];
  case 1 : a+=k[0];
    final(a,b,c);
  case 0:     /* case 0: nothing left to add */
    break;
  }
  /*------------------------------------------------------ report the result */
  return c;
}

static uint32_t hash_log (log_t *key) {
  return hashword((uint32_t *)key,sizeof(log_t)/sizeof(uint32_t)) ;
}

#ifdef STATS
static int hash_add(hash_t *t,log_t *key, param_t *v,count_t c,int ok) {
#else
static int hash_add(hash_t *t,log_t *key, count_t c,int ok) {
#endif
  uint32_t h = hash_log(key) ;
  h = h % HASHSZ ;
  for (int k = 0 ; k < HASHSZ ;  k++) {
    entry_t *p = t->t + h ;
    if (p->c == 0) { /* New entry */
      p->key = *key ;
#ifdef STATS
      p->p = *v ;
#endif
      p->c = c ;
      p->ok = ok ;
      t->nhash++ ;
      return 1 ;
    } else if (eq_log(key,&p->key)) {
      p->c += c ;
      return 1;
    }
    h++ ;
    h %= HASHSZ ;
  }
  return 0;
}

static int hash_adds(hash_t *t, hash_t *f) {
  int r = 1;
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = f->t+k ;
    if (p->c > 0) {
#ifdef STATS
      int rloc = hash_add(t,&p->key,&p->p,p->c,p->ok) ;
#else
      int rloc = hash_add(t,&p->key,p->c,p->ok) ;
#endif
      r = r && rloc;
    }
  }
  return r;
}

static void pp_entry(FILE *out,entry_t *p, int verbose, const char **group) {
  printf("%-6" PCTR "%c>",p->c,p->ok ? '*' : ':');
  pp_log(out,&p->key);
  if (verbose) {
    puts(" # ");
    pp_param(out,&p->p);
    puts(" ");
    puts(group[p->p.part]);
  }
  printf("%c",'\n');
}

static void set_feature(int _role) { }
/*************/
/* Test code */
/*************/

typedef void (*code0_t)(int *x,int *y);

noinline static void code0(int *x,int *y) {
  int trashed_x0;
  int trashed_x2;
asm __volatile__ (
"\n"
"#START _litmus_P0\n"
"#_litmus_P0_0\n\t"
"nop\n"
"#_litmus_P0_1\n\t"
"mov %w[x0],#1\n"
"#_litmus_P0_2\n\t"
"str %w[x0],[%[x1]]\n"
"#_litmus_P0_3\n\t"
"mov %w[x2],#1\n"
"#_litmus_P0_4\n\t"
"dmb sy\n"
"#_litmus_P0_5\n\t"
"str %w[x2],[%[x3]]\n"
"#_litmus_P0_6\n\t"
"nop\n"
"#END _litmus_P0\n"
:[x2] "=&r" (trashed_x2),[x0] "=&r" (trashed_x0)
:[x1] "r" (x),[x3] "r" (y)
:"cc","memory"
);
}

typedef void (*code1_t)(int *x,int *y,int *out_1_x0,int *out_1_x2);

noinline static void code1(int *x,int *y,int *out_1_x0,int *out_1_x2) {
  int* trashed_x3;
asm __volatile__ (
"\n"
"#START _litmus_P1\n"
"#_litmus_P1_0\n\t"
"nop\n"
"#_litmus_P1_1\n\t"
"ldr %w[x0],[%[x1]]\n"
"#_litmus_P1_2\n\t"
"pacda %[x3],%[x3]\n"
"#_litmus_P1_3\n\t"
"ldr %w[x2],[%[x3]]\n"
"#_litmus_P1_4\n\t"
"nop\n"
"#END _litmus_P1\n"
:[x2] "=&r" (*out_1_x2),[x0] "=&r" (*out_1_x0),[x3] "=&r" (trashed_x3)
:"[x0]" ((int)(0)),[x1] "r" (y),"[x2]" ((int)(0)),"[x3]" (x),[x5] "r" ((int64_t)(0))
:"cc","memory"
);
}

/***************/
/* Memory size */
/***************/

/* Page size line */
#define LINE LITMUS_PAGE_SIZE

static void vars_init(vars_t *_vars,intmax_t *_mem) {
  const size_t _sz = LINE/sizeof(intmax_t);
  pteval_t *_p;

  _vars->y = _mem;
  _vars->pte_y = _p = litmus_tr_pte((void *)_mem);
  _vars->saved_pte_y = *_p;
  _mem += _sz ;
  _vars->x = _mem;
  _vars->pte_x = _p = litmus_tr_pte((void *)_mem);
  _vars->saved_pte_x = *_p;
  _mem += _sz ;
}

static void vars_free(vars_t *_vars) {
}

static void labels_init(vars_t *_vars) {
  labels_t *lbls = &_vars->labels;
  lbls->ret[0] = ((ins_t *)code0)+find_ins(nop,(ins_t *)code0,1);
  lbls->ret[1] = ((ins_t *)code1)+find_ins(nop,(ins_t *)code1,1);
}


/************/
/* Instance */
/************/

typedef struct {
  int id ;
  intmax_t *mem;
  log_t out;
#ifdef SOME_PTR
  log_ptr_t out_ptr;
#endif
#ifdef SOME_VARS
  vars_t v;
#endif
#ifdef HAVE_TIMEBASE
  tb_t next_tb;
#endif
  hash_t t;
  sense_t b;
  param_t p;
  int ind[N]; /* Indirection for role shuffle */
  int stop_now;
} ctx_t ;


static void instance_init(ctx_t *p, int id, intmax_t *mem) {
  p->id = id;
  p->mem = mem;
  hash_init(&p->t);
  log_init(&p->out);
  barrier_init(&p->b,N);
  interval_init((int *)&p->ind,N);
  p->stop_now = 0;
#ifdef SOME_VARS
  vars_init(&p->v,mem);
  labels_init(&p->v);
#endif
}

static void instance_free(ctx_t *p) {
#ifdef SOME_VARS
  vars_free(&p->v);
#endif
}

/******************/
/* Global context */
/******************/

#define LINESZ (LINE/sizeof(intmax_t))
#define VOFFSZ (VOFF/sizeof(intmax_t))
#define MEMSZ ((NVARS*NEXE+1)*LINESZ)

#ifndef DYNALLOC
static intmax_t mem[MEMSZ] ;
#endif

typedef struct global_t {
  /* Command-line parameter */
  param_t *param ;
  parse_param_t *parse ;
  /* Topology */
  const int *inst, *role ;
  const char **group ;
  /* memory */
  intmax_t *mem ;
  /* Cache control */
#ifdef ACTIVE
  active_t *active;
#endif
  /* Runtime control */
  int verbose ;
  int size,nruns,nexe,noccs ;
  int delay,step ;
  int fix ;
  /* Indirection for shuffling all threads */
  int ind[AVAIL] ;
  /* Synchronisation for all threads */
  sense_t gb ;
  /* Count 'interesting' outcomes */
  volatile int ok ;
  /* Times for timeout */
  tsc_t start,now ;
  /* All instance contexts */
  ctx_t ctx[NEXE] ; /* All test instance contexts */
  hash_t hash ;     /* Sum of outcomes */
  int hash_ok;
#ifdef STATS
  /* statistics */
  stats_t stats ;
#endif
  /* Support for early exit when postcondition is observed */
  int speedcheck ;
  /* Flag to exit when postcondition is observed - only when speedcheck is enabled */
  int stop_now ;
} global_t ;


static void init_global(global_t *g) {
  g->param = &param;
  g->parse = &parse[0];
  g->inst = inst;
  g->role = role;
  g->group = group;
#ifdef ACTIVE
  g->active = active;
#endif
#ifdef TIMELIMIT
  /* Starting time */
  g->start = timeofday() ;
#endif
  /* Global barrier */
  barrier_init(&g->gb,AVAIL) ;
  /* Align  to cache line */
  uintptr_t x = (uintptr_t)(g->mem) ;
  x += LINE-1 ; x /=  LINE ; x *= LINE ;
  intmax_t *m = (intmax_t *)x ;
  /* Instance contexts */
  for (int k = 0 ; k < NEXE ; k++) {
    instance_init(&g->ctx[k],k,m) ;
    m += NVARS*LINESZ ;
  }
  g->hash_ok = 1;
}

static void free_global(global_t *g) {
  for (int k = 0 ; k < NEXE ; k++) {
    instance_free(&g->ctx[k]);
  }
#ifdef DYNALLOC
  free(g->mem);
  free(g);
#endif
}

/******************/
/* Thread context */
/******************/

typedef struct {
  int id ;
  st_t seed ;
  int role,inst ;
  ctx_t *ctx ;
#ifdef ACTIVE
  active_t *act;
#endif
} thread_ctx_t ;


static void set_role(global_t *g,thread_ctx_t *c,int part) {
  barrier_wait(&g->gb) ;
  int idx = SCANLINE*part+g->ind[c->id] ;
  int inst = g->inst[idx] ;
  if (0 <= inst && inst < g->nexe) {
    c->inst = inst ;
    c->ctx = &g->ctx[inst] ;
    c->role = g->role[idx] ;
    if (SCANSZ > 1 && !g->fix) {
    /* Shuffle roles in case several topological placements are possible. */
      ctx_t *d = c->ctx ;
      if (c->role == 0) interval_shuffle(&c->seed,(int *)&d->ind,N);
      barrier_wait(&d->b) ;
      c->role = d->ind[c->role] ;
    }
#ifdef KVM
    set_feature(c->role) ;
#ifdef HAVE_FAULT_HANDLER
    whoami[c->id].instance = inst ;
    whoami[c->id].proc = c->role ;
#if defined(SEE_FAULTS) || defined(PRECISE)
    if (c->role == 0) {
#ifdef SOME_VARS
      vars_ptr[inst] = &c->ctx->v;
#else
      vars_ptr[inst] = NULL;
#endif
#ifdef SEE_FAULTS
      th_faults[inst] = c->ctx->out.th_faults;
#endif
    }
#endif
#endif
#endif
#ifdef ACTIVE
    c->act = &g->active[part] ;
#endif
  } else {
    c->ctx = NULL ;
    c->role = -1 ;
  }
#ifdef KVM
  set_fault_vector(c->role);
#endif
  barrier_wait(&g->gb) ;
}

static void init_getinstrs(void) {
  nop = getnop();
}

inline static int do_run(thread_ctx_t *_c, param_t *_p,global_t *_g) {
  int _ok = 0;
  int _role = _c->role;
  if (_role < 0) return _ok;
  ctx_t *_ctx = _c->ctx;
  sense_t *_b = &_ctx->b;
  log_t *_log = &_ctx->out;
  vars_t *_vars = &_ctx->v;
  int *y = (int *)_vars->y;
  int *x = (int *)_vars->x;
  barrier_wait(_b);
  switch (_role) {
  case 0: {
    *y = 0;
    litmus_flush_tlb((void *)y);
    barrier_wait(_b);
    if (_p->c_0_x == ctouch) cache_touch((void *)x);
    else if (_p->c_0_x == cflush) cache_flush((void *)x);
    if (_p->c_0_y == ctouch) cache_touch((void *)y);
    else if (_p->c_0_y == cflush) cache_flush((void *)y);
    barrier_wait(_b);
    code0(x,y);
    barrier_wait(_b);
    (void)litmus_set_pte_safe(y,_vars->pte_y,_vars->saved_pte_y);
    litmus_flush_tlb((void *)y);
    int _cond = final_ok(final_cond(_log));
    int _added = hash_add(&_ctx->t,_log,_p,1,_cond);
    if (!_added && _g->hash_ok) _g->hash_ok = 0; // Avoid writing too much.
    if (_cond) {
      _ok = 1;
      (void)__sync_add_and_fetch(&_g->stats.groups[_p->part],1);
    }
    break; }
  case 1: {
    *x = 0;
    litmus_flush_tlb((void *)x);
    barrier_wait(_b);
    if (_p->c_1_x == ctouch) cache_touch((void *)x);
    else if (_p->c_1_x == cflush) cache_flush((void *)x);
    if (_p->c_1_y == ctouch) cache_touch((void *)y);
    else if (_p->c_1_y == cflush) cache_flush((void *)y);
    barrier_wait(_b);
    code1(x,y,&_log->out_1_x0,&_log->out_1_x2);
    barrier_wait(_b);
    (void)litmus_set_pte_safe(x,_vars->pte_x,_vars->saved_pte_x);
    litmus_flush_tlb((void *)x);
    break; }
  }
  return _ok;
}

/*******************/
/* Forked function */
/*******************/

inline static int comp_param (st_t *seed,int *g,int max,int delta) {
  int tmp = *g;
  return tmp >= 0 ? tmp : delta+rand_k(seed,max-delta);
}

static void choose_params(global_t *g,thread_ctx_t *c,int part) {
  int _role = c->role;
  if (_role < 0) return;
  ctx_t *ctx = c->ctx;
  param_t *q = g->param;

  for (int _s=0 ; _s < g->size; _s++) {
    barrier_wait(&ctx->b);
    switch (_role) {
    case 0:
      ctx->p.c_0_x = comp_param(&c->seed,&q->c_0_x,cmax,1);
      ctx->p.c_1_x = comp_param(&c->seed,&q->c_1_x,cmax,1);
      break;
    case 1:
      ctx->p.part = part;
      ctx->p.c_0_y = comp_param(&c->seed,&q->c_0_y,cmax,1);
      ctx->p.c_1_y = comp_param(&c->seed,&q->c_1_y,cmax,1);
      break;
    }
    int ok = do_run(c,&ctx->p,g);
    if (g->speedcheck) {
    /* Global stop */
      if (ok) g->stop_now = 1;
      /* Copy global stop */
      if (_role == 0) ctx->stop_now = g->stop_now;
      /* Synchronise, expecting ctx->stop_now update */
      barrier_wait(&ctx->b);
      if (ctx->stop_now) return;
    }
  }
}

static void choose(int id,global_t *g) {
  param_t *q = g->param;
  thread_ctx_t c; c.id = c.seed = id;
  st_t seed = 0; st_t seed0 = 0;

  for (int nrun = 0; nrun < g->nruns ; nrun++) {
    if (SCANSZ <= 1 && !g->fix && id == 0) {
    /* Shuffle all threads in absence of topology information. */
      interval_shuffle(&seed0,(int *)g->ind,AVAIL);
    }
    if (g->verbose>1) fprintf(stderr, "Run %d of %d\r", nrun, g->nruns);
    /* Select threads partition amounts SCANSZ */
    int part = q->part >= 0 ? q->part : rand_k(&seed,SCANSZ);
    set_role(g,&c,part);
    choose_params(g,&c,part);
    if (g->speedcheck) {
      /* Synchronise, expecting g->stop_now update */
      barrier_wait(&g->gb);
      if (g->stop_now) return;
    }
  }
}

typedef struct {
  int id;
  global_t *g;
} zyva_t;

static void zyva(void *_a) {
  int id = smp_processor_id();
  if (id >= AVAIL) return;
  zyva_t *a = (zyva_t*)_a + id;
  global_t *g = a->g;
  install_fault_handler(id);
  extern ins_t vector_table;
  exceptions_init_test(&vector_table);
  choose(id,g);
}

static int feature_check(void) {
  return 1;
}

#define RUN run
#define MAIN 1

#define ENOUGH 10

static void postlude(FILE *out,global_t *g,count_t p_true,count_t p_false,tsc_t total) {
  hash_t *hash = &g->hash ;
  puts("Test example Allowed\n");
  printf("Histogram (%d states)\n",hash->nhash);
  pp_hash(out,hash,g->verbose > 1,g->group);
  int cond = p_true > 0;
  puts(cond?"Ok":"No");
  puts("\n");
  puts("\nWitnesses\n");
  printf("Positive: %" PCTR ", Negative: %" PCTR "\n",p_true,p_false);
  puts("Condition ");
  puts("exists (1:X0=1 /\\ 1:X2=0)");
  puts(" is ");
  puts(cond ? "" : "NOT ");
  puts("validated\n");
  puts("Hash=f1229bb17a52cdde9077454f5cb11fa9\n");
  count_t cond_true = p_true;
  count_t cond_false = p_false;
  printf("Observation example %s %" PCTR " %" PCTR "\n",!cond_true ? "Never" : !cond_false ? "Always" : "Sometimes",cond_true,cond_false);
  if (p_true > 0) {
    count_t *ngroups = &g->stats.groups[0];
    for (int k = 0 ; k < SCANSZ ; k++) {
      count_t c = ngroups[k];
      if ((g->verbose > 1 && c > 0) || (c*100)/p_true > ENOUGH) {
        printf("Topology %-6" PCTR ":> part=%d %s\n",c,k,g->group[k]);
      }
    }
  }
  pp_faults();
  puts("Time example ");
  emit_millions(tsc_millions(total));
  puts("\n");
  if (!g->hash_ok) {
    puts("Warning: some hash table was full, some outcomes were not collected\n");
  }
}



/***************/
/* Entry point */
/***************/
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef DYNALLOC
static global_t global;
static zyva_t arg[AVAIL];
#ifndef KVM
static pthread_t th[AVAIL];
#endif
#endif

int RUN(int argc,char **argv,FILE *out) ;
int RUN(int argc,char **argv,FILE *out) {
  if (!feature_check()) {
    return -1;
  }
#ifdef DYNALLOC
#ifdef HAVE_FAULT_HANDLER
  alloc_fault_handler();
#if defined(SEE_FAULTS) || defined(PRECISE)
  alloc_see_faults();
#endif
#endif
  global_t *glo_ptr = malloc_check(sizeof(global_t));
  glo_ptr->mem = malloc_check(MEMSZ*sizeof(*glo_ptr->mem));
  zyva_t *arg = malloc_check(AVAIL*sizeof(*arg));
#ifndef KVM
  pthread_t *th = malloc_check(AVAIL*sizeof(*th));
#endif
#else
  global_t *glo_ptr = &global;
  glo_ptr->mem = mem;
#endif
  init_getinstrs();
  init_global(glo_ptr);
#ifdef OUT
#ifdef HAVE_TIMEBASE
  const int delta_tb = DELTA_TB;
#else
  const int delta_tb = 0;
#endif
  opt_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, AVAIL, NEXE, delta_tb, 0, 0 };
  opt_t d = def;
  char *prog = argv[0];
  char **p = parse_opt(argc,argv,&def,&d);
  int n_exe = d.n_exe;
  if (d.avail < AVAIL) n_exe = d.avail / N;
#ifdef HAVE_TIMEBASE
  if (d.delay < NSTEPS-1) d.delay = NSTEPS-1;
#endif
  if (n_exe < 1) n_exe = 1;
  if (n_exe > NEXE) n_exe = NEXE;
  glo_ptr->verbose = d.verbose;
  glo_ptr->nexe = n_exe;
  glo_ptr->nruns = d.max_run;
  glo_ptr->size = d.size_of_test;
#ifdef HAVE_TIMEBASE
  glo_ptr->delay = d.delay;
  glo_ptr->step = d.delay/(NSTEPS-1);
#endif
  glo_ptr->fix = d.fix;
  interval_init((int *)&glo_ptr->ind,AVAIL);
  if (glo_ptr->verbose) {
#ifdef NOSTDIO
    emit_string(stderr,prog);
    emit_string(stderr,": n=");
    emit_int(stderr,glo_ptr->nexe);
    emit_string(stderr,", r=");
    emit_int(stderr,glo_ptr->nruns);
    emit_string(stderr,", s=");
    emit_int(stderr,glo_ptr->size);
    emit_string(stderr,"\n");
#else
    fprintf(stderr,"%s: n=%d, r=%d, s=%d\n",prog,glo_ptr->nexe,glo_ptr->nruns,glo_ptr->size);
#endif
  }
  glo_ptr->speedcheck = d.speedcheck;
  glo_ptr->stop_now = 0;
  parse_param(prog,glo_ptr->parse,PARSESZ,p);
#ifdef PRELUDE
  prelude(out);
#endif
  tsc_t start = timeofday();
#endif
  for (int id=0; id < AVAIL; id++) {
    arg[id].id = id;
    arg[id].g = glo_ptr;
  }
#ifdef KVM
  on_cpus(zyva, arg);
#else
  for (int id=0; id < AVAIL ; id++) launch(&th[id],zyva,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);
#endif
  int nexe = glo_ptr->nexe ;
  hash_init(&glo_ptr->hash) ;
  for (int k=0 ; k < nexe ; k++) {
    glo_ptr->hash_ok = hash_adds(&glo_ptr->hash,&glo_ptr->ctx[k].t) && glo_ptr->hash_ok ;
  }
#ifdef OUT
  tsc_t total = timeofday()-start;
  count_t p_true = 0, p_false = 0;
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *e = &glo_ptr->hash.t[k];
    if (e->ok) {
      p_true += e->c;
    } else {
      p_false += e->c;
    }
  }
  postlude(out,glo_ptr,p_true,p_false,total);
#endif
  free_global(glo_ptr);
#ifdef DYNALLOC
#ifdef HAVE_FAULT_HANDLER
#if defined(SEE_FAULTS) || defined(PRECISE)
  free_see_faults();
#endif
  free_fault_handler();
#endif
  free(arg);
#ifndef KVM
  free(th);
#endif
#endif
  return EXIT_SUCCESS;
}

#ifdef MAIN
int main (int argc,char **argv) {
#ifdef KVM
  litmus_init();
#endif
  return RUN(argc,argv,stdout) ;
}
#endif
