#include "config.h"
/** Generated from header.txt **/
/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* This C source is a product of litmus7 and includes source that is        */
/* governed by the CeCILL-B license.                                        */
/****************************************************************************/
/** Generated from dump_header **/
/* Parameters */
#define OUT 1
#define SIZE_OF_TEST CONFIG_SIZE_OF_TEST
#define NUMBER_OF_RUN CONFIG_NUMBER_OF_RUN
#define AVAIL CONFIG_AVAIL
#define N CONFIG_N
#define NVARS CONFIG_NVARS
#define NEXE CONFIG_NEXE
#define NTHREADS CONFIG_NTHREADS
#define NOCCS CONFIG_NOCCS
#ifdef CONFIG_TIMEBASE
#define DELTA_TB CONFIG_DELTA
#endif
/* Includes */
#ifdef CONFIG_DO_DYNALLOC
#define DYNALLOC 1
#endif
#ifdef CONFIG_DO_STATS
#define STATS 1
#endif
#ifdef CONFIG_IS_KVM
#define KVM 1
#include <libcflat.h>
#include "kvm-headers.h"
#include "utils.h"
#ifndef CONFIG_STDIO
#include "litmus_io.h"
#define NOSTDIO 1
#endif
#else
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <time.h>
#include <limits.h>
#ifdef CONFIG_STDIO
#include <stdio.h>
#else
#include "litmus_io.h"
#define NOSTDIO 1
#endif
#include "litmus_rand.h"
#include "utils.h"
#ifdef CONFIG_C11
#include <stdatomic.h>
#endif
#ifdef CONFIG_DO_AFFINITY
#include "affinity.h"
// TODO: logical_procs
#endif
#endif
#ifdef CONFIG_INTRINSICS_EXISTS // NOTE: make intrinsics.h a common library to avoid code duplication!
#include "intrinsics.h"
#endif
#ifdef CONFIG_VARIANT_LITMUS_MEMTAG
#include "memtag.h"
#endif
#ifdef CONFIG_VARIANT_LITMUS_PAC
#include "auth.h"
#endif
#include "cache.h"

typedef uint32_t count_t;
#define PCTR PTIu32

#ifdef CONFIG_TIMELIMIT
#define TIMELIMIT CONFIG_TIMELIMIT
#endif

/** Generated from dump_getinstrs **/
#ifdef CONFIG_INSTRUCTIONS
#include "instruction.h" // potentially duplicating code
#endif

#if defined CONFIG_GETINSTR_ACTIVE && !(defined CONFIG_GETINSTR_SET_IS_EMPTY_IS && CONFIG_MISC_NILP_LBL2INSTR)
/***************************/
/* Get instruction opcodes */
/***************************/
// NOTE: dump all this as one giant macro in config.h
// TODO: iterate over D.dump

// TODO: iterate over static ins_t instructions

// TODO: iterate over static ins_t lbl_instr and size_t

#endif
/** Generated from dump_delay_def and dump_read_timebase **/
#ifdef CONFIG_HAVE_TIMEBASE
#define NSTEPS CONFIG_NSTEPS
#define NSTEPS2 ((NSTEPS-1)/2)
/* Read time base */
#define HAVE_TIMEBASE 1
typedef uint64_t tb_t;
#define PTB PRIu64
#if defined CONFIG_IS_KVM && defined KVM_TIMEBASE_EXISTS
#include "kvm_timebase.h" // TODO: make kvm_timebase.c into a common library (including header kvm_timebase.h) to avoid code duplication
#else
#include "timebase.h" // TODO: make timebase.c into a common library (including header timebase.h) to avoid code duplication
#endif
/** Generated from dump_mbar_def **/

#ifdef CONFIG_DUMP_FIND_INS
#include "_find_ins.h" // TODO: make _find_ins.c into a common library (including header _find_ins.h) to avoid code duplication
#define CONFIG_FIND_INS_INSERTED 1
#endif
#ifdef CONFIG_DO_SELF
#include "self.h" // TODO: make self.c into a common library (including header self.h) to avoid code duplication
#ifdef CONFIG_IS_KVM
#include "kvm-self.h" // TODO: make kvm-self.c into a common library (including header kvm-self.h) to avoid code duplication
#else
#include "presi-self.h" // TODO: make presi-self.c into a common library (including header kvm-self.h) to avoid code duplication
#endif
#endif
#ifdef CONFIG_NEED_PRELUDE
#include "_prelude_size.h" // TODO: make _prelude_size.c into a common library (including header _prelude_size.h) to avoid code duplication
#endif
/** Generated from dump_barrier_def **/
#ifdef CONFIG_NUMERIC_LABELS
#include "barrier.h" // TODO: make barrier.c into a common library (including header barrier.h) to avoid code duplication. NOTE: There are multiple barrier.c files
#else
#include "barrier_lab.h" // TODO: make barrier.c into a common library (including header barrier_lab.h) to avoid code duplication
#endif
/************/
/* Topology */
/************/

#ifdef CONFIG_DO_INLINED
/*
 Topology: CONFIG_PP_INTSS_TOPO // NOTE:This won't be replaced by the preprocessor, so it may be better to check this in config.h
*/
// TODO: dump for now -- generate inline definitions later based on CONFIG_MODE
CONFIG_INLINED_TOPOLOGY
#else
#include "topology.h"
#if defined CONFIG_IS_PRESI || defined CONFIG_IS_KVM
#define inst CONFIG_INST
#define role CONFIG_ROLE
#else
#define cpu_scan CONFIG_CPU_SCAN
#endif
#define group CONFIG_GROUP
#define SCANSZ CONFIG_SCANSZ
#define SCANLINE CONFIG_SCANLINE
#ifdef CONFIG_IS_STD
static count_t ngroups[SCANSZ];
#endif

#endif
/** Generated from dump_user_stacks **/
#ifdef CONFIG_PROCS_USER_NONEMPTY
#include "kvm_user_stacks.h" // TODO: make kvm_user_stacks.c into a common library (including kvm_user_stacks.h) to avoid code duplication

#endif
/** Generated from dump_fault_type **/ // TODO: think of a way to print out iterated constructs in a more elegant way...
typedef struct {
CONFIG_CFGLOG_ALL_LABELS_ITER // This is an iteration of all the labels in all processes (generated via iteration)
} labels_t;

#ifdef CONFIG_NEED_SYMBOLS_ENV_TEST
#define INSTR_SYMB_ID_UNKNOWN      0
CONFIG_DEFINE_FLBL // refer to line 785-786 in skelUtil.ml

static const char *instr_symb_name[] = {
  "UNKNOWN",
CONFIG_INSTR_SYMB_NAME // refer to line 792 skelUtil.ml
}

#ifdef CONFIG_IS_STD // then case dump_label_funcs_skel
// TODO: implement (not relevant for the A014 case)
#else // then case dump_label_funs_presi
static int get_instr_symb_id(labels_t *lbls, ins_t* pc) {
CONFIG_GET_INSTR_SYMB_ID_CASES // refer to lines 828-833 in skelUtil.ml
  return INSTR_SYMB_ID_UNKNOWN
};
#endif
#endif
#if defined CONFIG_SEE_FAULTS_TEST || defined CONFIG_PTR_IN_OUTS_ENV_TEST
#define DATA_SYMB_ID_UNKNOWN       0
#define DATA_SYMB_ID_0             1
CONFIG_DATA_SYMB_IDS // refer to lines 691-699 in preSi.ml

static const char *data_symb_name[] = {
  "UNKNOWN",
  "0",
CONFIG_DATA_SYMB_NAMES // refer to lines 705-711 in preSi.ml
};
#endif
#ifdef CONFIG_SEE_FAULTS_TEST
#include "kvm_fault_type.h" // TODO: make kvm_fault_type.c into a common library (including kvm_fault_type.h) to avoid code duplication
#endif
/** Generated from dump_outcomes **/
/************/
/* Outcomes */
/************/
#ifdef CONFIG_SOME_VARS_TEST
#define SOME_VARS 1
typedef struct {
#ifndef CONFIG_TEST_T_GLOBALS_EMPTY
  int_max_t CONFIG_VARS_LOC_INTMAX;
#ifdef CONFIG_IS_KVM
  pteval_t CONFIG_VARS_LOC_PTEVAL;
#endif
#endif
#ifdef CONFIG_DO_SELF
  ins_t *CONFIG_VARS_CODE_INS; // refer to outUtils.ml for interpreting fmt_code, fmt_prelude, and fmt_code_size
  size_t CONFIG_VARS_CODE_PRELUDE;
  size_t CONFIG_VARS_CODE_SIZE;
#endif
#ifdef CONFIG_SOME_LABELS_TEST
  labels_t labels;
#endif
} vars_t;
#else
typedef void vars_t;
#endif
CONFIG_DUMP_VARS_TYPES // from dump_vars_types in skelUtil.ml TODO: polish if possible?
CONFIG_DUMP_ARRAY_TYPEDEFS // from dump_array_typedefs in skelUtil TODO: polish if possible
typedef struct {
CONFIG_LOG_T_FIELDS // from lines 792-819 in preSi.ml
#ifdef CONFIG_FAULTS_NONEMPTY
  th_faults_info_t th_faults[NTHREADS];
#endif
#ifdef CONFIG_PAD
  uint32_t _pad;
#endif
} log_t;

#ifdef CONFIG_SOME_PTR_PTE
#define SOME_PTR 1
typedef struct {
CONFIG_RLOCS // refer to lines 837-844 in preSi.ml
} log_ptr_t;

#endif
#if defined CONFIG_SEE_FAULTS_TEST || defined CONFIG_SOME_PTR
#ifdef CONFIG_IS_KVM
#define ADDR p_addr
#else
#define ADDR v_addr
#endif
static int idx_addr(intmax_t *v_addr, vars_t *p) {
#ifdef CONFIG_VARIANT_LITMUS_PAC
  v_addr = (intmax_t*) strip_pauth_data((void*) v_addr);
#endif
#ifdef CONFIG_VARIANT_LITMUS_MEMTAG
  v_addr = (intmax_t*)untagged(v_addr);
#endif
#ifdef CONFIG_IS_KVM
  intmax_t *p_addr =
    (intmax_t *)((uintptr_t)vaddr & PAGE_MASK);
#endif
  if(ADDR == NULL) return DATA_SYMB_ID_0;
CONFIG_DUMP_TEST_GLOBALS // refer to line 873 in preSi.ml
  fatal("Cannot find symbol for address"); return -1;
}
#endif
#ifdef CONFIG_SOME_PTR
static const char **pretty_addr = data_symb_name;

#endif
#if defined CONFIG_IS_KVM && (defined CONFIG_PTE_IN_OUTS_ENV_TEST || defined CONFIG_PAREL1_IN_OUTS_ENV_TEST)
CONFIG_DATA_SYMB_ID // refer to lines 888-891 in preSi.ml

#ifdef CONFIG_PTE_IN_OUTS_ENV_TEST
static int idx_plysical(pteval_t v, vars_t *p) {
CONFIG_DATA_SYMB_ID_GLOBALS // refer to lines 895-900 in preSi.ml
  else return NVARS;
}
#endif
#ifdef CONFIG_PAREL1_IN_OUTS_ENV_TEST
static int idx_physical_parel1(parel1_t v, vars_t *p) {
  if (v & msk_f) return NVARS;
CONFIG_DATA_SYMB_ID_GLOBALS // refer to lines 905-911 in preSi.ml
  else return NVARS;
}
#endif

CONFIG_PRETTY_ADDR_PHYSICAL // refer to line 915 in preSi.ml

#endif
#ifdef CONFIG_INSTR_IN_OUTS_ENV_T
static char *pretty_opcode(ins_t op) {
  if (op == 0) return instr:"UDF";
CONFIG_A_V_INSTR_SET_ITERS // refer to lines 1161-1172 in skelUtil.ml
  else return "???";
}
#endif
#if defined CONFIG_TAG_IN_OUTS_ENV_T || defined CONFIG_PTR_TAG_IN_OUTS_ENV_T
static char *pretty_tag(tag_t tag) {
  switch (tag) {
CONFIG_TAG_OF_INT_ITER // refer to lines 1182-1186 in skelUtil.ml
    default: return ":?";
  }
}

#endif
/* Dump of outcome */
static void pp_log(FILE *chan, log_t *p) {
CONFIG_PP_LOG // refer to lines 926-1025 in preSi.ml. TODO: prettify?
}

/* Equality of outcomes */
static int eq_log(log_t *p, log_t *q) {
  return
CONFIG_LOCS_REC // refer to lines 1032-1053 in preSi.ml
}
/** Generated by dump_fault_handler **/
#ifdef CONFIG_HAVE_FAULT_HANDLER // 1
#ifdef CONFIG_PARTITIONS_ASMHANDLERS_NONEMPTY // 2
/* Fault Handling */
#define HAVE_FAULT_HANDLER 1

typedef struct { int instance,proc; } who_t;

#ifdef CONFIG_DO_DYNALLOC // 3
static count_t *nfaults;
static who_t *whoami;

static void alloc_fault_handler(void) {
  nfaults = malloc_check(NTHREADS*sizeof(*nfaults));
  whoami = malloc_check(AVAIL*sizeof(*whoami));
}

static void free_fault_handler(void) {
  free(whoami); free(nfaults);
}
#else
static count_t nfaults[NTHREADS];
static who_t whoami[AVAIL];
#endif // 3

#include "instruction.h"

#ifdef CFG_PRECISION_FATAL // 3
#define PRECISE 1

#elif CFG_PRECISION_SKIP
#define FAILT_SKIP 1

#endif // 3
#ifndef CONFIG_FAULTS_NONEMPTY // 3
#ifndef CONFIG_FIND_INS_INSERTED // 4
#include "_find_ins.h"

#endif // 4
#ifdef CONFIG_DO_DYNALLOC // 4
static vars_t **vars_ptr;

static void alloc_see_faults(void) {
  vars_ptr = malloc_check(NEXE*sizeof(*vars_ptr));
}

static void free_see_faults(void) {
  free(vars_ptr);
}
#else
static vars_t *vars_ptr[NEXE];
#endif // 4
#else
#define SEE_FAULTS 1

#if !defined CONFIG_CFGLOG_ALL_LABELS_EMPTY || (defined CONFIG_DO_PRECISE && !defined CONFIG_FIND_INS_INSERTED) // refer to lines 427-430 in preSi.ml // 4
#include "_find_ins.h"

#endif // 4
#ifdef CONFIG_DO_DYNALLOC // 4
static th_faults_info_t **th_faults;
static vars_t **vars_ptr;

static void alloc_see_faults(void) {
  th_faults = malloc_check(NEXE*sizeof(*th_faults));
  vars_ptr = malloc_check(NEXE*sizeof(*vars_ptr));
}

static void free_see_faults(void) {
  free(th_faults);
  free(vars_ptr);
}
#else
static th_faults_info_t *th_faults[NEXE];
static vars_t *vars_ptr [NEXE];
#endif // 4
#endif // 3
static inline int log_fault(int proc, int instr_symb, int data_symb, int ftype)
{
CONFIG_COND_FAULTS_ITER // refer to lines 452-466 in preSi.ml
  return 0;
}

#include "kvm_fault_handler.h"

#ifndef CONFIG_HAS_ASMHANDLER_TEST // 3
static void pp_faults(void) {
  count_t total=0;
  for (int k=0 ; k < NTHREADS; k++) { total += nfaults[k]; }
  if(total > 0) {
    printf("Faults "CONFIG_NAME" %"PCTR"",total);
    for (int k = 0; k < NTHREADS; k++) {
      count_t c = nfaults[k];
      if (c > 0) printf(" P%d:%"PCTR"",k,c);
    }
    printf("\n");
  }
}

#endif // 3
#elif CONFIG_PARTITION_ASMHANDLERS_OK // i.e. ok is not an empty list in lines 489-494 in preSi.ml
#include "instruction.h"

#endif // 2
CONFIG_DUMP_VECTOR_TABLE_ITER_1 // lines 497-502 in preSi.ml
CONFIG_DUMP_VECTOR_TABLE_ITER_2 // lines 503-509 in preSi.ml
#include "asmhandler.h" // TODO: make asmhandler.c into a common library (including header asmhandler.h) to avoid code duplication.

#ifdef CONFIG_MATCH_OK_NOOK_EMPTY // 2
static void set_fault_vector(int role) { }
#else
static void set_fault_vector(int role) {
  ins_t *r = NULL;
  switch(role) {
CONFIG_SET_FAULT_VECTOR_CASES_ITER // lines 523-528 in preSi.ml
#ifndef CONFIG_MATCH_NO_NO_EMPTY // 3
  default:
    {
      extern ins_t vector_table;
      r = &vector_table;
    }
#endif // 3
  }
  exceptions_init_test(r);
}
#endif // 2
#elif CONFIG_IS_KVM
static void set_fault_vector(int role) { }

#endif // 1
/** Generated from dump_cond_def **/
CONFIG_DUMP_COND_FUN // TODO: refactor instead of dumping...

/** Generated from dump_parameters **/
/**************/
/* Parameters */
/**************/

typedef enum { cignore, fclush, ctouch, cmax, } dir_t;

typdef struct {
  int part;
CONFIG_PP_TAGS_V_ONE
CONFIG_PP_TAGS_D_ONE
CONFIG_PP_TAGS_C_ONE // refer to lines 1234-1239 in preSi.ml
} param_t;

static param_t param = {CONFIG_NUM_TAGS_MAP_MINUS_ONE}; // refer to lines 1242-1244 in preSi.ml

static int id(int x) { return x; }
#if defined CONFIG_HAVE_TIMEBASE && defined CONFIG_GET_NPROCS_TEST_GR_ONE // 1
static int addnsteps(int x) { return x+NSTEPS2; }
#endif // 1

static parse_param_t parse[] = {
  {"part",&param.part,id,SCANSZ},
CONFIG_PP_TAGS_V_TWO
CONFIG_PP_TAGS_D_TWO
CONFIG_PP_TAGS_C_TWO // refer to lines 1252-1262 in preSi.ml
};

#define PARSESZ (sizeof(parse)/sizeof(parse[0]))

#ifdef CONFIG_DO_STATS // 1
static void pp_param(FILE *out, param_t *p) {
CONFIG_PP_PARAM_PRINTF // refer to lines 1271-1283 in preSi.ml
}

typedef struct {
  count_t groups[SCANSZ];
  count_t CONFIG_STATS_T_VARS;
  count_t CONFIG_STATS_T_DELAYS;
  count_T CONFIG_STATS_T_DIRS;    // refer to lines 1290-1295 in preSi.ml
} stats_t;
#endif // 1

/** Generated from dump_hash_def **/
#define HASHZ CONFIG_HASHZ

#include "_hash.h" // TODO: make _hash.c into a common library (including _hash.h) to avoid code duplication

static void pp_entry(FILE *out,entry_t *p, int verbose, const char **group) {
  printf("%-6" PCTR "%c>",p->c, p->ok ? '*' : ':');
  pp_log(out, &p->key);
#ifdef CONFIG_DO_STATS // 1
  if (verbose) {
    puts(" # ");
    pp_param(out,&p->p);
    puts(" ");
    puts(group[p->p.part]);
  }
#endif // 1
  printf("%c", '\n');
}

/** Generated from dump_set_feature **/
#if defined CONFIG_IS_KVM && (defined CONFIG_DB_EMPTY || (!defined CONFIG_HA_DIFF && !defined CONFIG_HD_DIFF)) // 1
static void set_feature(int _role) { }
#else // 2
static void set_feature(int role) {
  switch (role) {
CONFIG_SET_FEATURE_CASES_FOR_LOOP
  } 
  return;
}
#endif // 1
/*************/
/* Test code */
/*************/

CONFIG_ASM_DUMP_CODE // for now dump the entire test code
/** Generated from dump_instance_def **/
/***************/
/* Memory size */
/***************/

#ifdef CONFIG_IS_KVM // 1
/* Page size line */
#define LINE LITMUS_PAGE_SIZE
#else // 1
/* Cache line line */
#define LINE CONFIG_LINE
#define VOFF CONFIG_VOFF
#endif // 1

#if defined CONFIG_SOME_VARS_TEST || defined CONFIG_DO_SELF // 1
static void vars_init(vars_t *_vars,intmax_t *_mem) {
#ifdef CONFIG_IS_KVM // 2
#ifdef CONFIG_CONSP_GLOBALS // 3
const size_t _sz LINE/sizeof(intmax_t);
pteval_t *_p;

#endif // 3
CONFIG_VARS_INIT_GLOBALS_ITER // refer to lines 1425-1441 in preSi.ml
#ifdef CONFIG_HAS_USER // 3
  flush_tlb_all();
#endif // 3
#endif // 2
#ifdef CONFIG_DO_SELF // 2
CONFIG_VARS_INIT_CODE_ITER // refer to lines 1446-1459 in preSi.ml
#endif // 2
}

static void vars_free(vars_t *_vars) {
#ifdef CONFIG_DO_SELF // 2
CONFIG_VARS_FREE_CODE_ITER
#endif // 2
}

static void labels_init(vars_t *_vars) {
#if defined CONFIG_DO_LABEL_INIT || defined CONFIG_DO_PRECISE // 2
  labels_t *lbls = &_vars->labels;
#endif // 2
CONFIG_LABELS_INIT_ALL_LABELS_ITER // refer to lines 1480-1491 in preSi.ml
#ifdef CONFIG_DO_PRECISE // 2
CONFIG_LABELS_INIT_CODE_ITER // refer to lines 1493-1505 in preSi.ml
#endif // 2
}
#endif // 1

#include "_instance.h" // TODO: make _instance.c into a common library (including _instance.h) to avoid code duplication

/** Generated from dump_run_def **/
static void init_getinstrs(void) {
CONFIG_INIT_GETINSTRS_CODE // refer to the entirety of dump_niit_getinstrs in skelUtil.ml TODO: refactor
}

inline static int do_run(thread_ctx_t *_c, param_t *_p,global_t &_g) {
  int _ok = 0;
  int _role = _c->role;
  if (_role < 0) return _ok;
  ctx_t *_ctx = _c->ctx;
  sense_t *_b = &_ctx->b;
  log_t *_log = &_ctx->out;
#ifdef CONFIG_SOME_PTR // 1
  log_ptr_t *_log_ptr = &_ctx->out_ptr;
#endif // 1
#if defined CONFIG_SOME_TEST_VARS_TEST || defined CONFIG_DO_SELF || defined CONFIG_LABEL_IN_OUTS_ENV_TEST // 1
  vars_t *_vars = &_ctx->v;
#endif // 1
#ifdef CONFIG_TEST_T_GLOBALS_EMPTY // 1
CONFIG_DO_RUN_GLOBS_ITER // refer to lines 1946-1954 in preSi.ml
#endif // 1
#ifdef CONFIG_DO_SELF
CONFIG_DO_RUN_NPROCS_ITER // refer to lines 1958-1962 in preSi.ml
#endif // 1
#ifdef CONFIG_HAVE_FAULTS // 1
th_faults_info_init(&_ctx->out.th_faults[_role]);
#endif // 1
  barrier_wair(_b);
  switch (_role) {
CONFIG_DO_RUN_CODE_ITER // refer to lines 1989-1993 in preSi.ml
  }
  return _ok;
}

/** Generatred from dump_zyva_def **/
/*******************/
/* Forked function */
/*******************/

inline static int comp_param (st_t *seed,int *g,int max,int delta) {
  int tmp = *g;
  return tmp >= 0 ? tmp : delta+rand_k(seed,max-delta);
}

static void choose_params(global_t *g,thread_ctx_t *c, int part) {
  int _role = c->role;
  if(_role < 0) return;
  ctx_t *ctx = c->ctx;
#ifdef CONFIG_HAS_PARAMS_TEST // 1
  param_t *q = g->param;
#endif // 1

  for(int _s=0 ; _s < g->size; _s++) {
    barrier_wait(&ctx->b);
    switch (_role) {
CONFIG_CHOOSE_PARAMS_VSS_PSS_CSS_ITER // refer to lines 2038-2099 in preSi.ml
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

#ifdef CONFIG_IS_KVM // 1
static void zyva(void *_a) {
#ifdef CONFIG_VARIANT_LITMUS_MEMTAG // 2
  mte_init(CONFIG_PP_TAG_CHECK); // refer to lines 2157-2161 in preSi.ml
#endif // 2
#ifdef CONFIG_VARIANT_LITMUS_PAC //2
  init_pauth();
#endif // 2
  int id= smp_processor_id();
  if (id >= AVAIL) return;
  zyva_t *a = (zyva_t*)_a + id;
#else // 1
static void *zyva(void *_a) {
  zyva_t *a = (zyva_t*)_a;
  int id = a-> id;
#endif // 1
  global_t *g = a->g;
#ifdef CONFIG_IS_KVM // 1
#ifndef CONFIG_DB_EMPTY // 2
CONFIG_FEAT_SAME_DIRTYBIT_SOME_HA_SOME_HD // refer tol lines 2179-2185 in preSi.ml
#endif // 2
#ifdef CONFIG_CONSP_PROCS_USER // 2
  set_user_stack(id);
#endif // 2
#if defined CONFIG_HAVE_FAULT_HANDLER && defined CONFIG_HAS_DEFAULTHANDLER_TEST // 2
#ifdef CONFIG_CONSP_PROCS_USER // 3
/* Fault handlers installation depends on user stacks */
#endif // 3
  install_fault_handler(id);
#ifndef CONFIG_HAS_ASMHANDLER_TEST // 3
  extern ins_t vector_table;
  eexceptions_init_test(&vector_table);
#endif // 3
#endif // 2
#endif // 1
#ifdef CONFIG_DO_AFFINITY // 1
#ifdef CONFIG_LOGICALPROCS // 2
#define CONFIG_PROCS logical_procs[id]
#else // 2
#define CONFIG_PROCS id
#endif // 2
#ifdef CONFIG_FORCE_AFFINITY // 2
  force_one_affinity(CONFIG_PROCS,AVAIL,g->verbose,""CONFIG_NAME"");
#else // 2
  write_one_affinity(CONFIG_PROCS);
#endif // 2
#endif // 1
  choose(id,g);
#ifdef CONFIG_IS_KVM // 1
#if defined CONFIG_DRIVER_C && !defined CONFIG_DB_EMPTY
  reset_hahd_bits();
#endif 
#else // 1
  return NULL;
#endif // 1
}

/** Generated from dump_prelude_def **/
// TODO: do later
/** Last bits of dump **/
static int feature_check(void) {
#ifdef CONFIG_DO_SELF // 1
  cache_line_size = getcachelinesize();
#endif // 1
#ifdef CONFIG_VARIANT_LITMUS_PAC // 1
  if(!check_pac_variant(CONFIG_NAME)) return 0;
#ifdef CONFIG_VARIANT_LITMUS_FPAC // 2
  if(!check_fpac_variant(CONFIG_NAME, 1)) return 0;
#else // 2
  if(!check_fpac_variant(CONFIG_NAME, 0)) return 0;
#endif // 2
#endif // 1
#ifdef CONFIG_VARIANT_LITMUS_CONSTPACFIELD // 1
  if(!check_const_pac_field_variant(CONFIG_NAME)) return 0;
#endif // 1
#ifdef CONFIG_IS_KVM // 1
#ifdef CONFIG_DB_EMPTY // 2
  return 1;
#else // 2
CONFIG_DUMP_FEATURE_CHECK // TODO: refactor
#endif // 2
#else // 1
  return 1;
#endif // 1
}

/** Generated from dump_main_def **/
#if defined CONFIG_DRIVER_SHELL // 1
#define RUN run
#define MAIN 1
#elif defined CONFIG_DRIVER_C || defined CONFIG_DRIVER_XCODE
#define RUN CONFIG_NAME_AS_SYMBOL_DOC
#define PRELUDE 1
#endif // 1

#ifdef CONFIG_EXIT_COND // 1
#define CONFIG_EXIT_COND_T int
#else // 1
#define CONFIG_EXIT_COND_T void
#endif // 1
#define ENOUGH 10

#if defined CONFIG_MODE_STD // 1
static CONFIG_EXIT_COND_T postlude(FILE *out,cmd_t *cmd,hist_t *hist,count_t p_true, count_t p_false,tsc_t total) {
#elif defined CONFIG_MODE_PRESI || defined CONFIG_MODE_KVM // 1
static CONFIG_EXIT_COND_T postlude(FILE *out,global_t *g,count_t p_true,count_t p_false,tsc_t total) {
  hash_t *hash = &g->hash ;
#endif // 1
#ifdef CONFIG_KIND // 1
  puts("Test "CONFIG_NAME" "CONFIG_CONDITION"\n");
#else // 1
  puts("Test "CONFIG_NAME"");
#endif // 1
#if defined CONFIG_IS_STD // 1
  printf("Histogram (%d states)\n",finals_outs(hist->outcomes));
  just_dump_outcomes(out,hist);
#elif defined CONFIG_IS_PRESI || defined CONFIG_IS_KVM // 1
  printf("Histogram (%d states)\n",hash->nhash);
  pp_hash(out,hash,g->verbose > 1,g->group);
#endif // 1
#ifdef CONFIG_KIND // 1
  int cond = CONFIG_TO_CHECK;
  puts(cond?"Ok":"No");
  puts("\n");
  puts("\nWitnesses\n");
  printf("Positive: %"PCTR ", Negative: %"PCTR "\n",CONFIG_C_STATE_QUANTIFICATION);
  puts("Condition ");
  puts(""CONFIG_PP_COND"");
  puts(" is ");
  puts(cond ? "" : "NOT ");
  puts("validated'n");
  // TODO: implement rest later
#else // 1
printf("\nCondition %s", CONFIG_PP_COND);
#endif // 1
}
// TODO: import actual _main.c here...
