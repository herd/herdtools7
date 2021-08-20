/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2015-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/
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
#ifdef SEE_FAULTS
  see_fault_t f;
#endif
#ifdef HAVE_TIMEBASE
  tb_t next_tb;
#endif
  hash_t t;
  sense_t b;
  param_t p;
} ctx_t ;


static void instance_init (ctx_t *p, int id, intmax_t *mem) {
  p->id = id ;
  p->mem = mem ;
  hash_init(&p->t) ;
  barrier_init(&p->b,N) ;
#ifdef KVM
#ifdef SOME_VARS
  vars_init(&p->v,mem);
#endif
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
  /* Synchronisation for all threads */
  sense_t gb ;
  /* Count 'interesting' outcomes */
  volatile int ok ;
  /* Times for timeout */
  tsc_t start,now ;
  /* All instance contexts */
  ctx_t ctx[NEXE] ; /* All test instance contexts */
  hash_t hash ;     /* Sum of outcomes */
#ifdef STATS
  /* statistics */
  stats_t stats ;
#endif
} global_t ;

#ifndef DYNALLOC
static global_t global  =
  { &param, &parse[0],
    inst, role, group,
#ifdef DYNALLOC
    NULL,
#else
    mem,
#endif
#ifdef ACTIVE
    active,
#endif
    0,
    SIZE_OF_TEST, NUMBER_OF_RUN, NEXE, NOCCS,
  };
#endif

static void init_global(global_t *g) {
#ifdef DYNALLOC
  g->param = &param;
  g->parse = &parse[0];
  g->inst = inst;
  g->role = role;
  g->group = group;
#ifdef ACTIVE
  g->active = active;
#endif
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
  int idx = SCANLINE*part+c->id ;
  int inst = g->inst[idx] ;
  if (0 <= inst && inst < g->nexe) {
    c->inst = inst ;
    c->ctx = &g->ctx[inst] ;
    c->role = g->role[idx] ;
#ifdef KVM
    set_feature(c->role) ;
#ifdef HAVE_FAULT_HANDLER
    whoami[c->id].instance = inst ;
    whoami[c->id].proc = c->role ;
#ifdef SEE_FAULTS
    if (c->role == 0) {
      vars_ptr[inst] = &c->ctx->v;
      see_fault[inst] = &c->ctx->f;
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
  barrier_wait(&g->gb) ;
}
