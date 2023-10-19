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
#ifdef HAVE_TIMEBASE
  tb_t next_tb;
#endif
  hash_t t;
  sense_t b;
  param_t p;
  int ind[N]; /* Indirection for role shuffle */
} ctx_t ;


static void instance_init(ctx_t *p, int id, intmax_t *mem) {
  p->id = id ;
  p->mem = mem ;
  hash_init(&p->t) ;
  log_init(&p->out) ;
  barrier_init(&p->b,N) ;
  interval_init((int *)&p->ind,N) ;
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
#ifdef STATS
  /* statistics */
  stats_t stats ;
#endif
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
