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
  param_t p; // For random scan
} ctx_t ;


static void instance_init (ctx_t *p, int id, intmax_t *mem) {
  p->id = id ;
  p->mem = mem ;
  hash_init(&p->t) ;
  barrier_init(&p->b,N) ;
}

/******************/
/* Global context */
/******************/

#define LINESZ (LINE/sizeof(intmax_t))
#define MEMSZ ((NVARS*NEXE+1)*LINESZ)

static intmax_t mem[MEMSZ] ;

typedef struct global_t {
  /* Command-line parameter */
  param_t *param ;
  parse_param_t *parse ;
#ifdef KVM
  int volatile over;
#endif
  /* Topology */
  int *inst, *role ;  
  char **group ;
  /* memory */
  intmax_t *mem ;
  /* Cache control */
#ifdef ACTIVE
  active_t *active;
#endif  
  /* Runtime control */
  int verbose ;
  int size,nruns,nexe,noccs ;
  int do_scan ;
  /* Synchronisation for all threads */
  volatile int go ; /* First synchronisation */
  sense_t gb ;    /* All following synchronisation */
  /* Count 'interesting' outcomes */
  volatile int ok ;
  /* Times for timeout */
  tsc_t start,now ;
  /* All instance contexts */
  ctx_t ctx[NEXE] ; /* All test instance contexts */
  hash_t hash ;     /* Sum of outcomes */
  /* statistics */
  stats_t stats ;
} global_t ;

static global_t global  =
  { &param, &parse[0],
#ifdef KVM
    0,
#endif
    inst, role, group, mem,
#ifdef ACTIVE
    active,
#endif
  };

static void init_global(global_t *g,int id) {
  if (id == 0) {
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
    mbar() ;
    g->go = 1 ;
  } else {
    while (g->go == 0) ;
    mbar() ;
  }
}

/******************/
/* Thread context */
/******************/

typedef struct {
  int id ;
  st_t seed ;
  int role ;
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
    c->ctx = &g->ctx[inst] ;
    c->role = g->role[idx] ;
#ifdef ACTIVE
    c->act = &g->active[part] ;
#endif
  } else {
    c->ctx = NULL ;
    c->role = -1 ;
  }
  barrier_wait(&g->gb) ;
}
