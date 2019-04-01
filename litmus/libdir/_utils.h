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
#ifndef _UTILS_H
#define _UTILS_H 1

#include <stdio.h>
#include <inttypes.h>
#include <pthread.h>
#include "litmus_rand.h"


/********/
/* Misc */
/********/

void seterrlog(FILE *chan) ;

int log_error(const char *fmt,...) ;

void fatal(char *msg) ;
/* e is errno */
void errexit(char *msg,int e) ;

void *malloc_check(size_t sz) ;

void *mmap_exec(size_t sz) ;
void munmap_exec(void *p,size_t sz) ;
int max(int n,int m) ;

void pp_ints (FILE *fp,int *p,int n) ;

void *do_align(void *p, size_t sz) ;

void *do_noalign(void *p, size_t sz) ;

void cat_file(char *path,char *msg,FILE *out) ;

/***********/
/* CPU set */
/***********/

#define CPUS_DEFINED 1
typedef struct {
  int sz ;
  int *cpu ;
} cpus_t ;

cpus_t *cpus_create(int sz) ;
cpus_t *cpus_create_init(int sz, int t[]) ;
void cpus_free(cpus_t *p) ;
void cpus_dump(FILE *fp, cpus_t *p) ;
void cpus_dump_test(FILE *fp, int *p, int sz, cpus_t *cm,int nprocs) ;

int gcd(int a, int b) ;

cpus_t *coremap_seq(int navail, int nways) ;
cpus_t *coremap_end(int navail, int nways) ;

void custom_affinity
(st_t *st,cpus_t *cm,int **color,int *diff,cpus_t *aff_cpus,int n_exe, int *r) ;

/*************/
/* Int array */
/*************/

typedef struct {
  int sz ;
  int *t ;
} ints_t ;

void ints_dump(FILE *fp, ints_t *p) ;

/* Prefetch directives */
typedef enum {none, flush, touch, touch_store} prfdir_t ;

typedef struct {
  char *name ;
  prfdir_t dir ;
} prfone_t ;

typedef struct {
  int nvars ;
  prfone_t *t ;
} prfproc_t ;

typedef struct {
  int nthreads ;
  prfproc_t *t ;
} prfdirs_t ;

void prefetch_dump(FILE *fp, prfdirs_t *p) ;
int parse_prefetch(char *p, prfdirs_t *r) ;

/************************/
/* Command line options */
/************************/
typedef enum
  { aff_none, aff_incr, aff_random, aff_custom,
    aff_scan, aff_topo} aff_mode_t ;

typedef struct {
  int verbose ;
  /* Test parmeters */
  int max_run ;
  int size_of_test ;
  int stride ;
  int avail ;
  int n_exe ;
  int sync_n ;
  /* Affinity */
  aff_mode_t aff_mode ;
  int aff_custom_enabled ;
  int aff_scan_enabled ;
  int aff_incr ;
  cpus_t *aff_cpus ;
  char *aff_topo ;
  /* indirect mode */
  int shuffle ;
  /* loop test */
  int max_loop ;
  /* time base delays */
  ints_t * delta_tb ;
  /* prefetch control */
  prfdirs_t *prefetch ;
  int static_prefetch ;
  /* show time of synchronisation */
  int verbose_barrier ;
  /* Stop as soon as condition is settled */
  int speedcheck ;
  /* Enforce fixed launch order (ie cancel change lauch) */
  int fix ;
  /* Dump prelude to test output */
  int prelude ;
} cmd_t ;

void parse_cmd(int argc, char **argv, cmd_t *def, cmd_t *p) ;


/********************/
/* Thread utilities */
/********************/

/* Mutex */

typedef pthread_mutex_t pm_t ;

pm_t *pm_create(void) ;
void pm_free(pm_t *p) ;
void pm_lock(pm_t *m) ;
void pm_unlock(pm_t *m) ;

/* C11 locks interface */
#ifndef mtx_t
typedef pthread_mutex_t mtx_t ;

inline static void mtx_lock(mtx_t *p) { pm_lock(p); }
inline static void mtx_unlock(mtx_t *p) { pm_unlock(p); }
#endif

/* Condition variable */

typedef struct pc_t {
  pm_t *c_mutex ;
  pthread_cond_t *c_cond ;
#ifdef CACHE
  struct pc_t *next;
#endif
} pc_t ;

pc_t *pc_create(void) ;
void pc_free(pc_t *p) ;
void pc_wait(pc_t *p) ;
void pc_broadcast (pc_t *p) ;

/* Barrier */

/* Avoid pthread supplied barrier as they are not available in old versions */

typedef struct {
  volatile unsigned int count ;
  volatile int turn ;
  pc_t *cond ;
  unsigned int nprocs ;
} pb_t ;


pb_t *pb_create(int nprocs) ;
void pb_free(pb_t *p) ;
void pb_wait(pb_t *p) ;


/* Or flag */

typedef struct {
  pc_t *cond ;
  int nprocs ;
  int count ;
  volatile int val ;
  volatile int turn ;
} po_t ;

po_t *po_create(int nprocs) ;
void po_free(po_t *p) ;
/* Initialize flag, must be called by all participant */
void po_reinit(po_t *p) ;
/* Return the 'or' of the v arguments of all participants */
int po_wait(po_t *p, int v) ;

/* One place buffer */

typedef struct {
  pc_t *cond ;
  int volatile some ;
  void * volatile val ;
} op_t ;

op_t *op_create(void) ;
void op_free(op_t *p) ;
void op_set(op_t *p, void *v) ;
void *op_get(op_t *p) ;

/* Thread launch and join */

typedef void* f_t(void *);

void launch(pthread_t *th, f_t *f, void *a) ;

void *join(pthread_t *th) ;

/* Detached lauch and join */

op_t *launch_detached(f_t *f,void *a) ;
void *join_detached(op_t *p) ;

/* Thread cache */
void set_pool(void) ;
op_t *launch_cached(f_t *f,void *a) ;

/*****************/
/* Random things */
/*****************/

/* permutations */

void perm_prefix_ints(st_t *st,int t[], int used, int sz) ;
void perm_ints(st_t *st,int t[], int sz) ;
void perm_funs(st_t *st,f_t *t[], int sz) ;
void perm_threads(st_t *st,pthread_t t[], int sz) ;
void perm_ops(st_t *st,op_t *t[], int sz) ;

/* check permutation */
int check_shuffle(int **t, int *min, int sz) ;

/*********************/
/* Real time counter */
/*********************/

typedef unsigned long long tsc_t ;
#define PTSC "%llu"

/* Result in micro-seconds */
tsc_t timeofday(void) ;
double tsc_ratio(tsc_t t1, tsc_t t2) ;
double tsc_millions(tsc_t t) ;

/* String utilities */
int find_string(char *t[],int sz,char *s) ;

#endif
