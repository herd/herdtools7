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
#include <stdlib.h>
#ifdef KVM
#include <libcflat.h>
#else
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>
#include <errno.h>
#include <stdint.h>
#include <sys/mman.h>
#endif

#include "utils.h"


#ifdef KVM
static int errno = 0 ;
static const int ERANGE = 1 ;
static const char *strerror(int e) { return "ERROR"; }
#endif

/********/
/* Misc */
/********/

void fatal(const char *msg) {
#ifndef KVM
  fprintf(stderr,"Failure: %s\n", msg) ;
#endif
  fprintf(stdout,"Failure: %s\n", msg) ;
  exit(1) ;
}

void errexit(const char *msg,int err) {
  fprintf(stderr,"%s: %s\n",msg,strerror(err)) ;
  exit(2) ;
}

int max(int n, int m) { return n < m ? m : n ; }

void *do_align(void *p,size_t sz) {
  uintptr_t x = (uintptr_t)p ;
  x += sz-1 ;
  x /= sz ;
  x *= sz ;
  return (void *)x ;
}

#ifdef DYNALLOC
void* malloc_check(size_t sz) {
  void *r = malloc(sz) ;
  if (!r) fatal("malloc");
  return r ;
}
#endif

/* Artithmetic  */
static long my_add (long x, long y) {
  long r = x+y ;
  if (r < x || r < y) { errno = ERANGE ; fatal("overflow") ; }
  return r ;
}

static long my_pow10(int p,long x) {
  long r = x ;
  for ( ; p > 0 ; p--) {
    long y2 = my_add(r,r) ;
    long y4 = my_add(y2,y2) ;
    long y8 = my_add(y4,y4) ;
    r = my_add(y8,y2) ;
  }
  return r ;
}

static int do_argint(char *p, char **q) {
  long r =  strtol(p,q,10) ;
  if (errno == ERANGE) { fatal("overflow") ; }
  if (**q == 'k' || **q == 'K') { r = my_pow10(3,r) ; *q += 1; }
  else if (**q == 'm' || **q == 'M') { r = my_pow10(6,r) ; *q +=1 ; }
  return r ;
}

#ifndef KVM
/*************************/
/* Concurrency utilities */
/*************************/

/* Thread launch and join */

void launch(pthread_t *th, f_t *f, void *a) {
  int e = pthread_create(th,NULL,f,a);
  if (e) errexit("pthread_create",e);
}

void *join(pthread_t *th) {
  void *r ;
  int e = pthread_join(*th,&r) ;
  if (e)  errexit("pthread_join",e);
  return r ;
}
#endif

/****************/
/* Time counter */
/****************/

#ifdef KVM
#include "kvm_timeofday.h"
tsc_t timeofday(void) { return gettimeofday(); }
#else
#include <sys/time.h>
#include <time.h>

tsc_t timeofday(void) {
  struct timeval tv ;
  if (gettimeofday(&tv,NULL)) errexit("gettimeoday",errno) ;
  return tv.tv_sec * ((tsc_t)1000000) + tv.tv_usec ;
}
#endif

#ifdef KVM
sec_t tsc_millions(tsc_t t) {
  tsc_t a = t / 1000000 ;
  tsc_t b = t % 1000000 ;
  b = (b + 5000) / 10000 ;
  sec_t r = { a, b } ;
  return r ;
}

void emit_millions(sec_t f) {
  printf("%" PRIu64, f.sec) ;
  puts(".");
  printf("%02" PRIu64, f.frac) ;
}

#else
double tsc_ratio(tsc_t t1, tsc_t t2) {
  return ((double) t1) / ((double)t2) ;
}

double tsc_millions(tsc_t t) {
  return t / 1000000.0 ;
}
#endif

/**********/
/* Pre-Si */
/**********/

static void usage_opt(char *prog,opt_t *d) {
  fprintf(stderr,"usage: %s (options)* (parameters)*\n",prog) ;
  fprintf(stderr,"%s","  -v      be verbose\n") ;
  fprintf(stderr,"%s","  -q      be quiet\n") ;
  fprintf(stderr,"  -a <n>  consider that <n> cores are available (default %d)\n",d->avail) ;
  fprintf(stderr,"  -n <n>  run n tests concurrently (default %d)\n",d->n_exe) ;
  fprintf(stderr,"  -r <n>  perform n external runs (default %d)\n",d->max_run) ;
  fprintf(stderr,"  -s <n>  perform n internal runs (default %d)\n",d->size_of_test) ;
  if (d->delay > 0) {
    fprintf(stderr,"  -tb <n> time base delay  (default %d)\n",d->delay) ;
  }
  fprintf(stderr,"  +fix    do not shuffle threads\n");
  exit(2) ;
}

static int argint_opt(char *prog,char *p,opt_t *d) {
  char *q ;
  long r = do_argint(p,&q) ;
  if (*p == '\0' || *q != '\0') {
    usage_opt(prog,d) ;
  }
  return r ;
}

char **parse_opt(int argc,char **argv,opt_t *d, opt_t *p) {
  char *prog = argv[0] ;
  for ( ; ; ) {
    --argc ; ++argv ;
    if (!*argv) return argv ;
    char fst = **argv;
    if (fst != '-' && fst != '+') return argv ;
    if (strcmp(*argv,"-q") == 0) p->verbose=0 ;
    else if (strcmp(*argv,"-v") == 0) p->verbose++ ;
    else if (strcmp(*argv,"-r") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage_opt(prog,d) ;
      p->max_run = argint_opt(prog,argv[0],d) ;
    } else if (strcmp(*argv,"-s") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage_opt(prog,d) ;
      p->size_of_test = argint_opt(prog,argv[0],d) ;
    } else if (strcmp(*argv,"-a") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage_opt(prog,d) ;
      p->avail = argint_opt(prog,argv[0],d) ;
      if (p->avail < 1) p->n_exe = 1 ;
    } else if (strcmp(*argv,"-n") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage_opt(prog,d) ;
      p->n_exe = argint_opt(prog,argv[0],d) ;
      if (p->n_exe < 1) p->n_exe = 1 ;
    } else if (strcmp(*argv,"-tb") == 0 && d->delay > 0) {
      --argc ; ++argv ;
      if (!*argv) usage_opt(prog,d) ;
      p->delay = argint_opt(prog,argv[0],d) ;
      if (p->delay < 1) p->delay = 1 ;
    } else if (strcmp(*argv,"+fix") == 0) {
      p->fix = 1 ;
    } else usage_opt(prog,d);
  }
}

static char *check_key(const char *key, char *arg) {
  while (*key) {
    if (*key != *arg) return NULL ;
    key++ ; arg++ ;
  }
  if (*arg == '=') return arg+1 ;
  return NULL ;
}

static void do_parse(char *prog,parse_param_t *p,int sz, char *arg) {
  for (int k = 0 ; k < sz; k++, p++) {
    char *rem = check_key(p->tag,arg) ;
    if (rem != NULL) {
      long i = strtol(rem,NULL,0) ;
      *p->dst = p->f(i) ;
      if (*p->dst >= p->max) {
        fprintf(stderr,"%s: parameter %s is out of range\n",prog,p->tag) ;
        exit(2) ;
      }
      return ;
    }
  }
}

void parse_param(char *prog,parse_param_t *p,int sz,char **argv) {
  while (*argv) {
    do_parse(prog,p,sz,*argv) ;
    argv++ ;
  }
}

#ifndef KVM

/********************/
/* presi self alloc */
/********************/

void *mmap_exec(size_t sz) {
  void * p = mmap(NULL, sz, PROT_READ|PROT_EXEC|PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED) {
    errexit("mmap",errno) ;
  }
  return p ;
}

void munmap_exec(void *p,size_t sz) {
  if (munmap(p,sz)) errexit("munmap",errno);
}

#endif

/*******************/
/* Array utilities */
/*******************/

void interval_init(int *p,size_t sz) {
  for (int k=0 ; k < sz ; k++) *p++ = k;
}

void interval_shuffle(st_t *seed,int *p,size_t sz) {
  for (int k=0 ; k < sz-1 ; k++) {
    int j = k + rand_k(seed,sz-k) ;
    int tmp = p[k] ;
    p[k] = p[j] ;
    p[j] = tmp ;
  }
}
