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

#include <stdint.h>
#ifdef KVM
#include <libcflat.h>
typedef void FILE;
#define stdout NULL
#define stderr NULL
#define fprintf(stderr,...) printf(__VA_ARGS__)

#ifndef noinline
#define noinline __attribute__((noinline))
#endif

#else
#include <pthread.h>
#include <string.h>
#endif

/********/
/* Misc */
/********/

void fatal(const char *msg) ;

/* e is errno */
void errexit(const char *msg,int e) ;

int max(int n,int m) ;

void *do_align(void *p, size_t sz) ;

#ifdef DYNALLOC
#ifdef KVM
#include <alloc.h>
#endif
/* Dynamic memory allocation, KVM style */
void *malloc_check(size_t sz) ;
#endif

#ifndef KVM
/********************/
/* Thread utilities */
/********************/

/* Thread launch and join */

typedef void* f_t(void *);

void launch(pthread_t *th, f_t *f, void *a) ;

void *join(pthread_t *th) ;
#endif


/*********************/
/* Real time counter */
/*********************/

typedef uint64_t tsc_t ;
#define PTSC "%" PRIu64

/* Result in micro-seconds */
tsc_t timeofday(void) ;
#ifdef KVM
typedef struct { tsc_t sec,frac; } sec_t ;
sec_t tsc_millions(tsc_t t) ;
void emit_millions(sec_t f) ;
#else
double tsc_ratio(tsc_t t1, tsc_t t2) ;
double tsc_millions(tsc_t t) ;
#endif


/**********/
/* Pre-Si */
/**********/

typedef struct {
  int verbose;
  int max_run;
  int size_of_test;
  int avail ;
  int n_exe ;
  int delay ;
  int speedcheck ;
  int fix ;
} opt_t ;

char **parse_opt(int argc,char **argv,opt_t *def, opt_t *p) ;

typedef struct {
  const char* tag;
  int *dst;
  int (*f)(int);
  int max;
} parse_param_t;

void parse_param(char *prog,parse_param_t *p,int sz,char **argv) ;

#ifndef KVM

/********************/
/* presi self alloc */
/********************/
void *mmap_exec(size_t sz) ;
void munmap_exec(void *p,size_t sz) ;

#endif

/*******************/
/* Array utilities */
/*******************/
#include "litmus_rand.h"
void interval_init(int *p,size_t sz) ;
void interval_shuffle(st_t *seed,int *p,size_t sz) ;

#endif
