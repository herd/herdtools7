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
#else
#include <pthread.h>
#endif

/********/
/* Misc */
/********/

void fatal(const char *msg) ;

/* e is errno */
void errexit(const char *msg,int e) ;

int max(int n,int m) ;

void *do_align(void *p, size_t sz) ;

#ifndef KVM
/********************/
/* Thread utilities */
/********************/

/* Thread launch and join */

typedef void* f_t(void *);

void launch(pthread_t *th, f_t *f, void *a) ;

void *join(pthread_t *th) ;
#endif

#ifdef KVM
/**********/
/* Random */
/**********/

/* type of state for pseudorandom  generators */
typedef uint32_t st_t ;

/* Unlocked random bit */
int rand_bit(st_t *st) ;

uint32_t rand_k(st_t *st,uint32_t n) ;
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
void emit_double(sec_t f) ;
#else
double tsc_ratio(tsc_t t1, tsc_t t2) ;
double tsc_millions(tsc_t t) ;
#endif


/**********/
/* Pre-Si */
/**********/

typedef enum {
  mode_scan,mode_random,
} param_mode_t ;

typedef struct {
  int verbose;
  int max_run;
  int size_of_test;
  int avail ;
  int n_exe ;
  param_mode_t mode;
} opt_t ;

char **parse_opt(int argc,char **argv,opt_t *def, opt_t *p) ;

typedef struct {
  const char* tag;
  int *dst;
  int (*f)(int);
  int max;
} parse_param_t;

void parse_param(char *prog,parse_param_t *p,int sz,char **argv) ;

#endif
