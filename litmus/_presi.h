/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*        Susmit Sarkar, University of Cambridge, UK.                */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#ifndef _UTILS_H
#define _UTILS_H 1

#include <pthread.h>
/********/
/* Misc */
/********/

void fatal(char *msg) ;
/* e is errno */
void errexit(char *msg,int e) ;

int max(int n,int m) ;

void *do_align(void *p, size_t sz) ;

/********************/
/* Thread utilities */
/********************/

/* Thread launch and join */

typedef void* f_t(void *);

void launch(pthread_t *th, f_t *f, void *a) ;

void *join(pthread_t *th) ;

/*********************/
/* Real time counter */
/*********************/

typedef unsigned long long tsc_t ;
#define PTSC "%llu"

/* Result in micro-seconds */
tsc_t timeofday(void) ;
double tsc_ratio(tsc_t t1, tsc_t t2) ;
double tsc_millions(tsc_t t) ;

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
  char* tag;
  int *dst;
  int (*f)(int);
  int max;
} parse_param_t;

void parse_param(char *prog,parse_param_t *p,int sz,char **argv) ;

#endif
