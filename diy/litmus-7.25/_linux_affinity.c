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
#include <stdio.h>
#include <sched.h>
#include <unistd.h>
#include "utils.h"
#include "affinity.h"

#ifdef CPUS_DEFINED
cpus_t *read_affinity(void) {
  cpu_set_t mask;
  int sz = 0 ;
  int res = pthread_getaffinity_np(pthread_self(), sizeof(mask), &mask) ;
  
  if (res != 0) { 
    errexit("pthread_getaffinity_np",res);
  }
  for (int p=0 ; p <  CPU_SETSIZE ; p++) {
    if (CPU_ISSET(p,&mask)) sz++ ;
  }

  cpus_t *r = cpus_create(sz) ;
  for (int p=0, *q=r->cpu ; p <  CPU_SETSIZE ; p++) {
    if (CPU_ISSET(p,&mask)) *q++ = p ;
  }
  return r ;
}

#endif
/* Attempt to force processors wake up, on devices where unused procs
   go to sleep... */


#ifdef FORCE_AFFINITY
const static tsc_t sec = (tsc_t)1000000 ;

static void* loop(void *p)  {
  tsc_t *q = p ;
  tsc_t max = *q ;
  while (timeofday() < max) ;
  return NULL ;
}


static void warm_up(int sz, tsc_t d) {
    pthread_t th[sz];
    d += timeofday() ;
    for (int k = 0 ; k < sz ; k++) launch(&th[k], loop, &d) ;
    for (int k = 0 ; k < sz ; k++) join(&th[k]) ;
}

#ifdef CPUS_DEFINED
cpus_t *read_force_affinity(int n_avail, int verbose) {
  int sz = n_avail <= 1 ? 1 : n_avail ;
  tsc_t max = sec / 100 ;

  for ( ; ; ) {
    warm_up(sz+1,max) ;
    cpus_t *r = read_affinity() ;
    if (n_avail <= r->sz) return r ;
    if (verbose) {
      fprintf(stderr,"Read affinity: '") ;
      cpus_dump(stderr,r) ;
      fprintf(stderr,"'\n") ;
    }
    cpus_free(r) ;
  }
}
#endif
#endif

#ifdef CPUS_DEFINED

/* Enforcing processor affinity.
   Notice that logical processor numbers may be negative.
   In that case, affinity setting is ignored */
 

void write_affinity(cpus_t *p) {
  cpu_set_t mask;
  int exists_pos = 0 ;

  CPU_ZERO(&mask) ;
  for (int k = 0 ; k < p->sz ; k++) {
    if (p->cpu[k] >= 0) {
      CPU_SET(p->cpu[k],&mask) ;
      exists_pos = 1 ;
    }
  }
  if  (exists_pos) {
    int r = pthread_setaffinity_np(pthread_self(),sizeof(mask),&mask) ;
    if (r != 0) {
      errexit("pthread_setaffinity_np",r) ;
    }
  }
}
#endif

void write_one_affinity(int a) {
  if (a >= 0) {
    cpu_set_t mask;
    CPU_ZERO(&mask) ;
    CPU_SET(a,&mask) ;
    int r = pthread_setaffinity_np(pthread_self(), sizeof(mask), &mask) ;
    if (r != 0) {
      errexit("pthread_setaffinity_np",r) ;
    }
  }
}

#ifdef FORCE_AFFINITY
/* Get the number of present cpus, fragile */

static const char *present = "/sys/devices/system/cpu/present" ;

static int get_present(void) {
  FILE *fp = fopen(present,"r") ;
  if (fp == NULL) return -1 ;
  int r1,r2 ;
  int n = fscanf(fp,"%d-%d\n",&r1,&r2) ;
  fclose(fp) ;
  if (n != 2) return -1 ;
  return r2-r1+1 ;
}

void force_one_affinity(int a, int sz,int verbose, char *name) {
  if (a >= 0) {
    cpu_set_t mask;
    int r ;
    CPU_ZERO(&mask) ;
    CPU_SET(a,&mask) ;
    do {
      r = pthread_setaffinity_np(pthread_self(), sizeof(mask), &mask) ;
      if (r != 0) {
        if (verbose)
          fprintf(stderr,"%s: force %i failed\n",name,a) ;
        int nwarm = get_present() ;
        if (verbose > 1)
          fprintf(stderr,"%s: present=%i\n",name,nwarm) ;
        if (nwarm < 0) nwarm = sz+1 ;
        warm_up(nwarm,sec/100) ;
      }
    } while (r != 0) ;
  }
}
#endif
