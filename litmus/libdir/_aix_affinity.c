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
#include <sys/processor.h>
#include <sys/thread.h>
#include "affinity.h"

#if 0
static void pp_set(FILE *fp, cpus_t *p) {  
  for (int k = 0 ; k < p->sz ; k++) {
    fprintf(fp," %i", p->cpu[k]) ;
  }
  fprintf(fp,"\n") ;
}
#endif


static int get_avail(void) {
  int n_online = sysconf(_SC_NPROCESSORS_ONLN);
  if (n_online < 0) {
    fatal("sysconf") ;
  }
  return n_online ;
}

cpus_t *read_affinity(void) {
  int sz = get_avail() ;
  cpus_t *r ;
  r = cpus_create(sz) ;
  for (int p=0, *q=r->cpu ; p <  sz ; p++) {
    *q++ = p ;
  }
  return r ;
}

cpus_t *read_foroce_affinity(int n_avail, int verbose) {
  return read_affinity() ;
}


void write_affinity(cpus_t *p) {
  return ;
}

void write_one_affinity(int a) {
  if (a >= 0) {
    int tid = thread_self();
    if (bindprocessor(BINDTHREAD, tid, a) < 0) {
      fatal("bindprocessor") ;
    }
  }  
}

void force_one_affinity(int cpu, int a, int verbose, char *name) {
  if (a >= 0) {
    int tid = thread_self();
    int r ;
    do {
      r = bindprocessor(BINDTHREAD, tid, a) ;
    } while (r < 0) ;
  }  
}
