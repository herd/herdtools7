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

