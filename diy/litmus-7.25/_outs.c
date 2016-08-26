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
#include <stdio.h>
#include "outs.h"

/**********************/
/* Lexicographic tree */
/**********************/

#if 0
static void debug(int *t, int i, int j) {
  for (int k=i ; k <= j ; k++)
    fprintf(stderr,"%i",t[k]) ;
  fprintf(stderr,"\n") ;
}
#endif


void *malloc_check(size_t sz) ;

static outs_t *alloc_outs(intmax_t k) {
  outs_t *r = malloc_check(sizeof(*r)) ;
  r->k = k ;
  r->c = 0 ;
  r->show = 0 ;
  r->next = r->down = NULL ;
  return r ;
}

void free_outs(outs_t *p) {
  if (p == NULL) return ;
  free_outs(p->next) ;
  free_outs(p->down) ;
  free(p) ;
}

/* Worth writing as a loop, since called many times */
static outs_t *loop_add_outcome_outs(outs_t *p, intmax_t *k, int i, count_t c, int show) {
  outs_t *r = p ;
  if (p == NULL || k[i] < p->k) {
    r = alloc_outs(k[i]) ;
    r->next = p ;
    p = r ;
  }
  for ( ; ; ) {
    outs_t **q ;
    if (k[i] > p->k) {
      q = &(p->next) ;
      p = p->next ;
    } else if (i <= 0) {
      p->c += c ;
      p->show = show || p->show ;
      return r ;
    } else {
      i-- ;
      q = &(p->down) ;
      p = p->down ;
    }
    if (p == NULL || k[i] < p->k) {
      outs_t *a = alloc_outs(k[i]) ;
      a->next = p ;
      p = a ;
      *q = a ;
    }
  }
}

outs_t *add_outcome_outs(outs_t *p, intmax_t *k, int sz, count_t c, int show) {
  return loop_add_outcome_outs(p,k,sz-1,c,show) ;
}

count_t sum_outs(outs_t *p) {
  count_t r = 0 ;
  for ( ; p ; p = p->next) {
    r += p->c ;
    r += sum_outs(p->down) ;
  }
  return r ;
}

int finals_outs(outs_t *p) {
  int r = 0 ;
  for ( ; p ; p = p->next) {
    if (p->c > 0) r++ ;
    r += finals_outs(p->down) ;
  }
  return r ;
}

void dump_outs (FILE *chan, dump_outcome *dout,outs_t *p, intmax_t *buff,int sz) {
  for ( ; p ; p = p->next) {
    buff[sz-1] = p->k ;
    if (p->c > 0) {
      dout(chan,buff,p->c,p->show) ;
    } else if (p->down) {
      dump_outs(chan,dout,p->down,buff,sz-1) ;
    }
  }
}

/* merge p and q into p */
static outs_t *do_merge_outs(outs_t *p, outs_t *q) {
  if (q == NULL) { // Nothing to add
    return p ;
  }
  if (p == NULL || q->k < p->k) { // Need a cell
    outs_t *r = alloc_outs(q->k) ;
    r->next = p ;
    p = r ;
  }
  if (p->k == q->k) {
    p->c += q->c ;
    p->show = p->show || q->show ;
    p->down = do_merge_outs(p->down,q->down) ;
    p->next = do_merge_outs(p->next,q->next) ;
  } else {
    p->next = do_merge_outs(p->next,q) ;
  }
  return p ;
}

outs_t *merge_outs(outs_t *p, outs_t *q, int sz) {
  return do_merge_outs(p,q) ;
}

int same_outs(outs_t *p,outs_t *q) {
  while (p && q) {
    if (p->k != q->k || p->c != q->c || p->show != q->show) return 0 ;
    if (!same_outs(p->down,q->down)) return 0 ;
    p = p->next ;
    q = q->next ;
  }
  return p == q ; /* == NULL */
}
