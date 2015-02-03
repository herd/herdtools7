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

#ifndef _OUTS_H
#define _OUTS_H 1

#include <stdio.h>

/************************/
/* Histogram structure  */
/************************/


/* 64bit counters, should be enough! */
#include <inttypes.h>
typedef uint64_t count_t;
#define PCTR PRIu64




typedef struct outs_t {
  struct outs_t *next,*down ;
  count_t c ;
  intmax_t k ;
  int show ;
} outs_t ;

void free_outs(outs_t *p) ;
outs_t *add_outcome_outs(outs_t *p, intmax_t *o, int sz, count_t v, int show) ;
int finals_outs(outs_t *p) ;
count_t sum_outs(outs_t *p) ;
typedef void dump_outcome(FILE *chan, intmax_t *o, count_t c, int show) ;
void dump_outs (FILE *chan, dump_outcome *dout,outs_t *p, intmax_t *buff, int sz) ;
outs_t *merge_outs(outs_t *p,outs_t *q, int sz) ;
int same_outs(outs_t *p,outs_t *q) ;
#endif
