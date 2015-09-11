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
