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
/**********************/
/* User level barrier */
/**********************/


typedef struct {
  volatile int c,sense;
  int n ;
} sense_t;

static void barrier_init (sense_t *p,int n) {
  p->n = p->c = n;
  p->sense = 0;
}

/* C coding, play it safe with mbar's */

__attribute__ ((noinline)) static void barrier_wait(sense_t *p) {
  mbar() ;
  int sense = p->sense ;
  mbar();
  int rem = __sync_add_and_fetch(&p->c,-1) ;
  mbar() ;
  if (rem == 0) {
    p->c = p->n ;
    mbar();
    p->sense = 1-sense ;
  } else {
    while (p->sense == sense) ;
  }
  mbar() ;
}
