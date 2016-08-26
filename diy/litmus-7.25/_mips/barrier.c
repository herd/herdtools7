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

__attribute__ ((noinline)) static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1,r2 ;
  asm __volatile__ (
"sync\n\t"
"0:\n\t"
"ll %[r1],%[c]\n\t"
"addiu %[r2],%[r1],-1\n\t"
"sc %[r2],%[c]\n\t"
"beq %[r2],$0,0b\n\t"
"addiu %[r1],%[r1],-1\n\t"
"sync\n\t"
"bne %[r1],$0,1f\n\t"
"sw %[n],%[c]\n\t"
"sync\n\t"
"li %[r1],1\n\t"
"subu %[ms],%[r1],%[ms]\n\t"
"sw %[ms],%[s]\n\t"
"b 2f\n\t"
"1:\n\t"
"lw %[r1],%[s]\n\t"
"beq %[r1],%[ms],1b\n\t"
"2:\n\t"
"sync\n\t"
: [r1] "=&r" (r1), [r2] "=&r" (r2)
: [c] "m" (p->c), [s] "m" (p->sense), [ms] "r" (sense), [n] "r" (p->n)
: "memory") ;
}

#if 0
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
#endif
