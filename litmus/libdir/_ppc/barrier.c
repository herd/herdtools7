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

static void barrier_init (sense_t *p, int n) {
  p->n = p->c = n;
  p->sense = 0;
}

static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1;
asm __volatile__ (
"sync\n\t"
"0:\n\t"
"lwarx %[r1],0,%[c]\n\t"
"subi %[r1],%[r1],1\n\t"
"stwcx. %[r1],0,%[c]\n\t"
"bne 0b\n\t"
"cmpwi %[r1],0\n\t"
"bne 1f\n\t"
"sync\n\t"
"stw %[n],0(%[c])\n\t"
"sync\n\t"
"not %[r1],%[ms]\n\t"
"stw %[r1],0(%[s])\n\t"
"b 2f\n\t"
"1:\n\t"
"sync\n\t"
"lwz %[r1],0(%[s])\n\t"
"cmpw %[r1],%[ms]\n\t"
"beq 1b\n\t"
"2:\n\t"
"sync\n\t"
: [r1] "=&r" (r1)
: [c] "r" (&p->c), [s] "r" (&p->sense), [ms] "r" (sense), [n] "r" (p->n)
: "r0","memory") ;
}
