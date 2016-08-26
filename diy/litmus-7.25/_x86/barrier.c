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
  volatile int c,sense ;
  int n ;
} sense_t ;


static void barrier_init(sense_t *p, int n) {  
  p->n = p->c = n ;
  p->sense = 0 ;
}

static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1 ;
  asm __volatile__ (
    "movl $-1,%[eax]\n\t"
    "lock xaddl %[eax],%[c]\n\t"
    "subl $1,%[eax]\n\t"
    "je 0f\n"
    "1:\n\t"
    "cmpl %[ms],%[s]\n\t"
    "je 1b\n\t"
    "jmp 2f\n"
    "0:\n\t"
    "movl %[np],%[eax]\n\t"
    "movl %[eax],%[c]\n\t"
    "xorl %[eax],%[eax]\n\t"
    "testl %[ms],%[ms]\n\t"
    "sete %%al\n\t"
    "movl %[eax],%[s]\n\t"
    "mfence\n"
    "2:\n\t"
    : [eax] "=&a" (r1)
    : [ms] "r" (sense), [s] "m" (p->sense), [c] "m" (p->c), [np] "m" (p->n)
    : "memory") ;
}

