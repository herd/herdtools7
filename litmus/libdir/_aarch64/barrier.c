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

/*
barrier_wait:
	ldr	w2, [x0,4]
.L28:
	ldaxr	w1, [x0]
	sub	w1, w1, #1
	stlxr	w3, w1, [x0]
	cbnz	w3, .L28
	cmp	w1, wzr
	ble	.L24
.L26:
	ldr	w1, [x0,4]
	cmp	w1, w2
	beq	.L26
	dmb	ish
.L24:
	ldr	w1, [x0,8]
	str	w1, [x0]
	mov	w1, 1
	dmb	ish
	sub	w2, w1, w2
	str	w2, [x0,4]
	dmb	ish
	ret
*/
static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1,r3 ;
asm __volatile (
"dmb sy\n"
"0:\n\t"
"ldxr %w[r1],%[c]\n\t"
"sub %w[r1],%w[r1],#1\n\t"
"stxr %w[r3],%w[r1],%[c]\n\t"
"cbnz %w[r3],0b\n\t"
"cmp %w[r1],wzr\n\t"
"ble 2f\n\t"
"dmb sy\n"
"1:\n\t"
"ldr %w[r1],%[s]\n\t"
"cmp %w[r1],%w[ms]\n\t"
"beq 1b\n\t"
"b 3f\n"
"2:\n\t"
"dmb sy\n\t"
"ldr %w[r1],%[n]\n\t"
"str %w[r1],%[c]\n\t"
"mov %w[r3],#1\n\t"
"dmb sy\n\t"
"sub %w[ms],%w[r3],%w[ms]\n\t"
"str %w[ms],%[s]\n"
"3:\n\t"
"dsb sy\n\t"
: [r1] "=&r" (r1), [r3] "=&r" (r3)
: [c] "m" (p->c), [s] "m" (p->sense), [ms] "r" (sense), [n] "m" (p->n)
: "memory") ;
}
#if 0
/* C version */
static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  __sync_synchronize() ;
  int rem = __sync_add_and_fetch(&p->c,-1) ;
  if (rem > 0) {
    while (p->sense == sense) ;
    __sync_synchronize() ;
  } else {
    p->c = p->n ;
    __sync_synchronize() ;
    p->sense = 1-sense ;
    __sync_synchronize() ;
  }
}
#endif

