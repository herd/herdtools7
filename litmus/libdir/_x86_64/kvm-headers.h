/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2020-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/


#include<libcflat.h>
#include <vmalloc.h>
#include <asm-generic/atomic.h>
#include <asm/page.h>
#include <x86/smp.h>
#include <x86/delay.h>
#include <x86/vm.h>
#include <x86/processor.h>
#define LITMUS_PAGE_SIZE LARGE_PAGE_SIZE

static inline void litmus_init(void) {
  setup_vm();
}


// Same name as aarch64
static inline int smp_processor_id(void) { return smp_id(); }

static inline void mdelay(u64 count) { delay(count) ; }

static inline pteval_t litmus_set_pte(pteval_t *p,pteval_t x) {
  asm __volatile__
    ("xchgq (%[p]),%[r]" :[r] "+r" (x) :[p] "r" (p) :"memory") ;
  return x;
}

static inline pteval_t *litmus_tr_pte(void *p) {
  return get_pte((pgd_t *)read_cr3(),p);
}

static inline void litmus_flush_tlb(void *_p) {
  flush_tlb();
}

/* For AArch64, this performs BBM, not needed for X86_64, as
   no PTE change is performed. Yet? */
static inline pteval_t litmus_set_descriptor_safe(void *q,pteval_t *p,pteval_t x) {
  return litmus_set_pte(p,x);
}
