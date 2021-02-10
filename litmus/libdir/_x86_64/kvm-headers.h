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
  smp_init();
}

static inline void mdelay(u64 count) { delay(count) ; }

static inline pteval_t *litmus_tr_pte(void *p) {
  return get_pte((pgd_t *)read_cr3(),p);
}

static inline void litmus_flush_tlb(void *_p) {
  flush_tlb();
}
