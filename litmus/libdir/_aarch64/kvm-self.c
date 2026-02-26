/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2019-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/
/* Authors:                                                                 */
/* Nikos Nikoleris, Arm Limited.                                            */
/****************************************************************************/
#include <self.h>
#include <kvm-self.h>
#include <_find_ins.h>
#include <../kvm-headers.h>

static ins_t getret(void) {
  ins_t *x1;
  ins_t r;
  asm __volatile__ (
  "adr %[x1],0f\n\t"
  "ldr %w[x2],[%[x1]]\n\t"
  "b 1f\n"
  "0:\n\t"
  "ret\n"
  "1:\n"
:[x1] "=&r" (x1),[x2] "=&r" (r)
:
: "cc","memory"
);
  return r;
}

void litmus_icache_sync(uintptr_t vaddr, uintptr_t vaddr_end)
{
  while (vaddr < vaddr_end) {
    selfbar((void *)vaddr);
    vaddr += cache_line_size;
  }
}

size_t code_size(ins_t *p,int skip) {
  return (find_ins(getret(), p, skip) + 1) * sizeof(ins_t);
}

void litmus_pte_unset_el0(uintptr_t vaddr, uintptr_t vaddr_end)
{
  while (vaddr < vaddr_end) {
    pteval_t *pte = litmus_tr_pte((void *)vaddr);
    litmus_set_pte_safe((void *)vaddr, pte, *pte & ~msk_el0);
    vaddr += LITMUS_PAGE_SIZE;
  }
  asm __volatile__ (
    "dsb ish\n\t"
    "isb"
    ::: "memory"
  );
}

void code_init(void *code, void *src, size_t sz)
{
  memcpy(code, src, sz);
  litmus_icache_sync((uintptr_t)code, (uintptr_t)code + sz);
  /* Armv8 requires that code shared between EL1 and EL0 is read-only.
   * We need write access and therefore, we have to ensure that the
   * the memory we use for code won't be accessible from EL0 */
  litmus_pte_unset_el0((uintptr_t)code, (uintptr_t)code + sz);
}
