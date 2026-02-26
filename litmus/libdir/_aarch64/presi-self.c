/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2026-present Institut National de Recherche en Informatique et */
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
#include <presi-self.h>
#include <find_ins.h>
#include <string.h>

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

void code_init(void *code, void *src, size_t sz)
{
  memcpy(code, src, sz);
  litmus_icache_sync((uintptr_t)code, (uintptr_t)code + sz);
}
