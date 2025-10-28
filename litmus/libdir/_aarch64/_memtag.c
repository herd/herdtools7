/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2025-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/

#ifndef _MEMTAG_H
#define _MEMTAG_H 1

#include "kvm-headers.h"
#include "memtag.h"


#define ID_AA64PFR1_EL1_MTE_SHIFT      8

static uint64_t get_sctlr_el1(void)
{
  uint64_t r;
  asm volatile("mrs %x0, sctlr_el1" : "=r"(r));
  return r;
}

static void set_sctlr_el1(uint64_t v)
{
  asm volatile("msr sctlr_el1,%x0\n" ::"r"(v));
}


static uint64_t get_mte(void)
{
  uint64_t r;
  asm volatile("mrs %x0, id_aa64pfr1_el1" : "=r"(r));
  return (r >> ID_AA64PFR1_EL1_MTE_SHIFT) & 0b1111;
}

void mte_init(tag_check_key tag_check)
{
  uint64_t mte = get_mte();

  switch (tag_check) {
  case tag_check_Sync:
    if (mte < 0b0010)
      fatal("Synchronous Tag Check Fault not supported");
    break;
  case tag_check_Async:
    if (mte < 0b0011)
      fatal("Asynchronous Tag Check Fault not supported");
    break;
  case tag_check_Asymm:
    if (mte < 0b0011)
      fatal("Asymmetric Tag Check Fault not supported");
    break;
  case tag_check_Off:
    break;
  }

  if (mte >= 0b0010) {
    uint64_t tcf = (uint64_t)tag_check;
    uint64_t sctlr = get_sctlr_el1();
    uint64_t tcr = get_tcr_el1();

    sctlr |= SCTLR_EL1_ATA | SCTLR_EL1_ATA0;
    sctlr |= (tcf << SCTLR_EL1_TCF_SHIFT) | (tcf << SCTLR_EL1_TCF0_SHIFT);

    set_sctlr_el1(sctlr);

    tcr |= TCR_TBI0;
    set_tcr_el1(tcr); // also issues isb and flushes tlb
  }
}

#endif /* _MEMTAG_H */
