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
#include "kvm-headers.h"
#include "exs.h"

int check_exs(const char *tname) {
  uint64_t mmfr0;
  asm volatile("mrs %0, ID_AA64MMFR0_EL1" : "=r" (mmfr0));
  int exs = (mmfr0 >> 44) & 0xf;
  if (exs == 0) {
    printf("Test %s, ExS not supported on this system\n", tname);
    return 0;
  }
  return 1;
}

void init_exs(int eis_val, int eos_val) {
  uint64_t sctlr;
  asm volatile("mrs %0, SCTLR_EL1" : "=r" (sctlr));
  sctlr = eis_val ? (sctlr | SCTLR_EL1_EIS) : (sctlr & ~SCTLR_EL1_EIS);
  sctlr = eos_val ? (sctlr | SCTLR_EL1_EOS) : (sctlr & ~SCTLR_EL1_EOS);
  asm volatile("msr SCTLR_EL1, %0" :: "r" (sctlr));
  asm volatile("isb");
}
