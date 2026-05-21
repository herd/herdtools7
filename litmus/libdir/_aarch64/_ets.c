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
#include "kvm-headers.h"
#include "ets.h"

#define MMFR1_ETS_SHIFT 36
#define MMFR1_ETS_MASK 0xf

int check_ets(int level, const char *tname) {
  uint64_t mmfr1;
  asm volatile("mrs %0, ID_AA64MMFR1_EL1" : "=r" (mmfr1));
  int ets = (mmfr1 >> MMFR1_ETS_SHIFT) & MMFR1_ETS_MASK;
  if (ets < level) {
    printf("Test %s, ETS%d not supported on this system\n", tname, level);
    return 0;
  }
  return 1;
}
