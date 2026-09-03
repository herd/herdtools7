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

/* rdtime (CSR time), not rdcycle: the timebase must be shared across threads.
 * cycle is per-hart and is not a correct clock for -barrier timebase. */

inline static tb_t read_timebase(void) {
  tb_t r;
#if __riscv_xlen == 32
  uint32_t hi, lo, hi2;
  do {
    asm __volatile__("rdtimeh %0" : "=r"(hi));
    asm __volatile__("rdtime  %0" : "=r"(lo));
    asm __volatile__("rdtimeh %0" : "=r"(hi2));
  } while (hi != hi2);
  r = (((tb_t)hi) << 32) | (tb_t)lo;
#else
  asm __volatile__("rdtime %[r1]" : [r1] "=r"(r) : : "memory");
#endif
  return r;
}
