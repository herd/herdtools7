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
inline static tb_t read_timebase(void) {
  uint64_t tb;
#if __riscv_xlen == 32
  uint32_t tbh1, tbl, tbh2;
  do {
    asm __volatile__ ("rdtimeh %0 ; rdtime %1 ; rdtimeh %2"
                      : "=r"(tbh1), "=r"(tbl), "=r"(tbh2) : : "memory");
  } while (tbh1 != tbh2);
  tb = ((uint64_t)tbh1 << 32) | tbl;
#elif __riscv_xlen == 64
  asm __volatile__ ("rdtime %0" : "=r" (tb) : : "memory") ;
#else
#error "Unknown __riscv_xlen"
#endif
  return tb;
}
