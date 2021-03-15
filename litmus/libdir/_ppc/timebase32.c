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
  tb_t r;
  uint32_t r1,r2,r3;
asm __volatile__ (
"0:\n\t"
"mftbu %[r1]\n\t"
"mftb %[r2]\n\t"
"mftbu %[r3]\n\t"
"cmpw %[r1],%[r3]\n\t"
"bne 0b\n\t"
:[r1] "=r" (r1), [r2] "=r" (r2), [r3] "=r" (r3)
: :"memory" );
  r = r2;
  r |= ((tb_t)r1) << 32;
  return r;
}
