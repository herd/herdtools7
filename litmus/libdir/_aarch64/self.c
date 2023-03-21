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

/***********************************/
/* Support for self-modifying code */
/***********************************/

static uint32_t getcachelinesize(void) {
  uint64_t csz;
  asm __volatile__ (
                    "mrs %[r],CTR_EL0"
                    :[r] "=&r" (csz)
                    :
                    :"cc","memory"
                    );
  uint64_t sz1 = csz & 0xf;
  uint64_t sz2 = (csz >> 16 ) & 0xf;
  uint32_t x = (uint32_t) (sz1 >= sz2 ? sz1 : sz2);
  return (1 << x) * 4 ;
}

static uint32_t cache_line_size;

inline static void selfbar(void *p) {
  asm __volatile__
    ("dc cvau,%[p]\n\t" "dsb ish\n\t" "ic ivau,%[p]\n\t" "dsb ish\n\t" "isb"
     ::[p]"r"(p): "memory");
}

inline static void isync(void) {
  asm __volatile__ ("isb" ::: "memory");
}

inline static void check_dic_idc(int need_dic, int need_idc) {
  uint64_t ctr_el0;
  asm volatile ("mrs %0, ctr_el0" : "=r" (ctr_el0));
  int idc = (ctr_el0 >> 28) & 1;
  int dic = (ctr_el0 >> 29) & 1;
  if ((need_dic && !dic) || (need_idc && !idc)) {
    fprintf(stderr, "Fatal error: hardware does not support the CacheType "
            "feature IDC=%d, DIC=%d\n", need_idc, need_dic);
    exit(0);
  }
}
