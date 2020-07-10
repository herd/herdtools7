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
#include <asm/smp.h>
#include <asm/delay.h>
#include <asm/mmu.h>
#define LITMUS_PAGE_SIZE PAGE_SIZE

static inline void litmus_init(void) {}

static inline pteval_t *litmus_tr_pte(void *p) {
  return tr_pte(p);
}

static inline void litmus_flush_tlb(void *p) {
  flush_tlb_page((unsigned long)p);
}

/* Extract address component of PTE */
#define FULL_MASK ((((pteval_t)1 << 48)-1) & ~(((pteval_t)1 << 12)-1))
static inline void litmus_set_pte_physical(pteval_t *p,pteval_t v) {
  pteval_t prev = *p ;
  *p = (v & FULL_MASK)|(prev & ~FULL_MASK) ;
}

static inline void litmus_set_pte_invalid(pteval_t *p) {
  *p &= ~((pteval_t)1) ;
}

/* set PTE attributes */

typedef enum
  {attr_normal, attr_write_2D_through,
   attr_write_2D_back, attr_non_2D_cacheable,
   attr_device,
   attr_nGnRnE,attr_nGnRE,
   attr_nGRE,attr_GRE,
   attr_rNsh,attr_rIsh,attr_rOsh,
   /* attr_rSy, ?? */
   /*   attr_peripherical, LM:?? */
  } pte_attr_key;

/* Act on SH[1:0] ie bits [9:8] */
static inline pteval_t litmus_set_sh(pteval_t old,pteval_t sh) {
  pteval_t mask = ((pteval_t)3) << 8 ;
  old &= ~mask ; old |= sh << 8 ;
  return old ;
}

/* Act on MEMATTR[3:0] ie bits [5:2] */
static inline pteval_t litmus_set_memattr(pteval_t old,pteval_t memattr) {
  pteval_t mask = ((pteval_t)15) << 2 ;
  old &= ~mask ; old |= memattr << 2 ;
  return old ;
}

static inline void litmus_set_pte_attribute(pteval_t *p,pte_attr_key k) {
  switch (k) {
  case attr_rNsh:
    *p = litmus_set_sh(*p,0) ;
    break;
  case attr_rIsh:
    *p = litmus_set_sh(*p,3) ;
    break;
  case attr_rOsh:
    *p = litmus_set_sh(*p,2) ;
    break;
  /* For cacheability, set inner-cacheability only,
     is always non-cacheabke */
  case attr_normal:
  case attr_write_2D_back:
    *p = litmus_set_memattr(*p,7);
    break;
  case attr_write_2D_through:
    *p = litmus_set_memattr(*p,6);
    break;
  case attr_non_2D_cacheable:
    *p = litmus_set_memattr(*p,5);
    break;
  case attr_device:
  case attr_nGnRE:
    *p = litmus_set_memattr(*p,1);
    break;
  case attr_nGnRnE:
    *p = litmus_set_memattr(*p,0);
    break;
  case attr_nGRE:
    *p = litmus_set_memattr(*p,2);
    break;
  case attr_GRE:
    *p = litmus_set_memattr(*p,3);
    break;
  }
}

/* Faulty virtual adress in handler */
inline static void *read_far(void) {
  void *r ;
  asm volatile("mrs %0, far_el1": "=r" (r));
  return r ;
}

inline static void *read_elr_el1(void) {
  void *r ;
  asm volatile("mrs %0, elr_el1": "=r" (r));
  return r ;
}
