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

/* Field access */

static inline uint64_t litmus_get_field(pteval_t x,int low,int sz) {
  uint64_t y = (uint64_t)x;
  y >>= low ;
  int64_t mask = (((uint64_t)1) << sz)-1;
  return y & mask;
}

static inline pteval_t litmus_set_field(pteval_t old,int low,int sz,pteval_t v) {
  pteval_t mask = ((((uint64_t)1) << sz)-1) ;
  v &= mask ; v <<= low ;
  mask <<= low ;
  old &= ~mask ;
  old |= v ;
  return old;
}


/* Extract address component of PTE */
#define FULL_MASK ((((pteval_t)1 << 48)-1) & ~(((pteval_t)1 << 12)-1))

static inline pteval_t litmus_set_pte_physical(pteval_t old,pteval_t v) {
  return (v & FULL_MASK)|(old & ~FULL_MASK) ;
}

static inline pteval_t litmus_set_pte_invalid(pteval_t old) {
  return old & ~((pteval_t)1) ;
}

static const uint64_t msk_valid = 0x1U;
static const uint64_t msk_af = 0x400U;
static const uint64_t msk_dbm = 0x8000000000000U;
static const uint64_t msk_db = 0x80U;
static const uint64_t msk_full = msk_valid | msk_af | msk_dbm | msk_db;

static inline pteval_t litmus_set_pte_flags(pteval_t old,pteval_t flags) {
  flags ^= msk_db; /* inverse dirty bit -> AF[2] */
  old |= ~msk_full ;
  old |= flags ;
  return old ;
}

/* Some 'explicit' PTE attributes */

/* set 'global' PTE attributes */

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
  return litmus_set_field(old,8,2,sh);
}

/* Act on MEMATTR[3:0] ie bits [5:2] */
static inline pteval_t litmus_set_memattr(pteval_t old,pteval_t memattr) {
  return litmus_set_field(old,2,4,memattr);
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

/* Hardware managment of access flag and dirty state */

/* Feature check */
inline static uint64_t get_hafdbs(void) {
  uint64_t r ;
  asm volatile("mrs %0, id_aa64mmfr1_el1": "=r" (r));
  return r & 0b1111;
}

inline static uint64_t get_tcr_el1(void) {
  uint64_t r ;
  asm volatile("mrs %x0, tcr_el1": "=r" (r));
  return r ;
}

inline static void set_tcr_el1(uint64_t a) {
  asm volatile("msr tcr_el1,%x0": : "r" (a));
}

inline static void set_tcr_el1_bit(unsigned b,unsigned v) {
  uint64_t old = get_tcr_el1();
  uint64_t msk = ((uint64_t)1) << b;
  set_tcr_el1((old & ~msk)|(((uint64_t)v) << b));
}

inline static void set_ha_bit(unsigned v) {
  set_tcr_el1_bit(39,v);
}

inline static void set_hd_bit(unsigned v) {
  set_tcr_el1_bit(40,v);
}
