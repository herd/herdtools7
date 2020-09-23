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

static inline int litmus_same_oa(pteval_t p,pteval_t q) {
  return ((p ^ q) & FULL_MASK) == 0 ;
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
  old &= ~msk_full ;
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

/* Packed pte */
#define AF_PACKED 0
#define DB_PACKED 1
#define DBM_PACKED 2
#define VALID_PACKED 3
#define OA_PACKED 4

inline static pteval_t pack_flag(pteval_t v,pteval_t mask,int shift) {
  return (v & mask ? 1 : 0) << shift;
}


inline static pteval_t pack_pte(int oa,pteval_t v) {
  return
    ((pteval_t)oa << OA_PACKED) |
    pack_flag(v,msk_af,AF_PACKED) |
    pack_flag(v ^ msk_db,msk_db,DB_PACKED) |
    pack_flag(v,msk_dbm,DBM_PACKED) |
    pack_flag(v,msk_valid,VALID_PACKED) ;
}


inline static pteval_t pack_pack_flag(int f,int shift) {
  return ((pteval_t)f) << shift ;
}

inline static pteval_t pack_pack(int oa,int af,int db,int dbm,int valid) {
  return
    (((pteval_t)oa) << OA_PACKED) |
    pack_pack_flag(oa,OA_PACKED) |
    pack_pack_flag(db,DB_PACKED) |
    pack_pack_flag(dbm,DBM_PACKED) |
    pack_pack_flag(valid,VALID_PACKED) ;
}

inline static int unpack_oa(pteval_t v) {
  return v >> OA_PACKED;
}

inline static int unpack_flag(pteval_t v,int shift) {
  return (v >> shift) & 1;
}

inline static int unpack_af(pteval_t v) { return unpack_flag(v,AF_PACKED); }
inline static int unpack_db(pteval_t v) { return unpack_flag(v,DB_PACKED); }
inline static int unpack_dbm(pteval_t v) { return unpack_flag(v,DBM_PACKED); }
inline static int unpack_valid(pteval_t v) { return unpack_flag(v,VALID_PACKED); }

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
  asm volatile("mrs %x0, id_aa64mmfr1_el1": "=r" (r));
  return r & 0b1111;
}

/* Feature enabling/disabling */
inline static uint64_t get_tcr_el1(void) {
  uint64_t r ;
  asm volatile ("mrs %x0, tcr_el1" : "=r" (r));
  return r ;
}

inline static void set_tcr_el1(uint64_t a) {
  asm volatile
    (
     "msr tcr_el1,%x0\n"
     "\tisb\n"
     "\ttlbi vmalle1\n"
     "\tdsb nsh\n"
     "\tisb"
     : : "r" (a)
     );
}

inline static void set_tcr_el1_bit(unsigned b,uint64_t v) {
  uint64_t old = get_tcr_el1();
  uint64_t msk = ((uint64_t)1) << b;
  set_tcr_el1((old & ~msk)|(v << b));
}

inline static void set_ha_bit(uint64_t v) {
  set_tcr_el1_bit(39,v);
}

inline static void set_hd_bit(uint64_t v) {
  set_tcr_el1_bit(40,v);
}

inline static void set_hahd_bits(uint64_t v) {
  uint64_t old = get_tcr_el1();
  uint64_t msk = ((uint64_t)0b11) << 39;
  uint64_t n = (old & ~msk)|(v << 39);
  set_tcr_el1(n);
}
