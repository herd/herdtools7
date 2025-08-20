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
#include <alloc_page.h>
#include <asm-generic/atomic.h>
#include <asm/smp.h>
#include <asm/delay.h>
#include <asm/mmu.h>
#include <asm/pgtable-hwdef.h>

#define LITMUS_PAGE_SIZE PAGE_SIZE

static inline pteval_t *litmus_tr_pte(void *p) {
  return follow_pte(mmu_idmap, (uintptr_t)p);
}

static inline void litmus_flush_tlb(void *p) {
  flush_tlb_page((unsigned long)p);
}

static inline void litmus_flush_tlb_all(void) {
  flush_tlb_all();
}

static inline pteval_t litmus_set_pte(pteval_t *p,pteval_t v) {
  pteval_t w;
#ifdef NOSWP
  int t;
  asm __volatile (
  "0:\n\t"
  "ldxr    %[w],[%[p]]\n\t"
  "stlxr   %w[t], %[v], [%[p]]\n\t"
  "cbnz    %w[t],0b\n"
  :[w] "=&r" (w), [t] "=&r" (t)
  :[p] "r" (p),[v] "r" (v)
  :"memory") ;
#else
  asm __volatile__
    ("swp %[v],%[w],[%[p]]"
     :[w] "=r" (w)
     :[p] "r" (p),[v] "r" (v)
     :"memory") ;
#endif
  return w ;
}

static const uint64_t msk_valid = 0x1UL;

/* Safe pte update, with BBM sequence */
static inline pteval_t litmus_set_pte_safe(void *p,pteval_t *q,pteval_t v) {
  pteval_t w = v & ~msk_valid ;
  pteval_t r = litmus_set_pte(q,w);
  litmus_flush_tlb(p);
  (void)litmus_set_pte(q,v); /* Last flush_tlb to be performed explicitly */
  return r ;
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
  return old & ~msk_valid ;
}

static const uint64_t msk_af = 0x400UL;
static const uint64_t msk_dbm = 0x8000000000000UL;
static const uint64_t msk_db = 0x80UL;
static const uint64_t msk_el0 = 0x40UL;
#define  msk_full (msk_valid|msk_af|msk_dbm|msk_db|msk_el0)

static inline void unset_el0(pteval_t *p) {
  *p &= ~msk_el0;
}

static inline pteval_t litmus_set_pte_flags(pteval_t old,pteval_t flags) {
  flags ^= msk_db; /* inverse dirty bit -> AP[2] */
  old &= ~msk_full ;
  old |= flags ;
  return old ;
}

/* Some 'explicit' PTE attributes */

/* set 'global' PTE attributes */

typedef enum
  { attr_Normal_iWB_oWB,
    attr_Normal_iWT_oWT,
    attr_Normal_iNC_oNC,
    attr_NSH,
    attr_ISH,
    attr_OSH,
    attr_Device_GRE,
    attr_Device_nGnRE,
    attr_Device_nGnRnE
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
  case attr_NSH:
    *p = litmus_set_sh(*p,0) ;
    break;
  case attr_ISH:
    *p = litmus_set_sh(*p,3) ;
    break;
  case attr_OSH:
    *p = litmus_set_sh(*p,2) ;
    break;
  /* For cacheability, set inner-cacheability only,
     is always non-cacheabke */
  case attr_Normal_iWB_oWB:
    *p = litmus_set_memattr(*p, MT_NORMAL);
    break;
  case attr_Normal_iWT_oWT:
#ifdef MT_NORMAL_WT
    *p = litmus_set_memattr(*p, MT_NORMAL_WT);
#else
#pragma message "Normal WT attribute not supported, using NC instead\n"
    *p = litmus_set_memattr(*p, MT_NORMAL_NC);
#endif
    break;
  case attr_Normal_iNC_oNC:
    *p = litmus_set_memattr(*p, MT_NORMAL_NC);
    break;
  case attr_Device_nGnRE:
    *p = litmus_set_memattr(*p, MT_DEVICE_nGnRE);
    break;
  case attr_Device_nGnRnE:
    *p = litmus_set_memattr(*p, MT_DEVICE_nGnRnE);
    break;
  case attr_Device_GRE:
    *p = litmus_set_memattr(*p, MT_DEVICE_GRE);
    break;
  }
}

/* Packed pte */
#define AF_PACKED 0
#define DB_PACKED 1
#define DBM_PACKED 2
#define VALID_PACKED 3
#define EL0_PACKED 4
#define OA_PACKED 5

inline static pteval_t pack_flag(pteval_t v,pteval_t mask,int shift) {
  return (v & mask ? 1 : 0) << shift;
}


inline static pteval_t pack_pte(int oa,pteval_t v) {
  return
    ((pteval_t)oa << OA_PACKED) |
    pack_flag(v,msk_af,AF_PACKED) |
    pack_flag(v ^ msk_db,msk_db,DB_PACKED) |
    pack_flag(v,msk_dbm,DBM_PACKED) |
    pack_flag(v,msk_valid,VALID_PACKED) |
    pack_flag(v,msk_el0,EL0_PACKED) ;
}

inline static pteval_t pack_pack_flag(int f,int shift) {
  return ((pteval_t)f) << shift ;
}

inline static pteval_t
pack_pack(int oa,int af,int db,int dbm,int valid,int el0) {
  return
    (((pteval_t)oa) << OA_PACKED) |
    pack_pack_flag(af,AF_PACKED) |
    pack_pack_flag(db,DB_PACKED) |
    pack_pack_flag(dbm,DBM_PACKED) |
    pack_pack_flag(valid,VALID_PACKED) |
    pack_pack_flag(el0,EL0_PACKED) ;
}

/*
  This is the packed version of zero as pteval
  - Pointer null -> index NVARS
  - db set       -> corresponding bit unset
*/

#define NULL_PACKED (pack_pack(NVARS,0,1,0,0,0))

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
inline static int unpack_el0(pteval_t v) { return unpack_flag(v,EL0_PACKED); }

/* Hardware managment of access flag and dirty state */

/* Feature check */

inline static int check_atomic(void) {
  uint64_t r ;
  asm volatile("mrs %x0, id_aa64isar0_el1": "=r" (r));
  r >>= 20 ;
  r &= 0b1111 ;
  return r == 0b0010;
}

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

inline static void reset_hahd_bits(void) {
  uint64_t hafdbs = get_hafdbs();
  if (hafdbs == 0b0001) {
    set_ha_bit(0);
  } else if (hafdbs == 0b0010) {
    set_hahd_bits(0b00);
  }
}

static inline void litmus_init(void) { reset_hahd_bits(); }

// PAR_EL1
static const uint64_t msk_f = 0x1UL;
typedef uint64_t parel1_t;

inline static int unpack_f(parel1_t p) { return p & 0x1; }

static inline int litmus_same_oa_pte_par(parel1_t p,pteval_t q) {
  return ((p ^ q) & FULL_MASK) == 0 ;
}

inline static parel1_t pack_synthetic_par_el1(uint64_t pa, int f) {
  if (f & msk_f)  // Fault indicated
    return (parel1_t)msk_f;

  return ((parel1_t)pa << OA_PACKED) | (f & msk_f);
}

inline static parel1_t pack_par_el1(int pa, parel1_t p) {
  if (p & msk_f)            // If F bit is set
    return (parel1_t)msk_f; // Return just the F bit

  return
    ((parel1_t)pa << OA_PACKED) |
    pack_flag(p, msk_f, 0);
}
