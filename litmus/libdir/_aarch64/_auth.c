/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris, France.                                       */
/* Rémy Citérin, ARM Ltd, Cambridge, UK                                     */
/*                                                                          */
/* Copyright 2025-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/

#include "auth.h"
#ifndef KVM
#include <stdio.h>
#endif
// read the SCTLR_EL1 status register
static uint64_t read_sctlr_el1(void) {
  uint64_t ret;
  asm __volatile__("mrs %[ret], SCTLR_EL1": [ret] "=r" (ret));
  return ret;
}

static void init_pauth_key_ia(void) {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APIAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIAKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_ib(void) {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APIBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APIBKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_da(void) {
  uint64_t x = 0x5555555555555555;
  uint64_t y = 0xaaaaaaaaaaaaaaaa;
  asm __volatile__("msr APDAKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDAKeyLo_EL1, %[y]":: [y] "r" (y));
}

static void init_pauth_key_db(void) {
  uint64_t x = 0xaaaaaaaaaaaaaaaa;
  uint64_t y = 0x5555555555555555;
  asm __volatile__("msr APDBKeyHi_EL1, %[x]":: [x] "r" (x));
  asm __volatile__("msr APDBKeyLo_EL1, %[y]":: [y] "r" (y));
}

// update the SCTLR_EL1 status register
static void write_sctlr_el1(uint64_t x) {
  asm __volatile__("msr SCTLR_EL1, %[x]":: [x] "r" (x));
}

// Initialize pointer authentication
void init_pauth(void) {
  uint64_t enIA = 1ULL << 31;
  uint64_t enIB = 1ULL << 30;
  uint64_t enDA = 1ULL << 27;
  uint64_t enDB = 1ULL << 13;
  write_sctlr_el1(enIA | enIB | enDA | enDB | read_sctlr_el1());
  init_pauth_key_ia();
  init_pauth_key_ib();
  init_pauth_key_da();
  init_pauth_key_db();
}


/*
 * My reading of ARM ARM suggests combining the APA and API
 * fields with the or operation.
 * Namely, both fields have the same values for a certain
 * levels of the PAC feature, while specifying different
 * algorithms for implementing it.
 * Thus, encoding will tell us that any algorithm is
 * implementated at the specified level:
 * 0b0000 -> no authentification
 * 0b0001 -> FEAT_PAuth
 * 0b0010 -> FEAT_EPAC
 * 0b0011 -> FEAT_PAuth2
 * 0b0100 -> FEAT_FPAC
 * 0b0101 -> FEAT_FPACCOMBINE
 */
static uint64_t get_isar1_apia(void) {
  uint64_t isar1 ;
  asm volatile("mrs %[isar1], ID_AA64ISAR1_EL1": [isar1] "=r" (isar1));
  uint64_t isar1_api = (isar1 >> 8) & 0b1111;
  uint64_t isar1_apa = (isar1 >> 4) & 0b1111;
  return  isar1_api|isar1_apa ;
}

// Check if `FEAT_Pauth2` is implemented
int check_pac_variant(char* tname) {
  switch (get_isar1_apia()) {
    case 0b0001:
    case 0b0010:
      //  Illegitimately upgrading older systems, for testing purposes only
      printf("Warning: Pauth system detected, run at your own risk.\n");
      return 1;
    case 0b0011:
    case 0b0100:
    case 0b0101:
      return 1;
    default:
      printf("Test %s, PAC not available on this system\n", tname);
      return 0;
  }
}

// Check if `FEAT_FPAC` is implemented iff `present`:
// FPAC change the way `aut*` is executed in case of failure
int check_fpac_variant(char* tname, int present) {
  switch (get_isar1_apia()) {
    case 0b0100:
    case 0b0101:
      if (!present)
        printf("Test %s, FPAC is implemented on this system\n", tname);
      return present;
    default:
      if (present)
        printf("Test %s, FPAC not implemented on this system\n", tname);
      return !present;
  }
}

// Check if `FEAT_CONSTPACFIELD` is implemented
int check_const_pac_field_variant(char* tname) {
  uint64_t isar2;
  asm volatile("mrs %[isar2], ID_AA64ISAR2_EL1": [isar2] "=r" (isar2));
  uint64_t isar2_pac = (isar2 >> 24) & 0b1111;

  switch (isar2_pac) {
    case 0b0001:
      return 1;
    default:
      printf("Test %s, CONSTPACFIELD not available on this system\n", tname);
      return 0;
  }
}
