/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
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
#ifndef _AUTH_H
#define _AUTH_H 1

#include "utils.h"


// Initialize pointer authentication
void init_pauth(int enable_da, int enable_db, int enable_ia, int enable_ib);

// Check if `FEAT_Pauth` is implemented but not `FEAT_Pauth2`
int check_pauth1_variant(char* tname);

// Check if `FEAT_Pauth2` is implemented
int check_pauth2_variant(char* tname);

// Check if `FEAT_FPAC` is implemented iff `present`:
// FPAC change the way `aut*` instructions are executed in case of failure
int check_fpac_variant(char* tname, int present);

// Check if `FEAT_CONSTPACFIELD` is implemented
int check_const_pac_field_variant(char* tname);

// Remove the PAC field in an instruction pointer
void* strip_pauth_instruction(void* ptr);

// Remove the PAC field in a data pointer
void* strip_pauth_data(void* ptr);

#endif
