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
void init_pauth(void);

// Check if `FEAT_Pauth2` is implemented
int check_pac_variant(char* tname);

// Check if `FEAT_FPAC` is implemented iff `present`:
// FPAC change the way `aut*` instructions are executed in case of failure
int check_fpac_variant(char* tname, int present);

// Check if `FEAT_CONSTPACFIELD` is implemented
int check_const_pac_field_variant(char* tname);

#endif
