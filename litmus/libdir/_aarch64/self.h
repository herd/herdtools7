/****************************************************************************/
61;8203;1c/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2026-present Institut National de Recherche en Informatique et */
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
#ifndef SELF_H
#define SELF_H 1
#include <stdint.h>


uint32_t getcachelinesize(void);

extern uint32_t cache_line_size;

void selfbar(void *p);

void isync(void);

int check_dic_idc(int need_dic, int need_idc);
#endif
