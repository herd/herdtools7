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
#ifndef _LITMUS_IO_H
#define _LITMUS_IO_H 1

#include <stdint.h>
#include "platform_io.h"

void emit_string(FILE *out,const char *s) ;

void emit_int_uns(FILE *out,unsigned int x) ;
void emit_int_hex(FILE *out,unsigned int x) ;
void emit_int(FILE *out,int x) ;

void emit_int32_uns(FILE *out,uint32_t x) ;
void emit_int32_hex(FILE *out,uint32_t x) ;
void emit_int32(FILE *out,int32_t x) ;

void emit_int64_uns(FILE *out,uint64_t x) ;
void emit_int64_hex(FILE *out,uint64_t x) ;
void emit_int64(FILE *out,int64_t x) ;

void emit_pad_int_uns(FILE *out,char pc,int pval,unsigned int x) ;
void emit_pad_int_hex(FILE *out,char pc,int pval,unsigned int x) ;
void emit_pad_int(FILE *out,char pc,int pval,int x) ;

void emit_pad_int32_uns(FILE *out,char pc,int pval,uint32_t x) ;
void emit_pad_int32_hex(FILE *out,char pc,int pval,uint32_t x) ;
void emit_pad_int32(FILE *out,char pc,int pval,int32_t x) ;

void emit_pad_int64_uns(FILE *out,char pc,int pval,uint64_t x) ;
void emit_pad_int64_hex(FILE *out,char pc,int pval,uint64_t x) ;
void emit_pad_int64(FILE *out,char pc,int pval,int64_t x) ;

void emit_double(FILE *out,double f) ;
#endif
