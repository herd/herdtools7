/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*                                                                   */
/*  Copyright 2014 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#ifndef _LITMUS_IO_H
#define _LITMUS_IO_H 1

#include <stdint.h>
#include "platform_io.h"

void emit_string(FILE *out,char *s) ;

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
