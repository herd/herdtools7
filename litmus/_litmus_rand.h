/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*        Susmit Sarkar, University of Cambridge, UK.                */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#ifndef _LITMUS_RAND_H
#define _LITMUS_RAND_H 1

#include <stdint.h>

/* type of state for pseudorandom  generators */
typedef uint32_t st_t ;

/* Unlocked random bit */

int rand_bit(st_t *st) ;
uint32_t rand_k(st_t *st,uint32_t n) ;

#endif
