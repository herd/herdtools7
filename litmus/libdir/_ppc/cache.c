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
inline static void cache_flush(void *p) {
asm __volatile__ ("dcbf 0,%[p]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch(void *p) {
asm __volatile__ ("dcbt 0,%[p]" :: [p] "r" (p) : "memory");
}

inline static void cache_touch_store(void *p) {
asm __volatile__ ("dcbtst 0,%[p]" :: [p] "r" (p) : "memory");
}
