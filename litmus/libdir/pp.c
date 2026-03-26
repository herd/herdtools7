/****************************************************************************/
/*                           the diy toolsuite                              */
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
#include <pp.h>
#include <libcflat.h>

void pp_faults(int nthreads, count_t *nfaults, char *doc_name) {
  count_t total=0;
  for (int k=0 ; k < nthreads; k++) { total += nfaults[k]; }
  if (total > 0) {
    printf("Faults %s %"PRIu32"",doc_name,total);
    for (int k = 0 ; k < nthreads ; k++) {
      count_t c = nfaults[k];
      if (c > 0) printf(" P%d:%"PRIu32"",k,c);
    }
    printf("\n");
  }
}
