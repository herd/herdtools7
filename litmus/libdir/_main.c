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

/***************/
/* Entry point */
/***************/
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef DYNALLOC
static global_t global;
static zyva_t arg[AVAIL];
#ifndef KVM
static pthread_t th[AVAIL];
#endif
#endif

int RUN(int argc,char **argv,FILE *out) ;
int RUN(int argc,char **argv,FILE *out) {
#ifdef DYNALLOC
#ifdef HAVE_FAULT_HANDLER
  alloc_fault_handler();
#ifdef SEE_FAULTS
  alloc_see_faults();
#endif
#endif
  global_t *glo_ptr = malloc_check(sizeof(global_t));
  glo_ptr->mem = malloc_check(MEMSZ*sizeof(*glo_ptr->mem)) ;
  zyva_t *arg = malloc_check(AVAIL*sizeof(*arg));
#ifndef KVM
  pthread_t *th = malloc_check(AVAIL*sizeof(*th));
#endif
#else
  global_t *glo_ptr = &global;
  glo_ptr->mem = mem;
#endif
  init_global(glo_ptr);
#ifdef KVM
  feature_check();
#endif
#ifdef OUT
  opt_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, AVAIL, NEXE, };
  opt_t d = def ;
  char *prog = argv[0] ;
  char **p = parse_opt(argc,argv,&def,&d) ;
  int n_exe = d.n_exe ;
  if (d.avail < AVAIL) n_exe = d.avail / N ;
  if (n_exe < 1) n_exe = 1 ;
  if (n_exe > NEXE) n_exe = NEXE ;
  glo_ptr->verbose = d.verbose;
  glo_ptr->nexe = n_exe;
  glo_ptr->nruns = d.max_run;
  glo_ptr->size = d.size_of_test;
  if (glo_ptr->verbose) {
    fprintf(stderr,"%s: n=%d, r=%d, s=%d\n",prog,glo_ptr->nexe,glo_ptr->nruns,glo_ptr->size);
  }
  parse_param(prog,glo_ptr->parse,PARSESZ,p) ;
#ifdef PRELUDE
  prelude(out) ;
#endif
  tsc_t start = timeofday();
#endif
  for (int id=0; id < AVAIL ; id++) {
    arg[id].id = id;
    arg[id].g = glo_ptr;
  }
#ifdef KVM
  init_labels();
  on_cpus(zyva, arg);
#else
  for (int id=0; id < AVAIL ; id++) launch(&th[id],zyva,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);
#endif
  int nexe = glo_ptr->nexe ;
  hash_init(&glo_ptr->hash) ;
  for (int k=0 ; k < nexe ; k++) {
    hash_adds(&glo_ptr->hash,&glo_ptr->ctx[k].t) ;
  }
#ifdef OUT
  tsc_t total = timeofday()-start;
  count_t p_true = 0, p_false = 0;
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *e = &glo_ptr->hash.t[k];
    if (e->ok) {
      p_true += e->c ;
    } else {
      p_false += e->c;
    }
  }
  postlude(out,glo_ptr,p_true,p_false,total);
#endif
#ifdef DYNALLOC
#ifdef HAVE_FAULT_HANDLER
#ifdef SEE_FAULTS
  free_see_faults();
#endif
  free_fault_handler();
#endif
  free(glo_ptr->mem);
  free(glo_ptr);
  free(arg);
#ifndef KVM
  free(th);
#endif
#endif
  return EXIT_SUCCESS;
}

#ifdef MAIN
int main (int argc,char **argv) {
#ifdef KVM
  litmus_init();
#endif
  return RUN(argc,argv,stdout) ;
}
#endif
