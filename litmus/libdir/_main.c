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

static zyva_t arg[AVAIL];
#ifndef KVM
static pthread_t th[AVAIL];
#endif

#ifdef KVM
int RUN(int argc,char **argv) {
#else
int RUN(int argc,char **argv,FILE *out) {
#endif
#ifdef OUT
  opt_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, AVAIL, NEXE, mode_random, };
  opt_t d = def ;
  char *prog = argv[0] ;
  char **p = parse_opt(argc,argv,&def,&d) ;
  int n_exe = d.n_exe ;
  if (d.avail != AVAIL) n_exe = d.avail / N ;
  if (n_exe < 1) n_exe = 1 ;
  global.verbose = d.verbose;
  global.nexe = n_exe;
  global.noccs = NOCCS ;
  global.nruns = d.max_run;
  global.size = d.size_of_test;
  global.do_scan = d.mode == mode_scan ;
  if (global.verbose) {
#ifdef KVM
    printf("%s: n=%d, r=%d, s=%d, %s\n",prog,global.nexe,global.nruns,global.size,global.do_scan ? "+sp" : "+rp");
#else
    fprintf(stderr,"%s: n=%i, r=%i, s=%i, %s\n",prog,global.nexe,global.nruns,global.size,global.do_scan ? "+sp" : "+rp");
#endif
  }
  parse_param(prog,global.parse,PARSESZ,p) ;
#ifdef PRELUDE
#ifndef KVM
  prelude(out) ;
#endif
#endif
  tsc_t start = timeofday();
#else
  global.verbose = 0 ;
  global.nexe = NEXE ;
  global.noccs = NOCCS ;
  global.nruns = NUMBER_OF_RUN ;
  global.size = SIZE_OF_TEST ;
  global.do_scan = 0;
#endif
  for (int id=0; id < AVAIL ; id++) {
    arg[id].id = id;
    arg[id].g = &global;
  }
#ifdef KVM
  /* "spawn" downwards as id 0 is not asynchornous */
  global.over = 0 ;
  for (int id = AVAIL-1 ; id >= 0 ; id--) on_cpu_async(id,zyva,&arg[id]);
  while (global.over < AVAIL) mdelay(500);
#else
  for (int id=0; id < AVAIL ; id++) launch(&th[id],zyva,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);
#endif
  int nexe = global.nexe ;
  hash_init(&global.hash) ;  
  for (int k=0 ; k < nexe ; k++) {
    hash_adds(&global.hash,&global.ctx[k].t) ;
  }
#ifdef OUT
  tsc_t total = timeofday()-start;
  count_t p_true = 0, p_false = 0;
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *e = &global.hash.t[k];
    if (e->ok) {
      p_true += e->c ;
    } else {
      p_false += e->c;
    }
  }
#ifdef KVM
  postlude(&global,p_true,p_false,total);
#else
  postlude(out,&global,p_true,p_false,total);
#endif
#endif
  return EXIT_SUCCESS;
}

#ifdef MAIN
int main (int argc,char **argv) {
#ifdef KVM
  return RUN(argc,argv) ;
#else
  return RUN(argc,argv,stdout) ;
#endif
}
#endif
