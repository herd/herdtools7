/***************/
/* Entry point */
/***************/

static zyva_t arg[AVAIL];
static pthread_t th[AVAIL];

int RUN(int argc,char **argv,FILE *out) {
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
    fprintf(stderr,"%s: n=%i, r=%i, s=%i, %s\n",prog,global.nexe,global.nruns,global.size,global.do_scan ? "+sp" : "+rp");
  }
  parse_param(prog,global.parse,PARSESZ,p) ;
#ifdef PRELUDE
  prelude(out) ;
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
  for (int id=0; id < AVAIL ; id++) launch(&th[id],zyva,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);

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
  postlude(out,&global,p_true,p_false,total);
#endif
  return EXIT_SUCCESS;
}

#ifdef MAIN
int main (int argc,char **argv) {
  return RUN(argc,argv,stdout) ;
}
#endif
