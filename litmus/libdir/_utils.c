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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>
#include <errno.h>
#include <stdint.h>
#include <stdarg.h>
#include <sys/mman.h>
#include "utils.h"

/********/
/* Misc */
/********/

FILE *errlog ;

static void checkerrlog(void) {
  if (!errlog) errlog = stderr ;
}

void seterrlog(FILE *chan) {
  errlog = chan ;
}

int log_error(const char *fmt, ...) {
  int result;
  va_list args;
  va_start(args, fmt);
  checkerrlog() ;
  result = vfprintf(errlog, fmt, args);
  fflush(errlog);
  va_end(args);
  return result;
}

void fatal(char *msg) {
  log_error("Failure: %s\n", msg) ;
  fclose(errlog);
  fprintf(stdout,"Failure: %s\n", msg) ;
  exit(1) ;
}

void errexit(char *msg,int err) {
  log_error("%s: %s\n",msg,strerror(err)) ;
  fclose(errlog);
  exit(2) ;
}

void *malloc_check(size_t sz) {
  if (sz == 0) return NULL ;
  void *p = malloc(sz) ;
  if (!p) {
    if (!errno) errno = ENOMEM ;
    errexit("malloc",errno) ;
  }
  return p ;
}

void *mmap_exec(size_t sz) {
  void * p = mmap(NULL, sz, PROT_READ|PROT_EXEC|PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED) {
    errexit("mmap",errno) ;
  }
  return p ;
}

void munmap_exec(void *p,size_t sz) {
  if (munmap(p,sz)) errexit("munmap",errno);
}

int max(int n, int m) { return n < m ? m : n ; }

void pp_ints(FILE *fp,int *p,int n) {
  if (n > 0) {
    fprintf(fp,"%i",p[0]) ;
    for (int k = 1 ; k < n ; k++) {
      fprintf(fp,",%i",p[k]) ;
    }
  }
}


void *do_align(void *p,size_t sz) {
  uintptr_t x = (uintptr_t)p ;
  x += sz-1 ;
  x /= sz ;
  x *= sz ;
  return (void *)x ;
}

void *do_noalign(void *p,size_t sz) {
  void *q = do_align(p,sz) ;
  void *r = q - sz/2 ;
  if (r < p) r = q + sz/2 ;
  return r ;
}

void cat_file(char *path, char *msg, FILE *out) {
  FILE *fp = fopen(path,"r") ;
  if (fp == NULL) return ;
  fprintf(out,"%s\n",msg) ;
  int c,nl=1 ;
  while ((c = fgetc(fp)) != EOF) {
    fputc(c,out) ;
    nl = c == '\n' ;
  }
  fclose(fp) ;
  if (!nl) fputc('\n',out) ;
}

/************/
/* CPU sets */
/************/

cpus_t *cpus_create(int sz) {
  cpus_t *r = malloc_check(sizeof(*r)) ;
  r->sz = sz ;
  r->cpu = malloc_check(sizeof(r->cpu[0])*sz)  ;
  return r ;
}

cpus_t *cpus_create_init(int sz, int t[]) {
  cpus_t *r = cpus_create(sz) ;
  for (int k = 0 ; k < sz ; k++) r->cpu[k] = t[k] ;
  return r ;
}

void cpus_free(cpus_t *p) {
  free(p->cpu) ;
  free(p) ;
}

void cpus_dump(FILE *fp, cpus_t *p) {
  pp_ints(fp,p->cpu,p->sz) ;
}

void cpus_dump_test(FILE *fp, int *p, int sz, cpus_t *cm,int nprocs) {
  for (int k = 0 ; k < sz ; k += nprocs) {
    fprintf(fp,"[") ;
    pp_ints(fp,&p[k],nprocs) ;
    fprintf(fp,"] {") ;
    if (nprocs > 0) {
      fprintf(fp,"%i",cm->cpu[p[k]]) ;
      for (int i = 1 ; i < nprocs ; i++) {
        fprintf(fp,",%i",cm->cpu[p[k+i]]) ;
      }
    }
    fprintf(fp,"}\n") ;
  }
}

/*************/
/* Int array */
/*************/


void ints_dump(FILE *fp, ints_t *p) {
  if (p->sz > 0) {
    fprintf(fp,"%i:%i",0,p->t[0]) ;
    for (int k = 1 ; k < p->sz ; k++) {
      fprintf(fp,",%i:%i",k,p->t[k]) ;
    }
  }
}

/***********************/
/* Prefetch directives */
/***********************/
void prefetch_dump(FILE *fp, prfdirs_t *p) {
  prfproc_t *q = p->t ;
  int some = 0 ;
  for (int _p = 0 ; _p < p->nthreads ; _p++) {
    int nvars = q[_p].nvars ;
    prfone_t *r = q[_p].t ;
    for (int _v = 0 ; _v < nvars ; _v++) {
      prfdir_t dir = r[_v].dir ;
      if (dir != none) {
        char c = 'I' ;
        if (dir == flush) c = 'F' ;
        else if (dir == touch) c = 'T' ;
        else if (dir == touch_store) c = 'W' ;
        if (some) {
          fprintf(fp,",") ;
        } else {
          some = 1 ;
        }
        fprintf(fp,"%i:%s=%c",_p,r[_v].name,c) ;
      }
    }
  }
}

static void set_prefetch(prfdirs_t *p, prfdir_t d) {
  prfproc_t *q = p->t ;
  for (int _p = 0 ; _p < p->nthreads ; _p++) {
    int nvars = q[_p].nvars ;
    prfone_t *r = q[_p].t ;
    for (int _v = 0 ; _v < nvars ; _v++) {
      r[_v].dir = d ;
    }
  }
}

/* ??? */

int gcd(int a, int b) {
  for ( ; ; ) {
    if (a == 0) return b ;
    int tmp = a ;
    a = b % a ;
    b = tmp ;
  }
}

/* SMT description */


cpus_t *coremap_seq(int navail, int nways) {
  cpus_t *r = cpus_create(navail) ;
  int ncores = navail / nways ;
  int i = 0 ;
  for (int c = 0 ; c < ncores ; c++) {
    for (int k = 0 ; k < nways ; k++) {
      r->cpu[i++] = c ;
    }
  }
  return r ;
}

cpus_t *coremap_end(int navail, int nways) {
  cpus_t *r = cpus_create(navail) ;
  int ncores = navail / nways ;
  int i = 0 ;
  for (int k = 0 ; k < nways ; k++) {
    for (int c = 0 ; c < ncores ; c++) {
      r->cpu[i++] = c ;
    }
  }
  return r ;
}

typedef struct {
  int ncores ;
  cpus_t **core ;
} mapcore_t ;


static void mapcore_free(mapcore_t *p) {
  for (int c = 0 ; c < p->ncores ; c++) cpus_free(p->core[c]) ;
  free(p->core) ;
  free(p) ;
}

#if 0
static mapcore_t *inverse_coremap(cpus_t *p, int nways) {
  mapcore_t *r = malloc_check(sizeof(*r)) ;
  r->ncores = p->sz / nways ;
  r->core = malloc_check(r->ncores * sizeof(r->core[0])) ;
  for (int k = 0 ; k < r->ncores ; k++) {
    r->core[k] = cpus_create(nways) ;
    r->core[k]->sz = 0 ;
  }
  for (int k = 0 ; k < p->sz ; k++) {
    int c = p->cpu[k] ;
    cpus_t *q = r->core[c] ;
    q->cpu[q->sz++] = k ;
  }
  return r ;
}
#endif

static int get_ncores(cpus_t *cm) {
  int r = 0;
  for (int k = 0 ; k < cm->sz ; k++) {
    if (cm->cpu[k] > r) r = cm->cpu[k] ;
  }
  return r+1 ;
}

cpus_t *get_core_procs(cpus_t *cm, cpus_t *p,int c) {
  int sz = 0 ;
  cpus_t *r ;
  for (int k = 0 ; k < p->sz ; k++) {
    if (cm->cpu[p->cpu[k]] == c) sz++ ;
  }
  r = cpus_create(sz) ;
  int i = 0 ;
  for (int k = 0 ; k < p->sz ; k++) {
    int proc = p->cpu[k] ;
    if (cm->cpu[proc] == c) r->cpu[i++] = proc ;
  }
  return r ;
}

static  mapcore_t *inverse_procs(cpus_t *cm, cpus_t *p) {
  int ncores = get_ncores(cm) ;
  mapcore_t *r = malloc_check(sizeof(*r)) ;
  r->ncores = ncores ;
  r->core = malloc_check(sizeof(r->core[0])*ncores) ;
  for (int c = 0 ; c < ncores ; c++) {
    r->core[c] = get_core_procs(cm,p,c) ;
  }
  return r ;
}

static int get_node_sz(int *p) {
  int r = 0 ;
  while (*p++ >= 0) r++ ;
  return r ;
}

static int get_n(int **p) {
  int r = 0 ;
  while (*p) {
    r += get_node_sz(*p) ;
    p++ ;
  }
  return r ;
}

static int ok_one_color(int *cm,int *d,int *a,int n, int p, int c) {
  for (int k = 0 ; k < n ; k++) {
    int op = a[k] ;
    if (op >= 0) {
      if (d[n*p+k]) {
        int oc = cm[op] ;
        if (oc == c) {
          return 0 ;
        }
      }
    }
  }
  return 1 ;
}

static int ok_color(int *cm,int *d,int *a,int n, int *q, int c) {
  for ( ; *q >= 0 ; q++) {
    if (!ok_one_color(cm,d,a,n,*q,c)) return 0 ;
  }
  return 1 ;
}

static int find_color_diff
(int prev,st_t *st,int *cm,mapcore_t *mc,int *d, int *a,int n, int *q) {
  int sz = get_node_sz(q) ;
  int k0 = prev >= 0 && rand_bit(st) ? prev : rand_k(st,mc->ncores) ;
  int k = k0 ;
  do {
    cpus_t *p = mc->core[k] ;
    if (p->sz >= sz && ok_color(cm,d,a,n,q,k)) return k ;
    k++ ; k %= mc->ncores ;
  } while (k != k0) ;
  return -1 ;
}


static int find_one_proc
(int prev,st_t *st,int *cm,mapcore_t *mc,int *d,int *a,int n,int p) {
  int found = -1 ;
  int k0 = prev >= 0 && rand_bit(st) ? prev : rand_k(st,mc->ncores) ;
  int k = k0 ;
  do {
    cpus_t *pk = mc->core[k] ;
    if (pk->sz > 0) {
      if (found < 0) found = k ;
      if (ok_one_color(cm,d,a,n,p,k)) return k ;
    }
    k++ ; k %= mc->ncores ;
  } while (k != k0) ;
  if (found < 0) fatal("Cannot allocate threads") ;
  return found ;
}

void custom_affinity (st_t *st,cpus_t *cm,int **color,int *diff,cpus_t *aff_cpus,int n_exe, int *r) {
  mapcore_t *mc = inverse_procs(cm,aff_cpus) ;
  int n = get_n(color) ;
  /* Diff relation as matrix */
  int d[n*n] ;
  {
    int *q = diff ;
    for (int k = 0 ; k < n*n ; k++) d[k] = 0 ;
    while (*q >= 0) {
      int x = *q++, y = *q++ ;
      d[n*x+y] = d[n*y+x] = 1 ;
    }
  }
  for (int k = 0 ; k < n_exe ; k++) {
    int *a = &r[k*n] ;
    int prev_core = -1 ;
    for (int i = 0 ; i < n ; i++) a[i] = -1 ;
    for (int **q = color ; *q ; q++) {
      int c = find_color_diff(prev_core,st,aff_cpus->cpu,mc,d,a,n,*q) ;
      if (c >= 0) {
        cpus_t *p = mc->core[c] ;
        for (int *qq = *q ; *qq >= 0 ; qq++) {
          p->sz-- ;
          a[*qq] = p->cpu[p->sz] ;
        }
        prev_core = c ;
      } else {
        for (int *qq = *q ; *qq >= 0 ; qq++) {
          int c = find_one_proc(prev_core,st,aff_cpus->cpu,mc,d,a,n,*qq) ;
          cpus_t *p = mc->core[c] ;
          p->sz-- ;
          a[*qq] = p->cpu[p->sz] ;
          prev_core = c ;
        }
      }
    }
  }
  mapcore_free(mc) ;
}

/****************/
/* Command line */
/****************/

/* usage */

static void usage(char *prog, cmd_t *d) {
  log_error("usage: %s (options)*\n",prog) ;
  log_error("  -v      be verbose\n") ;
  log_error("  -q      be quiet\n") ;
  log_error("  -a <n>  run maximal number of tests for n available processors (default %i)\n",d->avail) ;
  log_error("  -n <n>  run n tests concurrently\n") ;
  log_error("  -r <n>  perform n runs (default %i)\n",d->max_run) ;
  log_error("  -fr <f> multiply run number per f\n") ;
  log_error("  -s <n>  outcomes per run (default %i)\n",d->size_of_test) ;
  if (d->stride > 0) {
    log_error("  -st <n> stride (default %i)\n",d->stride) ;
  }
  log_error("  -fs <f> multiply outcomes per f\n") ;
  log_error("  -f <f>  multiply outcomes per f, divide run number by f\n") ;
  if (d->aff_mode != aff_none) {
    log_error("  -i <n>  increment for allocating logical processors, -i 0 disables affinity mode") ;
    if (d->aff_mode == aff_incr) {
      log_error(" (default %i)\n",d->aff_incr) ;
    } else {
      log_error("\n") ;
    }
    log_error("  -p <ns> specify logical processors (default '") ;
    cpus_dump(errlog,d->aff_cpus) ;
    log_error("')\n") ;
    log_error("  +ra     randomise affinity%s\n",d->aff_mode == aff_random ? " (default)" : "") ;
    if (d->aff_custom_enabled) {
      log_error("  +ca     enable custom affinity%s\n",d->aff_mode == aff_custom ? " (default)" : "") ;
    } else {
      log_error("  +ca     alias for +ra\n") ;
    }
    if (d->aff_scan_enabled) {
      log_error("  +sa     enable scanning affinity%s\n",d->aff_mode == aff_scan ? " (default)" : "") ;
      log_error("  +ta <topo> set topology affinity\n") ;
    } else {
      log_error("  +sa     alias for +ra\n") ;
    }
  }
  if (d->shuffle >= 0) {
    log_error("  +rm     randomise memory accesses%s\n",d->shuffle ? " (default)" : "") ;
    log_error("  -rm     do not randomise memory accesses%s\n",!d->shuffle ? " (default)" : "") ;
  }
  if (d->speedcheck >= 0) {
    log_error("  +sc     stop as soon as possible%s\n",d->speedcheck ? " (default)" : "") ;
    log_error("  -sc     run test completly%s\n",!d->speedcheck ? " (default)" : "") ;
  }
  if (!d->fix) {
    log_error("  +fix    fix thread launch order\n") ;
  }
  if (d->delta_tb) {
    log_error("  -tb <list> set timebase delays, default '") ;
    ints_dump(errlog,d->delta_tb) ;
    log_error("'\n") ;
    log_error("    List syntax is comma separated proc:delay\n") ;
    log_error("  -ta <n>    set all timebase delays\n") ;
  }
  if (d->verbose_barrier >= 0) {
    log_error("  +vb     show iteration timings%s\n",d->verbose_barrier ? " (default)" : "") ;
    log_error("  -vb     do not show iteration timings%s\n",!d->verbose_barrier ? " (default)" : "") ;
  }
  if (d->prefetch) {
    log_error("  -pra (I|F|T|W) set all prefetch\n") ;
    log_error("  -prf <list> set prefetch, default '") ;
    prefetch_dump(errlog,d->prefetch) ;
    log_error("'\n") ;
    log_error("    List syntax is comma separated proc:name=(I|F|T|W)\n") ;
  }
  if (d->static_prefetch >= 0) {
    log_error("  -prs <n> prefetch probability is 1/n, -prs 0 disables feature, default %i\n",d->static_prefetch) ;
  }
  if (d->max_loop > 0) {
    log_error("  -l <n>  measure time by running assembly in a loop of size <n> (default %i)\n",d->max_loop) ;
  }
  if (d->prelude > 0) {
    log_error("  -vp     no verbose prelude\n") ;
  }
  if (d->sync_n > 0) {
    log_error("  -k <n>  undocumented (default %i)\n",d->sync_n) ;
  }
  exit(2) ;
}

static long my_add (long x, long y) {
  long r = x+y ;
  if (r < x || r < y) { errno = ERANGE ; fatal("overflow") ; }
  return r ;
}

static long my_pow10(int p,long x) {
  long r = x ;
  for ( ; p > 0 ; p--) {
    long y2 = my_add(r,r) ;
    long y4 = my_add(y2,y2) ;
    long y8 = my_add(y4,y4) ;
    r = my_add(y8,y2) ;
  }
  if (r >= INT_MAX || r <= 0) {  errno = ERANGE ; fatal("overflow") ; }
  return r ;
}

static int do_argint(char *p, char **q) {
  long r =  strtol(p,q,10) ;
  if (errno == ERANGE) { fatal("overflow") ; }
  if (**q == 'k' || **q == 'K') { r = my_pow10(3,r) ; *q += 1; }
  else if (**q == 'm' || **q == 'M') { r = my_pow10(6,r) ; *q +=1 ; }
  return (int)r ;
}

static int argint(char *prog,char *p,cmd_t *d) {
  char *q ;
  long r = do_argint(p,&q) ;
  if (*p == '\0' || *q != '\0') {
    usage(prog,d) ;
  }
  return (int)r ;
}

static cpus_t *argcpus(char *prog,char *p0,cmd_t *d) {
  int sz = 0 ;
  char *p ;

  p = p0 ;
  for ( ; ; ) {
    char *q ;
    int x = (int)strtol(p,&q,10) ;
    if (x < 0 || *p == '\0' || (*q != '\0' && *q != ','))  usage(prog,d) ;
    sz++ ;
    if (*q == '\0') break ;
    p = q+1 ;
  }
  cpus_t *r = cpus_create(sz) ;
  p = p0 ;
  for (int k = 0 ; k < sz ; k++) {
    char *q ;
    r->cpu[k] = (int)strtol(p,&q,10) ;
    p = q+1 ;
  }
  return r ;
}

static void argints(char *prog,cmd_t *d, char *p,ints_t *r) {
  while (*p) {
    char *q ;
    int idx = (int)strtol(p,&q,10) ;
    if (idx < 0 || idx >= r->sz || *p == '\0' || *q != ':')  usage(prog,d) ;
    p = q+1 ;
    int v = do_argint(p,&q) ;
    if (*p == '\0' || (*q != '\0' && *q != ','))  usage(prog,d) ;
    r->t[idx] = v ;
    if (*q == '\0') {
      p = q ;
    } else {
      p = q+1 ;
    }
  }
}

static prfone_t *get_name_slot(prfproc_t *p,char *name) {
  int nvars = p->nvars ;
  prfone_t *q = p->t ;
  for (int _v = 0 ; _v < nvars ; _v++) {
    if (strcmp(name,q[_v].name) == 0) return &q[_v] ;
  }
  return NULL ; /* Name not found */
}


static void argoneprefetch(char *prog,cmd_t *d, char *p, prfdirs_t *r) {
  prfdir_t dir = none ;
  switch (*p) {
  case 'F':
    dir = flush ;
    break ;
  case 'T':
    dir = touch ;
    break ;
  case 'W':
    dir = touch_store ;
    break ;
  }
  set_prefetch(r,dir) ;
}

int parse_prefetch(char *p, prfdirs_t *r) {
  if (!*p) return 1 ;
  for ( ;; ) {
    char *q ;
    int proc = (int)strtol(p,&q,10) ;
    if (proc < 0 || proc >= r->nthreads || *p == '\0' || *q != ':')
      return 0 ;
    p = q+1 ;
    char *p0 = p ;
    while (*p != '=') {
      if (*p == '\0') return 0 ;
      p++ ;
    }
    *p = '\0' ;
    prfone_t *loc_slot = get_name_slot(&r->t[proc],p0) ;
    if (loc_slot == NULL) {
      log_error("Proc %i does not access variable %s\n",proc,p0) ;
      *p = '=' ;
      return 0 ;
    }
    *p = '=' ;
    char c = *++p;
    prfdir_t dir = none ;
    switch (c) {
    case 'F':
      dir = flush ;
      break ;
    case 'T':
      dir = touch ;
      break ;
    case 'W':
      dir = touch_store ;
      break ;
    }
    loc_slot->dir = dir ;
    c = *++p ;
    if (c == '\0') return 1 ;
    else if (c == ',') p++ ;
    else return 0 ;
  }
}

static void argprefetch(char *prog,cmd_t *d, char *p, prfdirs_t *r) {
  if (!parse_prefetch(p,r)) usage(prog,d) ;
}

static double argdouble(char *prog,char *p,cmd_t *d) {
  char *q ;
  double r = strtod(p,&q) ;
  if (*p == '\0' || *q != '\0') {
    usage(prog,d) ;
  }
  return r ;
}

void parse_cmd(int argc, char **argv, cmd_t *d, cmd_t *p) {
  char *prog = argv[0] ;

  /* Options */
  for ( ; ; ) {
    --argc ; ++argv ;
    if (!*argv) break ;
    char fst = **argv ;
    if (fst != '-' && fst != '+') break ;
    if (strcmp(*argv,"-q") == 0) p->verbose=0 ;
    else if (strcmp(*argv,"-v") == 0) p->verbose++ ;
    else if (strcmp(*argv,"-r") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->max_run = argint(prog,argv[0],d) ;
    } else if (strcmp(*argv,"-fr") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->max_run *= argdouble(prog,argv[0],d) ;
    } else if (strcmp(*argv,"-s") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->size_of_test = argint(prog,argv[0],d) ;
    } else if (d->stride > 0 && strcmp(*argv,"-st") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->stride = argint(prog,argv[0],d) ;
      if (p->stride <= 0) p->stride = 0 ;
    } else if (strcmp(*argv,"-fs") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->size_of_test *= argdouble(prog,argv[0],d) ;
    } else if (strcmp(*argv,"-f") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      double f = argdouble(prog,argv[0],d) ;
      p->size_of_test *= f ;
      p->max_run /= f ;
    } else if (strcmp(*argv,"-n") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->n_exe = argint(prog,argv[0],d) ;
      if (p->n_exe < 1) p->n_exe = 1 ;
    } else if (strcmp(*argv,"-a") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int a = argint(prog,argv[0],d) ;
      p->avail = a ;
    } else if (d->sync_n > 0 && strcmp(*argv,"-k") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int a = argint(prog,argv[0],d) ;
      p->sync_n = a < 0 ? 0 : a ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"-i") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int i = argint(prog,argv[0],d) ;
      p->aff_mode = aff_incr ;
      p->aff_incr = i < 0 ? 0 : i ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"-p") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      cpus_t *cpus = argcpus(prog,argv[0],d) ;
      p->aff_cpus = cpus ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"+ra") == 0) {
      p->aff_mode = aff_random ;
    } else if (d->aff_custom_enabled && strcmp(*argv,"+ca") == 0) {
      p->aff_mode = aff_custom ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"+ca") == 0) {
      p->aff_mode = aff_random ;
    } else if (d->aff_scan_enabled && strcmp(*argv,"+sa") == 0) {
      p->aff_mode = aff_scan ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"+sa") == 0) {
      p->aff_mode = aff_random ;
    } else if (d->aff_scan_enabled && strcmp(*argv,"+ta") == 0) {
      p->aff_mode = aff_topo ;
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      p->aff_topo = argv[0] ;
    } else if (d->aff_mode != aff_none && strcmp(*argv,"+sa") == 0) {
      p->aff_mode = aff_random ;
    } else if (d->shuffle >= 0 && strcmp(*argv,"+rm") == 0) {
      p->shuffle = 1 ;
    } else if (d->shuffle >= 0 && strcmp(*argv,"-rm") == 0) {
      p->shuffle = 0 ;
    } else if (d->speedcheck >= 0 && strcmp(*argv,"+sc") == 0) {
      p->speedcheck = 1 ;
    } else if (d->speedcheck >= 0 && strcmp(*argv,"-sc") == 0) {
      p->speedcheck = 0 ;
    } else if (!d->fix &&  strcmp(*argv,"+fix") == 0) {
      p->fix = 1 ;
    } else if (d->verbose_barrier >= 0 && strcmp(*argv,"+vb") == 0) {
      p->verbose_barrier++ ;
    } else if (d->verbose_barrier >= 0 && strcmp(*argv,"-vb") == 0) {
      p->verbose_barrier = 0 ;
    } else if (d->prelude > 0 && strcmp(*argv,"-vp") == 0) {
      p->prelude = 0 ;
    } else if (d->delta_tb &&  strcmp(*argv,"-tb") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      argints(prog,d,argv[0],p->delta_tb) ;
    } else if (d->delta_tb &&  strcmp(*argv,"-ta") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int da = argint(prog,argv[0],d) ;
      for (int k = 0 ; k < p->delta_tb->sz ; k++) p->delta_tb->t[k] = da ;
    } else if (d->prefetch && strcmp(*argv,"-prf") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      argprefetch(prog,d,argv[0],p->prefetch) ;
    } else if (d->prefetch && strcmp(*argv,"-pra") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      argoneprefetch(prog,d,argv[0],p->prefetch) ;
    } else  if (d->static_prefetch >= 0 &&  strcmp(*argv,"-prs") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int prs = argint(prog,argv[0],d) ;
      p->static_prefetch = prs >= 0 ? prs : 0 ;
    } else if (d->max_loop > 0 && strcmp(*argv,"-l") == 0) {
      --argc ; ++argv ;
      if (!*argv) usage(prog,d) ;
      int i = argint(prog,argv[0],d) ;
      p->max_loop = i < 1 ? 1 : i ;
    } else usage(prog,d) ;
  }

  /* Argument */
  if (argc == 0) return ;
  usage(prog,d) ;
}

/*************************/
/* Concurrency utilities */
/*************************/

/* phread based mutex */

pm_t *pm_create(void) {
  pm_t *p = malloc_check(sizeof(*p)) ;
  int ret = pthread_mutex_init(p,NULL) ;
  if (ret) { errexit("mutex_init",ret) ; }
  return p ;
}

void pm_free(pm_t *p) {
  int ret = pthread_mutex_destroy(p);
  if (ret) { errexit("mutex_destroy",ret) ; }
  free(p) ;
}

void pm_lock(pm_t *m) {
  int ret = pthread_mutex_lock(m) ;
  if (ret) { errexit("mutex_lock",ret) ; }
}

void pm_unlock(pm_t *m) {
  int ret = pthread_mutex_unlock(m) ;
  if (ret) { errexit("mutex_unlock",ret) ; }
}

/* phread condition */

static pc_t *do_pc_create(void) {
  pc_t *p = malloc_check(sizeof(*p)) ;
  p->c_mutex = pm_create() ;
  p->c_cond = malloc_check(sizeof(*(p->c_cond))) ;
  int e = pthread_cond_init(p->c_cond,NULL) ;
  if (e) { errexit("cond_init",e); }
  return p ;
}

#ifndef CACHE

pc_t *pc_create(void) { return do_pc_create(); }

void pc_free(pc_t *p) {
  pm_free(p->c_mutex) ;
  int ret = pthread_cond_destroy(p->c_cond);
  if (ret) { errexit("cond_destroy",ret); }
  free(p->c_cond) ;
  free(p) ;
}

#else

static pm_t pc_list_mutex =  PTHREAD_MUTEX_INITIALIZER;
static pc_t *pc_list = NULL;

pc_t *pc_create(void) {
  pm_lock(&pc_list_mutex);
  if (pc_list == NULL) {
    pm_unlock(&pc_list_mutex);
    return do_pc_create();
  } else {
    pc_t *r = pc_list;
    pc_list = r->next;
    r->next = NULL;
    pm_unlock(&pc_list_mutex);
    return r;
  }
}

void pc_free(pc_t *p) {
  pm_lock(&pc_list_mutex);
  p->next = pc_list ;
  pc_list = p;
  pm_unlock(&pc_list_mutex);
}
#endif

static void pc_lock(pc_t *p) {
  pm_lock(p->c_mutex) ;
}

static void pc_unlock(pc_t *p) {
  pm_unlock(p->c_mutex) ;
}

void pc_wait(pc_t *p) {
  int e = pthread_cond_wait(p->c_cond, p->c_mutex) ;
  if (e) { errexit("cond_wait",e) ; }
}

void pc_broadcast (pc_t *p) {
  int e = pthread_cond_broadcast(p->c_cond) ;
  if (e) { errexit("cond_broadcast",e) ; }
}

static void pc_signal(pc_t *p) {
  int e = pthread_cond_signal(p->c_cond);
  if (e) errexit("cond_signal",e) ;
}


/* pthread based barrier, usable for nproc threads */


pb_t *pb_create(int nprocs) {
  pb_t *p = malloc_check(sizeof(*p)) ;
  p->cond = pc_create() ;
  p->count = p->nprocs = nprocs ;
  p->turn = 0 ;
  return p ;
}

void pb_free(pb_t *p) {
  pc_free(p->cond) ;
  free(p) ;
}

/* The following code should protect us against spurious wake ups */
void pb_wait(pb_t *p) {
  pc_lock(p->cond) ;
  int t = p->turn ;
  --p->count ;
  if (p->count == 0) {
    p->count = p->nprocs ;
    p->turn = !t ;
    pc_broadcast(p->cond) ;
  } else {
    do {
      pc_wait(p->cond) ;
    } while (p->turn == t) ;
  }
  pc_unlock(p->cond) ;
}


/* pthread based or flag */

po_t *po_create(int nprocs) {
  po_t *p = malloc_check(sizeof(*p)) ;
  p->cond = pc_create() ;
  p->nprocs = p->count = nprocs ;
  p->val = 0 ;
  p->turn = 0 ;
  return p ;
}

void po_free(po_t *p) {
  pc_free(p->cond) ;
  free(p) ;
}

void po_reinit(po_t *p) {
  pc_lock(p->cond) ;
  int t = p->turn ;
  --p->count ;
  if (p->count == 0) {
    p->count = p->nprocs ;
    p->val = 0 ;
    p->turn = !t ;
    pc_broadcast(p->cond) ;
  } else {
    do {
      pc_wait(p->cond) ;
    } while (p->turn == t) ;
  }
  pc_unlock(p->cond) ;
}

int po_wait(po_t *p, int v) {
  pc_lock(p->cond) ;
  int t = p->turn ;
  --p->count ;
  p->val = p->val || v ;
  if (p->count == 0) {
    p->count = p->nprocs ;
    p->turn = !t ;
    pc_broadcast(p->cond) ;
  } else {
    do {
      pc_wait(p->cond) ;
    } while (p->turn == t) ;
  }
  int r = p->val ;
  pc_unlock(p->cond) ;
  return r ;
}


/* One place buffer */

op_t *op_create(void) {
  op_t *p = malloc_check(sizeof(*p)) ;
  p->cond = pc_create() ;
  p->val = NULL ;
  p->some = 0 ;
  return p;
}

void op_free(op_t *p) {
  pc_free(p->cond) ;
  free(p) ;
}

void op_set(op_t *p, void *v) {
  pc_lock(p->cond) ;
  if (p->some) { fatal("op_set") ; }
  p->val = v ;
  p->some = 1 ;
  pc_signal(p->cond) ;
  pc_unlock(p->cond) ;
}

void *op_get(op_t *p) {
  void *v = NULL ;
  pc_lock(p->cond) ;
  while (!p->some) {
    pc_wait(p->cond) ;
  }
  v = (void *) p->val ;
  p->val = NULL ;
  p->some = 0 ;
  pc_unlock(p->cond) ;
  return v ;
}

/* Thread launch and join */

void launch(pthread_t *th, f_t *f, void *a) {
  int e = pthread_create(th,NULL,f,a);
  if (e) errexit("phread_create",e);
}

void *join(pthread_t *th) {
  void *r ;
  int e = pthread_join(*th,&r) ;
  if (e)  errexit("pthread_join",e);
  return r ;
}

static void do_launch_detached(f_t *f, void *a) {
  pthread_t th;
  pthread_attr_t attr;
  int e = pthread_attr_init(&attr);
  if (e) errexit("phread_attr",e);
  e = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (e) errexit("pthread_attr_setdetachstate",e);
  e = pthread_create(&th,&attr,f,a);
  if (e) errexit("phread_create",e);
  e = pthread_attr_destroy(&attr);
  if (e) errexit("phread_attr_destroy",e);
}

/* Detached */

typedef struct {
  f_t *f;
  void *a ;
  op_t *op;
} detarg_t ;

static void *zyva_det(void *_b) {
  detarg_t *b = (detarg_t *)_b;
  f_t *f = b->f ;
  void *a = b->a ;
  op_t *op = b->op ;
  free(b) ;
  void *r = f(a) ;
  op_set(op,r) ;
  return NULL ;
}

op_t *launch_detached(f_t *f,void *a) {
  op_t *op = op_create() ;
  detarg_t *b = malloc_check(sizeof(*b)) ;
  b->f = f ; b->a = a; b->op = op ;
  do_launch_detached(zyva_det,b) ;
  return op ;
}

void *join_detached(op_t *op) {
  void *r = op_get(op) ;
  op_free(op) ;
  return r ;
}

/*********/
/* Cache */
/*********/

/* Table of detached arguments */

typedef struct {
  int sz,next;
  detarg_t *t;
} table_t;

static table_t *table_create(void) {
  table_t *r = malloc_check(sizeof(*r)) ;
  r->next = 0 ;
  r->sz = 0 ;
  r->t = NULL ;
  return r ;
}

static void table_push(table_t *t,detarg_t p) {
  if (t->next >= t->sz) {
    t->sz = 2*t->sz+1;
    t->t = realloc(t->t,t->sz*sizeof(t->t[0]));
  }
  t->t[t->next++] = p;
}

static detarg_t table_pop(table_t *t) {
  return t->t[--t->next];
}

/* Task pool */

typedef struct {
  int n_pool; // waiting in pool
  int n_out ; // outside pool
  int n_thread; // total number of threads
  pc_t *c_wait;
  table_t *t;
  int nop ;
} pool_t;

static pool_t *pool;

static pool_t *pool_create(void) {
  pool_t *r = malloc_check(sizeof(*r));
  r->n_pool = r->n_out = r->n_thread = 0;
  r->c_wait = pc_create();
  r->t = table_create();
  r->nop = 0 ;
  return r;
}

void set_pool(void) {
  if (pool == NULL) pool = pool_create();
}

#ifdef VERB
inline static int check_verb(int ok) { return ok; }
#else
inline static int check_verb(int ok) { return 0; }
#endif

static void pool_status(char *msg,pool_t *p,int force) {
  int sum = p->n_pool+p->n_out+p->t->next;;
  int ok = sum == p->n_thread;
  force = force || p->n_pool <= 0 ;

  if (!ok || force || check_verb(++p->nop > 8192)) {
    p->nop = 0;
    log_error("%s: npool=%02i, nout=%02i, ntable=%02i, sum=%i\n",msg,p->n_pool,p->n_out,p->t->next,sum);
  }
  if (!ok) fatal("Cache pool invariant violation");

}

/* Returns task to run, called by cached threads, see zyva_cache below */
static detarg_t pool_get(pool_t *pool) {
  detarg_t r;
  pc_lock(pool->c_wait);
  pool->n_pool++; pool->n_out--;
  pool_status("GET",pool,0);
  while (pool->t->next <= 0) pc_wait(pool->c_wait);
  r = table_pop(pool->t);
  pool->n_out++;
  pc_unlock(pool->c_wait);
  return r ;
}

/* Code for cached threads, a first task is run, then wait on pool for the next task */

typedef struct {
  pool_t *pool;
  detarg_t arg;
} cachearg_t;

static void *zyva_cache(void *_a) {
  cachearg_t *a = (cachearg_t *)_a;
  detarg_t b = a->arg;
  pool_t *pool = a->pool;
  free(a);

  for ( ; ; ) {
    f_t *f = b.f ;
    void *a = b.a ;
    op_t *op = b.op ;
    void *r = f(a) ;
    op_set(op,r) ;
    b = pool_get(pool);
  }
  return NULL;
}

/*
  Send task to pool, there are two cases
   1. No thread in pool, then launch a new cached thread.
   2. Some thread in pool, then signal
*/

static void pool_put(pool_t *pool,detarg_t p) {
  pc_lock(pool->c_wait) ;
  if (pool->n_pool <= 0) {
    pool->n_out++;  pool->n_thread++;
    pool_status("CAC",pool,1);
    pc_unlock(pool->c_wait);
    cachearg_t *a = malloc_check(sizeof(*a));
    a->pool = pool; a->arg = p;
    do_launch_detached(zyva_cache,a);
  } else {
    pool_status("PUT",pool,0);
    table_push(pool->t,p);
    pool->n_pool--; /* reserve one thread */
    pc_broadcast(pool->c_wait);
    pc_unlock(pool->c_wait) ;
  }
}

op_t *launch_cached(f_t *f,void *a) {
  op_t *op = op_create() ;
  detarg_t b ;
  b.f = f ; b.a = a; b.op = op ;
  pool_put(pool,b);
  return op ;
}

/*****************/
/* Random things */
/*****************/

void perm_prefix_ints(unsigned *st,int *_t, int m, int n) {
  int k;
  for (k = 0 ; k < m ; k++) {
    int j = k+rand_k(st,n-k);
    int x = _t[k]; _t[k] = _t[j]; _t[j] = x;
  }
}

void perm_ints(unsigned *st,int *_t, int n) {
  perm_prefix_ints(st, _t,n-1,n) ;
}

void perm_funs(unsigned *st,f_t *fun[], int n) {
  int k;
  for (k = 0 ; k < n-1 ; k++) {
    int j = k+rand_k(st,n-k);
    f_t *t = fun[j];
    fun[j] = fun[k]; fun[k] = t;
  }
}

void perm_ops(unsigned *st,op_t *op[], int n) {
  int k;
  for (k = 0 ; k < n-1 ; k++) {
    int j = k+rand_k(st,n-k);
    op_t *t = op[j];
    op[j] = op[k]; op[k] = t;
  }
}

void perm_threads(unsigned *st,pthread_t thread[], int n) {
  int k;
  for (k = 0 ; k < n-1 ; k++) {
    int j = k+rand_k(st,n-k);
    pthread_t t = thread[j];
    thread[j] = thread[k]; thread[k] = t;
  }
}

static int int_cmp(const void *_p, const void *_q) {
  int x = *((int *)_p) ;
  int y = *((int *)_q) ;
  if (x < y) return -1 ;
  else if (x > y) return 1 ;
  else return 0 ;
}

int check_shuffle(int **t, int *min, int sz) {
  int *idx = malloc_check(sizeof(*idx)*sz) ;
  for (int k=0 ; k < sz ; k++) {
    idx[k] = (int)(t[k] - min) ;
    //    fprintf(stderr," %i",idx[k]) ;
  }
  //  fprintf(stderr,"\n") ;
  qsort(&idx[0],sz, sizeof(idx[0]), int_cmp) ;
  for (int k=0 ; k < sz ; k++) {
    if (idx[k] != k) {
      free(idx) ;
      return 0 ;
    }
  }
  free(idx) ;
  return 1 ;
}

/****************/
/* Time counter */
/****************/

#include <sys/time.h>
#include <time.h>

tsc_t timeofday(void) {
  struct timeval tv ;
  if (gettimeofday(&tv,NULL)) errexit("gettimeoday",errno) ;
  return tv.tv_sec * ((tsc_t)1000000) + tv.tv_usec ;
}

double tsc_ratio(tsc_t t1, tsc_t t2) {
  return ((double) t1) / ((double)t2) ;
}


double tsc_millions(tsc_t t) {
  return t / 1000000.0 ;
}

/*******************/
/* String handling */
/*******************/

int find_string(char *t[], int sz, char *s) {
  for (int k = 0 ; k < sz ; k++) {
    if (strcmp(t[k],s) == 0) return k ;
  }
  return -1 ;
}
