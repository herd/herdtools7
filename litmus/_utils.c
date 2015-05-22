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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>
#include <errno.h>
#include <stdint.h>
#include "utils.h"

/********/
/* Misc */
/********/

void fatal(char *msg) {
  fprintf(stderr,"Failure: %s\n", msg) ;
  fprintf(stdout,"Failure: %s\n", msg) ;
  exit(1) ;
}

void errexit(char *msg,int err) {
  fprintf(stderr,"%s: %s\n",msg,strerror(err)) ;
  exit(2) ;
}

void *malloc_check(size_t sz) {
  if (sz == 0) return NULL ;
  void *p = malloc(sz) ;
  if (!p) { 
    if (!errno) errno = ENOMEM ;
    perror("malloc") ;
    exit(2) ;
  }
  return p ;
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
  fprintf(stderr,"usage: %s (options)*\n",prog) ;
  fprintf(stderr,"  -v      be verbose\n") ;
  fprintf(stderr,"  -q      be quiet\n") ;
  fprintf(stderr,"  -a <n>  run maximal number of tests for n available processors (default %i)\n",d->avail) ;
  fprintf(stderr,"  -n <n>  run n tests concurrently\n") ;
  fprintf(stderr,"  -r <n>  perform n runs (default %i)\n",d->max_run) ;
  fprintf(stderr,"  -fr <f> multiply run number per f\n") ;
  fprintf(stderr,"  -s <n>  outcomes per run (default %i)\n",d->size_of_test) ;
  if (d->stride > 0) {
    fprintf(stderr,"  -st <n> stride (default %i)\n",d->stride) ;
  }
  fprintf(stderr,"  -fs <f> multiply outcomes per f\n") ;
  fprintf(stderr,"  -f <f>  multiply outcomes per f, divide run number by f\n") ;
  if (d->aff_mode != aff_none) {
    fprintf(stderr,"  -i <n>  increment for allocating logical processors, -i 0 disables affinity mode") ;
    if (d->aff_mode == aff_incr) {
      fprintf(stderr," (default %i)\n",d->aff_incr) ;
    } else {
      fprintf(stderr,"\n") ;
    }
    fprintf(stderr,"  -p <ns> specify logical processors (default '") ;
    cpus_dump(stderr,d->aff_cpus) ;
    fprintf(stderr,"')\n") ;
    fprintf(stderr,"  +ra     randomise affinity%s\n",d->aff_mode == aff_random ? " (default)" : "") ;
    if (d->aff_custom_enabled) {
      fprintf(stderr,"  +ca     enable custom affinity%s\n",d->aff_mode == aff_custom ? " (default)" : "") ;
    } else {
      fprintf(stderr,"  +ca     alias for +ra\n") ;
    }
    if (d->aff_scan_enabled) {
      fprintf(stderr,"  +sa     enable scanning affinity%s\n",d->aff_mode == aff_scan ? " (default)" : "") ;
      fprintf(stderr,"  +ta <topo> set topology affinity\n") ;
    } else {
      fprintf(stderr,"  +sa     alias for +ra\n") ;
    }
  }
  if (d->shuffle >= 0) {
    fprintf(stderr,"  +rm     randomise memory accesses%s\n",d->shuffle ? " (default)" : "") ;
    fprintf(stderr,"  -rm     do not randomise memory accesses%s\n",!d->shuffle ? " (default)" : "") ;
  }
  if (d->speedcheck >= 0) {
    fprintf(stderr,"  +sc     stop as soon as possible%s\n",d->speedcheck ? " (default)" : "") ;
    fprintf(stderr,"  -sc     run test completly%s\n",!d->speedcheck ? " (default)" : "") ;
  }   
  if (!d->fix) {
    fprintf(stderr,"  +fix    fix thread launch order\n") ;
  }
  if (d->delta_tb) {
    fprintf(stderr,"  -tb <list> set timebase delays, default '") ;    
    ints_dump(stderr,d->delta_tb) ;
    fprintf(stderr,"'\n") ;
    fprintf(stderr,"    List syntax is comma separated proc:delay\n") ;
    fprintf(stderr,"  -ta <n>    set all timebase delays\n") ;
  }
  if (d->verbose_barrier >= 0) {
    fprintf(stderr,"  +vb     show iteration timings%s\n",d->verbose_barrier ? " (default)" : "") ;
    fprintf(stderr,"  -vb     do not show iteration timings%s\n",!d->verbose_barrier ? " (default)" : "") ;
  }
  if (d->prefetch) {
    fprintf(stderr,"  -pra (I|F|T|W) set all prefetch\n") ;
    fprintf(stderr,"  -prf <list> set prefetch, default '") ;
    prefetch_dump(stderr,d->prefetch) ;
    fprintf(stderr,"'\n") ;
    fprintf(stderr,"    List syntax is comma separated proc:name=(I|F|T|W)\n") ;
  }
  if (d->static_prefetch >= 0) {
    fprintf(stderr,"  -prs <n> prefetch probability is 1/n, -prs 0 disables feature, default %i\n",d->static_prefetch) ;
  }
  if (d->max_loop > 0) {
    fprintf(stderr,"  -l <n>  measure time by running assembly in a loop of size <n> (default %i)\n",d->max_loop) ;
  }  
  if (d->prelude > 0) {
    fprintf(stderr,"  -vp     no verbose prelude\n") ;
  }
  if (d->sync_n > 0) {
    fprintf(stderr,"  -k <n>  undocumented (default %i)\n",d->sync_n) ;
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
      fprintf(stderr,"Proc %i does not access variable %s\n",proc,p0) ;
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
      if (p->stride <= 0) p->stride = 1 ;
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

pc_t *pc_create(void) {
  pc_t *p = malloc_check(sizeof(*p)) ;
  p->c_mutex = pm_create() ;
  p->c_cond = malloc_check(sizeof(*(p->c_cond))) ;
  int e = pthread_cond_init(p->c_cond,NULL) ;
  if (e) { errexit("cond_init",e); }
  return p ;
}

void pc_free(pc_t *p) {
  pm_free(p->c_mutex) ;
  free(p->c_cond) ;
  free(p) ;
}

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

/* Thread cache */

void *start_thread(void *_a) {
  sarg_t *_b = (sarg_t *)_a ;
  for (int _k = _b->max_run ; _k > 0 ; _k--) {
    void *_c = op_get(_b->op_arg) ;
    f_t *f = (f_t *)_c ;
    if (f == NULL) break ;
    void *ret = f(_b->arg) ;
    op_set(_b->op_ret,ret) ;
  }
  return NULL ;
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

