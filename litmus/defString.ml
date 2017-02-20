(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(*
Put those strings in a separate file,
so as to avoid confusing auto-indentation in skel.ml
*)

  let pthread_barrier_def =
"
/* pthread based test barrier */

typedef struct {
  pb_t *b ;
  int volatile loop ;
} barrier_t ;

static barrier_t *barrier_create(void) {
  barrier_t *p = malloc_check(sizeof(*p)) ;
  p->b = pb_create(N) ;
  return p ;
}

static void barrier_free(barrier_t *p) {
  pb_free(p->b) ;
  free(p) ;
}

static void barrier_wait(int id, barrier_t *p) {
  p->loop = 1000 ;
  pb_wait(p->b) ;
  /* give a chance for everybody to start together */
  while (--(p->loop) > 0) ;
}

"

let hist_defs =
"
typedef struct hist_t {
  outs_t *outcomes ;
  count_t n_pos,n_neg ;
} hist_t ;

static hist_t *alloc_hist(void) {
  hist_t *p = malloc_check(sizeof(*p)) ;
  p->outcomes = NULL ;
  p->n_pos = p->n_neg = 0 ;
  return p ;
}

static void free_hist(hist_t *h) {
  free_outs(h->outcomes) ;
  free(h) ;
}

static void add_outcome(hist_t *h, count_t v, outcome_t o, int show) {
  h->outcomes = add_outcome_outs(h->outcomes,o,NOUTS,v,show) ;
}

static void merge_hists(hist_t *h0, hist_t *h1) {
  h0->n_pos += h1->n_pos ;
  h0->n_neg += h1->n_neg ;
  h0->outcomes = merge_outs(h0->outcomes,h1->outcomes,NOUTS) ;
}

"
