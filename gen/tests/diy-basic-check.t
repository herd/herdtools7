A test for no metadata, `-metadata false`
  $ diyone7 -arch AArch64 -metadata false PodWR Fre PodWR Fre
  AArch64 SB
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
A diy7 test for repeated nested predicates
  $ diy7 -arch AArch64 -relax '[@before(@before(Po)) PodRW]' -unfold-only 2>&1 | grep -v '^$'
  ***relax***
  [@before(PosWR),PodRW] [@before(PosRR),PodRW] [@before(PodWR),PodRW] [@before(PodRR),PodRW]
  ***safe***
  ***reject***
A diy7 test for conflicting nested predicates
  $ diy7 -arch AArch64 -relax '[@before(@after(Po)) PodRW]' -unfold-only 2>&1
  diy7: before and after predicates cannot apply to the same edge
  [2]
A diy7 predicate merge test for before/after boundary predicates
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[@before(Po) PodRW Rfe @after(Po)]' -safe Po 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [@before(Po) PodRW Rfe @after(Po)] -safe Po
  Generator produced 2 tests
  LB000: PosRR PodRW Rfe PosRR PodRW Rfe
  LB001: PodRR PodRW Rfe PodRR PodRW Rfe
A diy7 predicate merge test for repeated after predicates
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[PodRW Rfe @after(PodRW) @after(Rfe)]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [PodRW Rfe @after(PodRW) @after(Rfe)]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 predicate merge test for after on composite relaxations
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[PodRW Rfe @after([PodRW Rfe])]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [PodRW Rfe @after([PodRW Rfe])]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 predicate merge test for before on composite relaxations
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[@before([PodRW Rfe]) PodRW Rfe]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [@before([PodRW Rfe]) PodRW Rfe]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 with predicate unfolds composite arguments
  $ diy7 -arch AArch64 -relax '[PodRW Rfe @with(PodRW Rfe)]' -unfold-only 2>&1 | grep -v '^$'
  ***relax***
  [PodRW,Rfe,@with(PodRW),@with(Rfe)]
  ***safe***
  ***reject***
A diy7 with predicate accepts identical anchored boundaries
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @with(PodRW Rfe)]' '[@with(PodRW Rfe) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@with(PodRW),@with(Rfe)]` `[@with(PodRW),@with(Rfe),PodRW,Rfe]` passes the internal filter in mode `default`
A diy7 with predicate rejects an unmatched boundary
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @with(PodRW Rfe)]' '[PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@with(PodRW),@with(Rfe)]` `[PodRW,Rfe]` is prohibited in the internal filter in mode `default`
A diy7 with predicate then checks remaining after predicates
  $ diy7 -arch AArch64 -filter-check '[PodRW @after(Rfe) @with(PodRW)]' '[@with(PodRW) Rfe]' 2>&1
  Sequence `[PodRW,@after(Rfe),@with(PodRW)]` `[@with(PodRW),Rfe]` passes the internal filter in mode `default`
A diy7 with predicate then checks remaining before predicates
  $ diy7 -arch AArch64 -filter-check '[PodRW @with(Rfe)]' '[@with(Rfe) @before(PodRW) PodRW]' 2>&1
  Sequence `[PodRW,@with(Rfe)]` `[@with(Rfe),@before(PodRW),PodRW]` passes the internal filter in mode `default`
A diy7 with predicate rejects incompatible remaining after predicates
  $ diy7 -arch AArch64 -filter-check '[PodRW @after(PodRW) @with(PodRW)]' '[@with(PodRW) Rfe]' 2>&1
  Sequence `[PodRW,@after(PodRW),@with(PodRW)]` `[@with(PodRW),Rfe]` is prohibited in the internal filter in mode `default`
A diy7 state predicate is transparent between with predicates
  $ diy7 -arch AArch64 -filter-check '[PodRW @with(PodRW) @state(S) @with(Rfe)]' '[@with(PodRW) @state(S) @with(Rfe) Rfe]' 2>&1
  Sequence `[PodRW,@with(PodRW),@state(S),@with(Rfe)]` `[@with(PodRW),@state(S),@with(Rfe),Rfe]` passes the internal filter in mode `default`
A diy7 with predicate materialises one copy of the matched sequence
  $ diy7 -arch AArch64 -cycleonly true -size 2 -exact -relax '[PodRW Rfe @with(PodRW Rfe)]' -safe '[@with(PodRW Rfe) PodRW Rfe]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 2 -exact -relax [PodRW Rfe @with(PodRW Rfe)] -safe [@with(PodRW Rfe) PodRW Rfe]
  Generator produced 1 tests
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 state predicate unfold test preserves state tags
  $ diy7 -arch AArch64 -relax '[@state(ImpTagObs)|@state(ExpObs) PodWW L]' -unfold-only 2>&1 | grep -v '^$'
  ***relax***
  [@state(ExpObs),PodWW,L] [@state(ImpTagObs),PodWW,L]
  ***safe***
  ***reject***
A diy7 state predicate filter check accepts matching states
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(ImpTagObs)]' '[@state(ImpTagObs) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(ImpTagObs)]` `[@state(ImpTagObs),PodRW,Rfe]` passes the internal filter in mode `default`
A diy7 state predicate filter check compares state sets
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(A) @state(B)]' '[@state([B A]) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(A),@state(B)]` `[@state(A),@state(B),PodRW,Rfe]` passes the internal filter in mode `default`
A diy7 state predicate filter check rejects different state sets
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(A) @state(B)]' '[@state(A) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(A),@state(B)]` `[@state(A),PodRW,Rfe]` is prohibited in the internal filter in mode `default`
A diy7 state predicate filter check rejects plain neighbours
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(ImpTagObs)]' '[PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(ImpTagObs)]` `[PodRW,Rfe]` is prohibited in the internal filter in mode `default`
A diy7 state predicate filter check rejects different states
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(ImpTagObs)]' '[@state(ExpObs) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(ImpTagObs)]` `[@state(ExpObs),PodRW,Rfe]` is prohibited in the internal filter in mode `default`
A diy7 state predicate can be mixed with an after predicate
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(A) @after(PodRW)]' '[@state(A) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(A),@after(PodRW)]` `[@state(A),PodRW,Rfe]` passes the internal filter in mode `default`
A diy7 state predicate can be mixed with a before predicate
  $ diy7 -arch AArch64 -filter-check '[PodRW Rfe @state(A)]' '[@before(Rfe) @state(A) PodRW Rfe]' 2>&1
  Sequence `[PodRW,Rfe,@state(A)]` `[@before(Rfe),@state(A),PodRW,Rfe]` passes the internal filter in mode `default`
A diy7 state predicate cycle test accepts matching state boundaries
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[@state(A) PodRW Rfe @state(A)]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [@state(A) PodRW Rfe @state(A)]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 state transition predicate generates a cycle
  $ diy7 -arch AArch64 -cycleonly true -nprocs 2 -eprocs -safe '[@state(A) PodWW Rfe @state(L)] [@state(L) PodRR Fre @state(A)]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -nprocs 2 -eprocs -safe [@state(A) PodWW Rfe @state(L)] [@state(L) PodRR Fre @state(A)]
  Generator produced 1 tests
  MP000: PodWW Rfe PodRR Fre
A diy7 bare state predicate reports an error
  $ diy7 -arch AArch64 -relax '@state(ImpTagObs)' -unfold-only 2>&1
  diy7: predicate state(ImpTagObs) cannot be used without a relaxation.
  [2]
A diy7 state predicate accepts a set of words
  $ diy7 -arch AArch64 -relax '[PodWW @state([ImpTagObs ExpObs])]' -unfold-only 2>&1 | grep -v '^$'
  ***relax***
  [PodWW,@state(ExpObs),@state(ImpTagObs)]
  ***safe***
  ***reject***
A diy7 predicate reject cannot silently fall back to default relaxations
  $ diy7 -arch AArch64 -safe '[@before([PodRW Rfe]) PodRW Rfe]' -reject '[@before([PodRW Rfe]) PodRW Rfe]' -size 2 -exact -stdout 2>&1
  diy7: Fatal error: relaxations provided in safelist could not be used to generate cycles
  [2]
A VMSA test for a negated exists check, `-neg true`
  $ diyone7 -arch AArch64 -neg true -info "User-define=User-define" PodWR Fre PodWR Fre
  AArch64 SB
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  ~exists (0:X3=0 /\ 1:X3=0)
A test for observing locations, `-cond observe`
  $ diyone7 -arch AArch64 -info "User-define=User-define" -cond observe PodWR Fre PodWR Fre
  AArch64 SB
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  locations [x; y; 0:X3; 1:X3;]
  forall (true)
A test for a forall check, `-cond unicond`
  $ diyone7 -arch AArch64 -info "User-define=User-define" -cond unicond PodWR Fre PodWR Fre
  AArch64 SB
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  forall (true /\ ([x]=1 /\ ([y]=1 /\ (0:X3=1 /\ (1:X3=1 \/ 1:X3=0) \/ 0:X3=0 /\ (1:X3=1 \/ 1:X3=0)))))
A memtag generation test with `Variant` duplicated in metadata, because of (1) `-info "Variant=memtag"` and (2) automatically generated `Variant=memtag`
  $ diyone7 -arch AArch64 -variant memtag -info "Variant=memtag" PodWR Fre PodWR Fre
  AArch64 SB
  Variant=memtag memtag
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  "PodWR Fre PodWR Fre"
  {
   0:X1=x:green; 0:X2=y:green;
   1:X1=x:green; 1:X2=y:green;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
A C test for exists
  $ diyone7 -arch C PodWR Fre PodWR Fre
  Warning: optimised conditions are not supported by C arch
  C SB
  "PodWR Fre PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 1;
    int r0 = *y;
  }
  
  P1 (volatile int* y,volatile int* x) {
    *y = 1;
    int r0 = *x;
  }
  
  exists (0:r0=0 /\ 1:r0=0)
A C test for negated exists
  $ diyone7 -arch C -neg true PodWR Fre PodWR Fre
  Warning: optimised conditions are not supported by C arch
  C SB
  "PodWR Fre PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 1;
    int r0 = *y;
  }
  
  P1 (volatile int* y,volatile int* x) {
    *y = 1;
    int r0 = *x;
  }
  
  ~exists (0:r0=0 /\ 1:r0=0)
A C test for forall
  $ diyone7 -arch C -cond unicond PodWR Fre PodWR Fre
  Warning: optimised conditions are not supported by C arch
  C SB
  "PodWR Fre PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 1;
    int r0 = *y;
  }
  
  P1 (volatile int* y,volatile int* x) {
    *y = 1;
    int r0 = *x;
  }
  
  forall (true /\ ([x]=1 /\ ([y]=1 /\ (0:r0=1 /\ (1:r0=1 \/ 1:r0=0) \/ 0:r0=0 /\ (1:r0=1 \/ 1:r0=0)))))
An invalid `diyone7` input that expands to several cycles
  $ diyone7 -arch AArch64 'PodWR|Fre'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 '[PodWR|Fre]'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 'PodWR?'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 'PodWR|[Fre,PodWR]'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `default` mode
  $ diy7 -arch AArch64 -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `default`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `free` mode
  $ diy7 -arch AArch64 -mode free -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `free`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `sc` mode
  $ diy7 -arch AArch64 -mode sc -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `sc`

Backward-compatible edge aliases are accepted by the edge parser
  $ diy7 -arch AArch64 -relax 'Dp** Ctrl** DpData' -unfold-only 2>&1
  ***relax***
  DpAddrsW DpAddrsR DpAddrdW DpAddrdR DpDatasW DpDatasR DpDatadW DpDatadR DpCtrlIsbsW DpCtrlIsbsR DpCtrlIsbdW DpCtrlIsbdR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'DpAddr*W DpAddrd* DpData' -unfold-only 2>&1
  ***relax***
  DpAddrsW DpAddrdW DpAddrdR DpDatasW DpDatasR DpDatadW DpDatadR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax R -unfold-only 2>&1
  ***relax***
  Read
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax W -unfold-only 2>&1
  ***relax***
  Write
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax Ws -unfold-only 2>&1
  ***relax***
  Coi Coe
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'Wse Wsi' -unfold-only 2>&1
  ***relax***
  Coi Coe
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax Rmw -unfold-only 2>&1
  ***relax***
  LxSx
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax Amo -unfold-only 2>&1
  ***relax***
  Amo.Swp Amo.Cas Amo.LdAdd Amo.LdEor Amo.LdSet Amo.LdClr Amo.StAdd Amo.StEor Amo.StSet Amo.StClr
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax Amo.Safe -unfold-only 2>&1
  ***relax***
  Amo.Swp Amo.Cas Amo.LdAdd Amo.StAdd
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax LxSx -unfold-only 2>&1
  ***relax***
  LxSx
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax FencedWW -unfold-only 2>&1
  ***relax***
  DMB.SYdWW
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'Fence***' -unfold-only 2>&1
  ***relax***
  DMB.SYsWW DMB.SYsWR DMB.SYsRW DMB.SYsRR DMB.SYdWW DMB.SYdWR DMB.SYdRW DMB.SYdRR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -variant ifetch -relax 'Iff*' -unfold-only 2>&1
  ***relax***
  RfiPI RfePI
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -variant ifetch -relax 'Iffi Irfi Fifi Ifri Iffe Irfe Fife Ifre' -unfold-only 2>&1
  ***relax***
  RfiPI RfePI FriIP FreIP
  ***safe***
  
  ***reject***
  

`PPO` unfolds to concrete PPC relaxations in `diy7 -unfold-only`
  $ diy7 -arch PPC -relax PPO -unfold-only
  ***relax***
  DpAddrdR [DpAddrdR,DpAddrdR] [DpAddrdR,DpDatadW] [DpAddrdR,DpCtrldW] [DpAddrdR,DpCtrlIsyncdR] DpDatadW [DpDatadW,PosWR] [DpDatadW,PosWR,DpAddrdR] [DpDatadW,PosWR,DpDatadW] [DpDatadW,PosWR,DpCtrldW] [DpDatadW,PosWR,DpCtrlIsyncdR] DpCtrldW DpCtrlIsyncdR
  ***safe***
  
  ***reject***
  

A `BC` relax macro unfolds before raw edge parsing
  $ diy7 -arch PPC -relax BCDpDatadW -unfold-only
  ***relax***
  [DpDatadW,Rfe]
  ***safe***
  
  ***reject***
  

An `AC` relax macro unfolds before raw edge parsing
  $ diy7 -arch AArch64 -relax ACDMB.SYdRW -unfold-only
  ***relax***
  [Rfe,DMB.SYdRW]
  ***safe***
  
  ***reject***
  

An `ABC` relax macro unfolds before raw edge parsing
  $ diy7 -arch AArch64 -relax ABCDMB.SYdRW -unfold-only
  ***relax***
  [Rfe,DMB.SYdRW,Rfe]
  ***safe***
  
  ***reject***
  

`allRW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -relax allRW -unfold-only
  ***relax***
  PodRW ISBdRW GCSB.DSYNCdRW DMB.NSHLDdRW DMB.NSHSTdRW DMB.NSHdRW DMB.ISHLDdRW DMB.ISHSTdRW DMB.ISHdRW DMB.OSHLDdRW DMB.OSHSTdRW DMB.OSHdRW DMB.LDdRW DMB.STdRW DMB.SYdRW DSB.NSHLDdRW DSB.NSHSTdRW DSB.NSHdRW DSB.ISHLDdRW DSB.ISHSTdRW DSB.ISHdRW DSB.OSHLDdRW DSB.OSHSTdRW DSB.OSHdRW DSB.LDdRW DSB.STdRW DSB.SYdRW DpAddrCseldW DpAddrdW DpDataCseldW DpDatadW DpCtrlCseldW DpCtrldW DpCtrlIsbCseldW DpCtrlIsbdW
  ***safe***
  
  ***reject***
  

`someRW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -relax someRW -unfold-only
  ***relax***
  PodRW ISBdRW DMB.LDdRW DMB.STdRW DMB.SYdRW DpDatadW
  ***safe***
  
  ***reject***
  

`allWR` expands through the named relax lookup table
  $ diy7 -arch AArch64 -relax allWR -unfold-only
  ***relax***
  PodWR ISBdWR GCSB.DSYNCdWR DMB.NSHLDdWR DMB.NSHSTdWR DMB.NSHdWR DMB.ISHLDdWR DMB.ISHSTdWR DMB.ISHdWR DMB.OSHLDdWR DMB.OSHSTdWR DMB.OSHdWR DMB.LDdWR DMB.STdWR DMB.SYdWR DSB.NSHLDdWR DSB.NSHSTdWR DSB.NSHdWR DSB.ISHLDdWR DSB.ISHSTdWR DSB.ISHdWR DSB.OSHLDdWR DSB.OSHSTdWR DSB.OSHdWR DSB.LDdWR DSB.STdWR DSB.SYdWR
  ***safe***
  
  ***reject***
  

`someWW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -relax someWW -unfold-only
  ***relax***
  PodWW ISBdWW DMB.LDdWW DMB.STdWW DMB.SYdWW
  ***safe***
  
  ***reject***
  

`diy7 -unfold-only` unfolds relaxations and drops invalid composites
  $ diy7 -arch AArch64 -relax '[Po,DpAddr?]' -unfold-only 2>&1
  ***relax***
  PosWW PosWR [PosWR,DpAddrsW] [PosWR,DpAddrsR] [PosWR,DpAddrdW] [PosWR,DpAddrdR] PosRW PosRR [PosRR,DpAddrsW] [PosRR,DpAddrsR] [PosRR,DpAddrdW] [PosRR,DpAddrdR] PodWW PodWR [PodWR,DpAddrsW] [PodWR,DpAddrsR] [PodWR,DpAddrdW] [PodWR,DpAddrdR] PodRW PodRR [PodRR,DpAddrsW] [PodRR,DpAddrsR] [PodRR,DpAddrdW] [PodRR,DpAddrdR]
  ***safe***
  
  ***reject***
  
`diy7 -unfold-only` expands choice, optional, and grouped syntax
  $ diy7 -arch AArch64 -relax 'PodWR|Fre' -unfold-only 2>&1
  ***relax***
  Fre PodWR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'PodWR?' -unfold-only 2>&1
  ***relax***
  PodWR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax '[PodWR Fre]' -unfold-only 2>&1
  ***relax***
  [PodWR,Fre]
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'PodWR Fre' -unfold-only 2>&1
  ***relax***
  Fre PodWR
  ***safe***
  
  ***reject***
  

`diy7 -unfold-only` removes duplicate relaxes after unfolding
  $ diy7 -arch AArch64 -relax 'PodWR|PodWR' -unfold-only 2>&1
  ***relax***
  PodWR
  ***safe***
  
  ***reject***
  
`diy7 -unfold-only` also unfolds `-safe`
  $ diy7 -arch AArch64 -safe 'Fre|Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre Coe
  ***reject***
  
  $ diy7 -arch AArch64 -safe 'Fre?' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre
  ***reject***
  
  $ diy7 -arch AArch64 -safe '[PodWR Fre]' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  [PodWR,Fre]
  ***reject***
  
  $ diy7 -arch AArch64 -safe 'Fre Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre Coe
  ***reject***
  

`diy7 -unfold-only` removes duplicate safe edges after unfolding
  $ diy7 -arch AArch64 -safe 'Fre|Fre' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre
  ***reject***
  
