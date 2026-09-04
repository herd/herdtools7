A test for no metadata, `-metadata false`
  $ diyone7 -arch AArch64 -variant vmsa -metadata false PodWR Fre PodWR Fre
  AArch64 SB
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#2   | MOV W0,#6   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=5 /\ 1:X3=1)
A VMSA test for a negated exists check, `-neg true`
  $ diyone7 -arch AArch64 -variant vmsa -neg true -info "User-define=User-define" PodWR Fre PodWR Fre
  AArch64 SB
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   [x]=1;
   [y]=5;
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#2   | MOV W0,#6   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  ~exists (0:X3=5 /\ 1:X3=1)
A VMSA test for observing locations, `-cond observe`
  $ diyone7 -arch AArch64 -variant vmsa -info "User-define=User-define" -cond observe PodWR Fre PodWR Fre
  AArch64 SB
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   [x]=1;
   [y]=5;
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#2   | MOV W0,#6   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  locations [x; y; 0:X3; 1:X3;]
  forall (true)
A VMSA test for a forall check, `-cond unicond`
  $ diyone7 -arch AArch64 -variant vmsa -info "User-define=User-define" -cond unicond PodWR Fre PodWR Fre
  AArch64 SB
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  User-define=User-define
  "PodWR Fre PodWR Fre"
  {
   [x]=1;
   [y]=5;
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#2   | MOV W0,#6   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  forall (true /\ ([x]=2 /\ ([y]=6 /\ (0:X3=6 /\ (1:X3=2 \/ 1:X3=0) \/ 0:X3=0 /\ (1:X3=2 \/ 1:X3=0)))))
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
  DpAddrsW DpAddrsR DpAddrdW DpAddrdR DpDatasW DpDatadW DpCtrlIsbsW DpCtrlIsbsR DpCtrlIsbdW DpCtrlIsbdR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'DpAddr*W DpAddrd* DpData' -unfold-only 2>&1
  ***relax***
  DpAddrsW DpAddrdW DpAddrdR DpDatasW DpDatadW
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
  
