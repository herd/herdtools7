aarch64.cat
  $ mcat2config7 --set-libdir ./libdir --let ca libdir/aarch64.cat
  Fr
  Co
  $ mcat2config7 --set-libdir ./libdir --let lrs libdir/aarch64.cat
  PosWR
  $ mcat2config7 --set-libdir ./libdir --let Exp-haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  $ mcat2config7 --set-libdir ./libdir --let haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  $ mcat2config7 --set-libdir ./libdir --let Exp-obs libdir/aarch64.cat
  Rfe
  Fre
  Coe
aarch64hwreqs.cat
  $ mcat2config7 --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  DSB.SY***
  DSB.LD*R*
  DSB.ST*W*
  $ mcat2config7 --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [DpCtrl,ISB]
  [DpCtrlCsel,ISB]
  [DpAddr,ISB***]
  [DpAddrCsel,ISB***]
  [DSB.SY***,ISB]
  [DSB.LD*R*,ISB]
  [DSB.ST*W*,ISB]
  $ mcat2config7 --set-libdir ./libdir --let dob libdir/aarch64.cat
  DpAddr
  DpData*W
  DpCtrl*W
  [DpAddr,Po**W]
  [DpAddr*W,PosWR]
  [DpData*W,PosWR]
  $ mcat2config7 --set-libdir ./libdir --let pob libdir/aarch64.cat
  DpAddrCsel*W
  DpDataCsel
  DpCtrlCsel*W
  [DpAddrCsel,Po**W]
  $ mcat2config7 --set-libdir ./libdir --let aob libdir/aarch64.cat
  LxSx
  Amo.Safe
  [LxSx,PosWR,A]
  [Amo.Safe,PosWR,A]
  [LxSx,PosWR,A,LxSx]
  [Amo.Safe,PosWR,A,LxSx]
  [LxSx,PosWR,A,Amo.Safe]
  [Amo.Safe,PosWR,A,Amo.Safe]
  [LxSx,PosWR,Q]
  [Amo.Safe,PosWR,Q]
  [LxSx,PosWR,Q,LxSx]
  [Amo.Safe,PosWR,Q,LxSx]
  [LxSx,PosWR,Q,Amo.Safe]
  [Amo.Safe,PosWR,Q,Amo.Safe]
  $ mcat2config7 --set-libdir ./libdir --let bob libdir/aarch64.cat
  DMB.SY***
  DMB.LD*R*
  DMB.ST*WW
  [A,Amo.Safe,L,Po]
  [L,Po,A]
  [L,Po,A,LxSx]
  [L,Po,A,Amo.Safe]
  [LxSx,L,Po,A]
  [Amo.Safe,L,Po,A]
  [LxSx,L,Po,A,LxSx]
  [Amo.Safe,L,Po,A,LxSx]
  [LxSx,L,Po,A,Amo.Safe]
  [Amo.Safe,L,Po,A,Amo.Safe]
  [A,Po]
  [A,LxSx,Po]
  [A,Amo.Safe,Po]
  [Q,Po]
  [Q,LxSx,Po]
  [Q,Amo.Safe,Po]
  [Po,L]
  [Po,LxSx,L]
  [Po,Amo.Safe,L]
aarch64deps.cat
  $ mcat2config7 --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  Pos*W
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let pick-lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let hw-reqs libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let ob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let local-hw-reqs libdir/aarch64.cat
