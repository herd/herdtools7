aarch64.cat
  $ mcat2config7 -set-libdir ./libdir -let ca libdir/aarch64.cat
  Fre
  Fri
  Coe
  Coi
  $ mcat2config7 -set-libdir ./libdir -let Exp-haz-ob libdir/aarch64.cat
  [PosRR, Fre]
  $ mcat2config7 -set-libdir ./libdir -let haz-ob libdir/aarch64.cat
  [PosRR, Fre]
  $ mcat2config7 -set-libdir ./libdir -let Exp-obs libdir/aarch64.cat
  Rfe
  Fre
  Coe
aarch64hwreqs.cat
  $ mcat2config7 -set-libdir ./libdir -let DSB-ob libdir/aarch64.cat
  DSB.SYd**
  DSB.SYs**
  DSB.LDdR*
  DSB.LDsR*
  DSB.STdW*
  DSB.STsW*
  $ mcat2config7 -set-libdir ./libdir -let IFB-ob libdir/aarch64.cat
  [DpCtrld*, ISB]
  [DpCtrls*, ISB]
  [DpAddrd*, ISBd**]
  [DpAddrs*, ISBd**]
  [DpAddrd*, ISBs**]
  [DpAddrs*, ISBs**]
  [DSB.SYd**, ISB]
  [DSB.SYs**, ISB]
  [DSB.LDdR*, ISB]
  [DSB.LDsR*, ISB]
  [DSB.STdW*, ISB]
  [DSB.STsW*, ISB]
  $ mcat2config7 -set-libdir ./libdir -let dob libdir/aarch64.cat
  DpAddrd*
  DpAddrs*
  DpDatadW
  DpDatasW
  DpCtrldW
  DpCtrlsW
  [DpAddrd*, Pod*W]
  [DpAddrs*, Pod*W]
  [DpAddrd*, Pos*W]
  [DpAddrs*, Pos*W]
  [DpAddrdW, PosWR]
  [DpAddrsW, PosWR]
  [DpDatadW, PosWR]
  [DpDatasW, PosWR]
  $ mcat2config7 -set-libdir ./libdir -let pob libdir/aarch64.cat
  DpAddrdW
  DpAddrsW
  DpDatadW
  DpDatasW
  DpCtrldW
  DpCtrlsW
  [DpAddrd*, Pod*W]
  [DpAddrs*, Pod*W]
  [DpAddrd*, Pos*W]
  [DpAddrs*, Pos*W]
  $ mcat2config7 -set-libdir ./libdir -let aob libdir/aarch64.cat
  LxSx
  Amo.Swp
  Amo.Cas
  [LxSx, PosWRPA]
  [Amo.Swp, PosWRPA]
  [Amo.Cas, PosWRPA]
  [LxSx, PosWRPQ]
  [Amo.Swp, PosWRPQ]
  [Amo.Cas, PosWRPQ]
  $ mcat2config7 -set-libdir ./libdir -let bob libdir/aarch64.cat
  DMB.SYd**
  DMB.SYs**
  DMB.LDdR*
  DMB.LDsR*
  DMB.STdWW
  DMB.STsWW
  [Amo.SwpAL, Pod**LP]
  [Amo.CasAL, Pod**LP]
  [Amo.SwpAL, Pos**LP]
  [Amo.CasAL, Pos**LP]
  Pod**LA
  Pos**LA
  Pod**AP
  Pos**AP
  Pod**QP
  Pos**QP
  Pod**PL
  Pos**PL
aarch64deps.cat
  $ mcat2config7 -set-libdir ./libdir -let lwfs libdir/aarch64.cat
  Pos*W
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 -set-libdir ./libdir -let pick-lob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let hw-reqs libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 -set-libdir ./libdir -let ob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let lob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let local-hw-reqs libdir/aarch64.cat
