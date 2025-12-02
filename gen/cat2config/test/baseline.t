aarch64.cat
  $ mcat2config7 -set-libdir ./libdir -let ca libdir/aarch64.cat
  Fre
  Fri
  Coe
  Coi
  $ mcat2config7 -set-libdir ./libdir -let lrs libdir/aarch64.cat
  PosWR
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
  [DpCtrlCseld*, ISB]
  [DpCtrlCsels*, ISB]
  [DpAddrd*, ISBd**]
  [DpAddrs*, ISBd**]
  [DpAddrd*, ISBs**]
  [DpAddrs*, ISBs**]
  [DpAddrCseld*, ISBd**]
  [DpAddrCsels*, ISBd**]
  [DpAddrCseld*, ISBs**]
  [DpAddrCsels*, ISBs**]
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
  DpAddrCseldW
  DpAddrCselsW
  DpDataCseld*
  DpDataCsels*
  DpCtrlCseldW
  DpCtrlCselsW
  [DpAddrCseld*, Pod*W]
  [DpAddrCsels*, Pod*W]
  [DpAddrCseld*, Pos*W]
  [DpAddrCsels*, Pos*W]
  $ mcat2config7 -set-libdir ./libdir -let aob libdir/aarch64.cat
  LxSx
  Amo
  [LxSx, PosWRPA]
  [Amo, PosWRPA]
  [LxSx, PosWRPA, AmoAP]
  [Amo, PosWRPA, AmoAP]
  [LxSx, PosWRPQ]
  [Amo, PosWRPQ]
  [LxSx, PosWRPQ, AmoQP]
  [Amo, PosWRPQ, AmoQP]
  $ mcat2config7 -set-libdir ./libdir -let bob libdir/aarch64.cat
  DMB.SYd**
  DMB.SYs**
  DMB.LDdR*
  DMB.LDsR*
  DMB.STdWW
  DMB.STsWW
  [AmoAL, Pod**LP]
  [AmoAL, Pos**LP]
  Pod**LA
  Pos**LA
  [Pod**LA, AmoAP]
  [Pos**LA, AmoAP]
  [AmoPL, Pod**LA]
  [AmoPL, Pos**LA]
  [AmoPL, Pod**LA, AmoAP]
  [AmoPL, Pos**LA, AmoAP]
  Pod**AP
  Pos**AP
  [AmoAP, Pod**]
  [AmoAP, Pos**]
  Pod**QP
  Pos**QP
  [AmoQP, Pod**]
  [AmoQP, Pos**]
  Pod**PL
  Pos**PL
  [Pod**, AmoPL]
  [Pos**, AmoPL]
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
