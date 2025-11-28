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
  [Pod**LA, Amo.SwpAP]
  [Pos**LA, Amo.SwpAP]
  [Pod**LA, Amo.CasAP]
  [Pos**LA, Amo.CasAP]
  [Amo.SwpPL, Pod**LA]
  [Amo.CasPL, Pod**LA]
  [Amo.SwpPL, Pos**LA]
  [Amo.CasPL, Pos**LA]
  [Amo.SwpPL, Pod**LA, Amo.SwpAP]
  [Amo.CasPL, Pod**LA, Amo.SwpAP]
  [Amo.SwpPL, Pos**LA, Amo.SwpAP]
  [Amo.CasPL, Pos**LA, Amo.SwpAP]
  [Amo.SwpPL, Pod**LA, Amo.CasAP]
  [Amo.CasPL, Pod**LA, Amo.CasAP]
  [Amo.SwpPL, Pos**LA, Amo.CasAP]
  [Amo.CasPL, Pos**LA, Amo.CasAP]
  Pod**AP
  Pos**AP
  [Amo.SwpAP, Pod**]
  [Amo.CasAP, Pod**]
  [Amo.SwpAP, Pos**]
  [Amo.CasAP, Pos**]
  Pod**QP
  Pos**QP
  [Amo.SwpQP, Pod**]
  [Amo.CasQP, Pod**]
  [Amo.SwpQP, Pos**]
  [Amo.CasQP, Pos**]
  Pod**PL
  Pos**PL
  [Pod**, Amo.SwpPL]
  [Pos**, Amo.SwpPL]
  [Pod**, Amo.CasPL]
  [Pos**, Amo.CasPL]
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
