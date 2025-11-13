aarch64.cat
  $ mcat2config7 -set-libdir ./libdir -let ca libdir/aarch64.cat
  Fre
  Fri
  Coe
  Coi
  $ mcat2config7 -set-libdir ./libdir -let Exp-haz-ob libdir/aarch64.cat
  PosRR Fre
  $ mcat2config7 -set-libdir ./libdir -let haz-ob libdir/aarch64.cat
  PosRR Fre
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
  [DpCtrld*, ISB]
  [DpCtrls*, ISB]
  DpAddrd* ISBd**
  DpAddrs* ISBd**
  DpAddrd* ISBs**
  DpAddrs* ISBs**
  DpAddrd* ISBd**
  DpAddrs* ISBd**
  DpAddrd* ISBs**
  DpAddrs* ISBs**
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
  DpAddrd* Pod*W
  DpAddrs* Pod*W
  DpAddrd* Pos*W
  DpAddrs* Pos*W
  DpAddrdW PosWR
  DpAddrsW PosWR
  DpDatadW PosWR
  DpDatasW PosWR
  $ mcat2config7 -set-libdir ./libdir -let pob libdir/aarch64.cat
  DpAddrdW
  DpAddrsW
  DpDatadW
  DpDatasW
  DpCtrldW
  DpCtrlsW
  DpAddrd* Pod*W
  DpAddrs* Pod*W
  DpAddrd* Pos*W
  DpAddrs* Pos*W
  $ mcat2config7 -set-libdir ./libdir -let aob libdir/aarch64.cat
  LxSx
  Amo.LdAdd
  Amo.LdEor
  Amo.LdSet
  Amo.LdClr
  Amo.StAdd
  Amo.StEor
  Amo.StSet
  Amo.StClr
  Amo.Swp
  Amo.Cas
  LxSx PosWRPA
  Amo.LdAdd PosWRPA
  Amo.LdEor PosWRPA
  Amo.LdSet PosWRPA
  Amo.LdClr PosWRPA
  Amo.StAdd PosWRPA
  Amo.StEor PosWRPA
  Amo.StSet PosWRPA
  Amo.StClr PosWRPA
  Amo.Swp PosWRPA
  Amo.Cas PosWRPA
  $ mcat2config7 -set-libdir ./libdir -let bob libdir/aarch64.cat
  DMB.SYd**
  DMB.SYs**
  DMB.LDdR*
  DMB.LDsR*
  DMB.STdWW
  DMB.STsWW
  Pod**LA
  Pos**LA
  Pod**AP
  Pos**AP
  Pod**PL
  Pos**PL
aarch64deps.cat
  $ mcat2config7 -set-libdir ./libdir -let lwfs libdir/aarch64.cat
  Pos*W
Pure unions
$ mcat2config7 -set-libdir ./libdir -let pick-lob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let hw-reqs libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let obs libdir/aarch64.cat
Recursive unions
$ mcat2config7 -set-libdir ./libdir -let ob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let lob libdir/aarch64.cat
$ mcat2config7 -set-libdir ./libdir -let local-hw-reqs libdir/aarch64.cat
