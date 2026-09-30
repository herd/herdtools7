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
  [Fre|Coe]
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
  [[DSB.LD*R*|DSB.ST*W*|DSB.SY***],ISB]
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
  [LxSx|Amo.Safe]
  [[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let bob libdir/aarch64.cat
  DMB.SY***
  DMB.LD*R*
  DMB.ST*WW
  [Po?,A,Amo.Safe,L,Po]
  [[LxSx|Amo.Safe]?,L,Po,A,[LxSx|Amo.Safe]?]
  [[A|Q],[LxSx|Amo.Safe]?,Po]
  [Po,[LxSx|Amo.Safe]?,L]
aarch64deps.cat
  $ mcat2config7 --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  Pos*W
Pos*W pruning
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  [DSB.SY**R|DSB.SYd*W]
  [DSB.LDdRW|DSB.LD*RR]
  [DSB.STdWW|DSB.ST*WR]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [DpCtrl,ISB]
  [DpCtrlCsel,ISB]
  [DpAddr,ISB***]
  [DpAddrCsel,ISB***]
  [[DSB.LD*R*|DSB.ST*W*|DSB.SY***],ISB]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let dob libdir/aarch64.cat
  [DpAddrdW|DpAddr*R]
  DpDatadW
  DpCtrldW
  [DpAddr,Po**W]
  [DpAddr*W,PosWR]
  [DpData*W,PosWR]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let pob libdir/aarch64.cat
  DpAddrCseldW
  [DpDataCseldW|DpDataCsel*R]
  DpCtrlCseldW
  [DpAddrCsel,Po**W]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let bob libdir/aarch64.cat
  [DMB.SY**R|DMB.SYd*W]
  [DMB.LDdRW|DMB.LD*RR]
  DMB.STdWW
  [A,Amo.Safe,L,Po]
  [Po,A,Amo.Safe,L,Pod**]
  [Pod**,A,Amo.Safe,L,Pos**]
  [[LxSx|Amo.Safe]?,L,Po,A,[LxSx|Amo.Safe]?]
  [[A|Q],[LxSx|Amo.Safe]?,Po]
  [Po,[LxSx|Amo.Safe]?,L]
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let pick-lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let hw-reqs libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let ob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let local-hw-reqs libdir/aarch64.cat
