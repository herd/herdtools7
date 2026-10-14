aarch64.cat
  $ mcat2config7 --set-libdir ./libdir --let ca libdir/aarch64.cat
  Fr
  Co
  $ mcat2config7 --set-libdir ./libdir --let lrs libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,PosWR,[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let Exp-haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  [PosRR,[LxSx|Amo.Safe],Coe]
  [[LxSx|Amo.Safe],PosWR,Fre]
  [[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe],Coe]
  $ mcat2config7 --set-libdir ./libdir --let haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  [PosRR,[LxSx|Amo.Safe],Coe]
  [[LxSx|Amo.Safe],PosWR,Fre]
  [[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe],Coe]
  $ mcat2config7 --set-libdir ./libdir --let Exp-obs libdir/aarch64.cat
  Rfe
  [Fre|Coe]
aarch64hwreqs.cat
  $ mcat2config7 --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,DSB.SY***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe],DSB.LD*W*,[LxSx|Amo.Safe]?]
  [DSB.LD*R*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DSB.ST*W*,[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,DpCtrl,ISB,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DpCtrlCsel,ISB,[LxSx|Amo.Safe]?]
  [DpAddr,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DpAddrCsel,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe],[DSB.LD*W*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?]
  [[DSB.LD*R*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let dob libdir/aarch64.cat
  DpAddr
  DpData*W
  [DpCtrl*R,[LxSx|Amo.Safe]]
  DpCtrl*W
  [DpAddr,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]]
  [DpAddr,[LxSx|Amo.Safe]?,Po**W]
  [DpAddr*R,[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe]?]
  [DpAddr*W,PosWR,[LxSx|Amo.Safe]?]
  [DpData*W,PosWR,[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let pob libdir/aarch64.cat
  [DpAddrCsel*R,[LxSx|Amo.Safe]]
  DpAddrCsel*W
  DpDataCsel
  [DpCtrlCsel*R,[LxSx|Amo.Safe]]
  DpCtrlCsel*W
  [DpAddrCsel,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]]
  [DpAddrCsel,[LxSx|Amo.Safe]?,Po**W]
  $ mcat2config7 --set-libdir ./libdir --let aob libdir/aarch64.cat
  [LxSx|Amo.Safe]
  [[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let bob libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,DMB.SY***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe],DMB.LD*W*,[LxSx|Amo.Safe]?]
  [DMB.LD*R*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DMB.ST*WR,[LxSx|Amo.Safe]]
  [[LxSx|Amo.Safe]?,DMB.ST*WW]
  [[LxSx|Amo.Safe]?,Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?]
  [A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,L,Po*WR,A,[LxSx|Amo.Safe]?]
  [[A|Q],[LxSx|Amo.Safe],Po*W*,[LxSx|Amo.Safe]?]
  [[A|Q],Po*R*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe],L]
  [[LxSx|Amo.Safe]?,Po**W,L]
aarch64deps.cat
  $ mcat2config7 --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,Pos*R,[LxSx|Amo.Safe]]
  [[LxSx|Amo.Safe]?,Pos*W]
Communication pruning
  $ mcat2config7 -prune communication --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  [[Hat|ExpObs],[LxSx|Amo.Safe],Pos*R,[LxSx|Amo.Safe]]
  [[Hat|ExpObs],[LxSx|Amo.Safe],Pos*W]
  [ExpObs,Pos*R,[LxSx|Amo.Safe]]
  [ExpObs,Pos*W]
  $ mcat2config7 -prune communication --set-libdir ./libdir --let aob libdir/aarch64.cat
  [[Hat|ExpObs],[LxSx|Amo.Safe],@after([ExpObs|Hat])]
  [[Hat|ExpObs],[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  $ mcat2config7 -prune communication --set-libdir ./libdir --let bob libdir/aarch64.cat
  [[Hat|ExpObs],[LxSx|Amo.Safe],DMB.SY***,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [ExpObs,DMB.SY***,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [DMB.LD*R*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [Hat?,[LxSx|Amo.Safe],DMB.LD*W*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [DMB.ST*WR,[LxSx|Amo.Safe]]
  DMB.ST*WW
  [Hat?,[LxSx|Amo.Safe],DMB.ST*WR,[LxSx|Amo.Safe]]
  [Hat?,[LxSx|Amo.Safe],DMB.ST*WW]
  [[Hat|ExpObs],[LxSx|Amo.Safe],Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [ExpObs,Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [L,Po*WR,A,[LxSx|Amo.Safe]?]
  [Hat?,[LxSx|Amo.Safe],L,Po*WR,A,[LxSx|Amo.Safe]?]
  [[A|Q],Po*R*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [Hat?,[A|Q],[LxSx|Amo.Safe],Po*W*,[LxSx|Amo.Safe]?,@after([ExpObs|Hat])]
  [[Hat|ExpObs],[LxSx|Amo.Safe],Po**R,[LxSx|Amo.Safe],L]
  [[Hat|ExpObs],[LxSx|Amo.Safe],Po**W,L]
  [ExpObs,Po**R,[LxSx|Amo.Safe],L]
  [ExpObs,Po**W,L]
Communication-state pruning
  $ mcat2config7 -prune communication_state --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Pos*R,[LxSx|Amo.Safe]]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Pos*W]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Pos*R,[LxSx|Amo.Safe]]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Pos*W]
  $ mcat2config7 -prune communication_state -prune pos_w --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Pos*R,[LxSx|Amo.Safe]]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Pos*W]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Pos*R,[LxSx|Amo.Safe]]
  [@state(StartExpM),[LxSx|Amo.Safe],Pos*W]
  $ mcat2config7 -prune communication_state --set-libdir ./libdir --let Exp-obs libdir/aarch64.cat
  [@state(EndExpM)?,Rfe,@state(StartExpM)?]
  [@state(EndExpM)?,[Fre|Coe],@state(StartExpM)?]
  $ mcat2config7 -prune communication_state --set-libdir ./libdir --let Exp-haz-ob libdir/aarch64.cat
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],PosWR,Fre]
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe],Coe]
  [@state(StartExpM),PosRR,Fre]
  [@state(StartExpM),PosRR,[LxSx|Amo.Safe],Coe]
  $ mcat2config7 -prune communication_state --set-libdir ./libdir --let aob libdir/aarch64.cat
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],@state(EndExpM)]
  [@state(StartExpM),[LxSx|Amo.Safe],@state(EndExpM)]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  [@state(StartExpM),[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  $ mcat2config7 -prune communication_state --set-libdir ./libdir --let bob libdir/aarch64.cat
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],DMB.SY***,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(StartExpM),[LxSx|Amo.Safe]?,DMB.SY***,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [DMB.LD*R*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],DMB.LD*W*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [DMB.ST*WR,[LxSx|Amo.Safe]]
  DMB.ST*WW
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],DMB.ST*WR,[LxSx|Amo.Safe]]
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],DMB.ST*WW]
  [A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [L,Po*WR,A,[LxSx|Amo.Safe]?]
  [@state(EndExpM)?,Hat?,[LxSx|Amo.Safe],L,Po*WR,A,[LxSx|Amo.Safe]?]
  [[A|Q],Po*R*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(EndExpM)?,Hat?,[A|Q],[LxSx|Amo.Safe],Po*W*,[LxSx|Amo.Safe]?,@state(EndExpM)]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Po**R,[LxSx|Amo.Safe],L]
  [@state(EndExpM)?,Hat,[LxSx|Amo.Safe],Po**W,L]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe],L]
  [@state(StartExpM),[LxSx|Amo.Safe]?,Po**W,L]
Pos*W pruning
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  [[LxSx|Amo.Safe],DSB.SY***,[LxSx|Amo.Safe]?]
  [DSB.SY***,[LxSx|Amo.Safe]]
  [DSB.SY**R|DSB.SYd*W]
  [DSB.LDdRW|DSB.LD*RR]
  [[LxSx|Amo.Safe],DSB.LD*W*,[LxSx|Amo.Safe]?]
  [DSB.LD*R*,[LxSx|Amo.Safe]]
  [DSB.STdWW|DSB.ST*WR]
  [[LxSx|Amo.Safe],DSB.ST*W*,[LxSx|Amo.Safe]?]
  [DSB.ST*W*,[LxSx|Amo.Safe]]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [[LxSx|Amo.Safe]?,DpCtrl,ISB,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DpCtrlCsel,ISB,[LxSx|Amo.Safe]?]
  [DpAddr,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,DpAddrCsel,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe],[DSB.LD*W*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?]
  [[DSB.LD*R*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let dob libdir/aarch64.cat
  [DpAddrdW|DpAddr*R]
  DpDatadW
  DpCtrldW
  [DpCtrl*R,[LxSx|Amo.Safe]]
  [DpAddr,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]]
  [DpAddr,[LxSx|Amo.Safe]?,Po**W]
  [DpAddr*R,[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe]?]
  [DpAddr*W,PosWR,[LxSx|Amo.Safe]?]
  [DpData*W,PosWR,[LxSx|Amo.Safe]?]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let pob libdir/aarch64.cat
  DpAddrCseldW
  [DpAddrCsel*R,[LxSx|Amo.Safe]]
  [DpDataCseldW|DpDataCsel*R]
  DpCtrlCseldW
  [DpCtrlCsel*R,[LxSx|Amo.Safe]]
  [DpAddrCsel,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]]
  [DpAddrCsel,[LxSx|Amo.Safe]?,Po**W]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let bob libdir/aarch64.cat
  [[LxSx|Amo.Safe],DMB.SY***,[LxSx|Amo.Safe]?]
  [DMB.SY***,[LxSx|Amo.Safe]]
  [DMB.SY**R|DMB.SYd*W]
  [DMB.LDdRW|DMB.LD*RR]
  [[LxSx|Amo.Safe],DMB.LD*W*,[LxSx|Amo.Safe]?]
  [DMB.LD*R*,[LxSx|Amo.Safe]]
  DMB.STdWW
  [[LxSx|Amo.Safe]?,DMB.ST*WR,[LxSx|Amo.Safe]]
  [[LxSx|Amo.Safe],DMB.ST*WW]
  [[LxSx|Amo.Safe]?,Po**R,A,Amo.Safe,L,PodW*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,Pod*R,A,Amo.Safe,L,PosW*,[LxSx|Amo.Safe]?]
  [A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,L,Po*WR,A,[LxSx|Amo.Safe]?]
  [[A|Q],[LxSx|Amo.Safe],Po*W*,[LxSx|Amo.Safe]?]
  [[A|Q],Po*R*,[LxSx|Amo.Safe]?]
  [[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe],L]
  [[LxSx|Amo.Safe]?,Po**W,L]
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let pick-lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let hw-reqs libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let ob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let local-hw-reqs libdir/aarch64.cat
