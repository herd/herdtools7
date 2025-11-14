  $ mcat2config7 -set-libdir ./libdir -conf true -let ca -let Exp-haz-ob -let haz-ob -let Exp-obs -let DSB-ob -let IFB-ob -let dob -let pob -let aob -let bob -let lwfs libdir/aarch64.cat
  -arch AArch64
  -nprocs 2
  -size 6
  -name cat2config-conf
  
  ### ca
  -safe Fre Fri
  -safe Coe Coi
  
  ### Exp-haz-ob
  -safe [PosRR, Fre]
  
  ### haz-ob
  -safe [PosRR, Fre]
  
  ### Exp-obs
  -safe Rfe
  -safe Fre
  -safe Coe
  
  ### DSB-ob
  -safe DSB.SYd** DSB.SYs**
  -safe DSB.LDdR* DSB.LDsR*
  -safe DSB.STdW* DSB.STsW*
  
  ### IFB-ob
  -safe [DpCtrld*, ISB] [DpCtrls*, ISB]
  -safe [DpCtrlCseld*, ISB] [DpCtrlCsels*, ISB]
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
  -safe [DpAddrCseld*, ISBd**] [DpAddrCsels*, ISBd**] [DpAddrCseld*, ISBs**] [DpAddrCsels*, ISBs**]
  -safe [DSB.SYd**, ISB] [DSB.SYs**, ISB]
  -safe [DSB.LDdR*, ISB] [DSB.LDsR*, ISB]
  -safe [DSB.STdW*, ISB] [DSB.STsW*, ISB]
  
  ### dob
  -safe DpAddrd* DpAddrs*
  -safe DpDatadW DpDatasW
  -safe DpCtrldW DpCtrlsW
  -safe [DpAddrd*, Pod*W] [DpAddrs*, Pod*W] [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  -safe [DpAddrdW, PosWR] [DpAddrsW, PosWR]
  -safe [DpDatadW, PosWR] [DpDatasW, PosWR]
  
  ### pob
  -safe DpAddrCseldW DpAddrCselsW
  -safe DpDataCseld* DpDataCsels*
  -safe DpCtrlCseldW DpCtrlCselsW
  -safe [DpAddrCseld*, Pod*W] [DpAddrCsels*, Pod*W] [DpAddrCseld*, Pos*W] [DpAddrCsels*, Pos*W]
  
  ### aob
  -safe LxSx Amo.Swp Amo.Cas
  -safe [LxSx, PosWRPA] [Amo.Swp, PosWRPA] [Amo.Cas, PosWRPA]
  -safe [LxSx, PosWRPQ] [Amo.Swp, PosWRPQ] [Amo.Cas, PosWRPQ]
  
  ### bob
  -safe DMB.SYd** DMB.SYs**
  -safe DMB.LDdR* DMB.LDsR*
  -safe DMB.STdWW DMB.STsWW
  -safe [Amo.SwpAL, Pod**LP] [Amo.CasAL, Pod**LP] [Amo.SwpAL, Pos**LP] [Amo.CasAL, Pos**LP]
  -safe Pod**LA Pos**LA
  -safe [Pod**LA, Amo.SwpAP] [Pos**LA, Amo.SwpAP] [Pod**LA, Amo.CasAP] [Pos**LA, Amo.CasAP]
  -safe [Amo.SwpPL, Pod**LA] [Amo.CasPL, Pod**LA] [Amo.SwpPL, Pos**LA] [Amo.CasPL, Pos**LA]
  -safe [Amo.SwpPL, Pod**LA, Amo.SwpAP] [Amo.CasPL, Pod**LA, Amo.SwpAP] [Amo.SwpPL, Pos**LA, Amo.SwpAP] [Amo.CasPL, Pos**LA, Amo.SwpAP] [Amo.SwpPL, Pod**LA, Amo.CasAP] [Amo.CasPL, Pod**LA, Amo.CasAP] [Amo.SwpPL, Pos**LA, Amo.CasAP] [Amo.CasPL, Pos**LA, Amo.CasAP]
  -safe Pod**AP Pos**AP
  -safe [Amo.SwpAP, Pod**] [Amo.CasAP, Pod**] [Amo.SwpAP, Pos**] [Amo.CasAP, Pos**]
  -safe Pod**QP Pos**QP
  -safe [Amo.SwpQP, Pod**] [Amo.CasQP, Pod**] [Amo.SwpQP, Pos**] [Amo.CasQP, Pos**]
  -safe Pod**PL Pos**PL
  -safe [Pod**, Amo.SwpPL] [Pos**, Amo.SwpPL] [Pod**, Amo.CasPL] [Pos**, Amo.CasPL]
  
  ### lwfs
  -safe Pos*W
