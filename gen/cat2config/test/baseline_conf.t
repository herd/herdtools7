  $ mcat2config7 -set-libdir ./libdir -conf true -let ca -let Exp-haz-ob -let haz-ob -let Exp-obs -let DSB-ob -let IFB-ob -let dob -let pob -let aob -let bob -let lwfs -let pick-lob -let hw-reqs -let obs libdir/aarch64.cat
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
  -safe [DpCtrld*, ISB] [DpCtrls*, ISB]
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
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
  -safe DpAddrdW DpAddrsW
  -safe DpDatadW DpDatasW
  -safe DpCtrldW DpCtrlsW
  -safe [DpAddrd*, Pod*W] [DpAddrs*, Pod*W] [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  
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
  -safe Pod**AP Pos**AP
  -safe Pod**QP Pos**QP
  -safe Pod**PL Pos**PL
  
  ### lwfs
  -safe Pos*W
  
  ### pick-lob
  -safe [PosWR, DSB.SYdRW] [PosWR, DSB.SYsRW]
  -safe [PosWR, DSB.LDdRW] [PosWR, DSB.LDsRW]
  -safe [PosWR, DpCtrldW, ISB] [PosWR, DpCtrlsW, ISB]
  -safe [PosWR, DpCtrldW, ISB] [PosWR, DpCtrlsW, ISB]
  -safe [PosWR, DpAddrd*, ISBd*W] [PosWR, DpAddrs*, ISBd*W] [PosWR, DpAddrd*, ISBs*W] [PosWR, DpAddrs*, ISBs*W]
  -safe [PosWR, DpAddrd*, ISBd*W] [PosWR, DpAddrs*, ISBd*W] [PosWR, DpAddrd*, ISBs*W] [PosWR, DpAddrs*, ISBs*W]
  -safe [PosWR, DSB.SYdRW, ISB] [PosWR, DSB.SYsRW, ISB]
  -safe [PosWR, DSB.LDdRW, ISB] [PosWR, DSB.LDsRW, ISB]
  -safe [PosWR, PosRW]
  -safe [PosWR, DpAddrdW] [PosWR, DpAddrsW]
  -safe [PosWR, DpDatadW] [PosWR, DpDatasW]
  -safe [PosWR, DpCtrldW] [PosWR, DpCtrlsW]
  -safe [PosWR, DpAddrd*, Pod*W] [PosWR, DpAddrs*, Pod*W] [PosWR, DpAddrd*, Pos*W] [PosWR, DpAddrs*, Pos*W]
  -safe [PosWR, DpAddrdW] [PosWR, DpAddrsW]
  -safe [PosWR, DpDatadW] [PosWR, DpDatasW]
  -safe [PosWR, DpCtrldW] [PosWR, DpCtrlsW]
  -safe [PosWR, DpAddrd*, Pod*W] [PosWR, DpAddrs*, Pod*W] [PosWR, DpAddrd*, Pos*W] [PosWR, DpAddrs*, Pos*W]
  -safe [PosWR, LxSx] [PosWR, Amo.Swp] [PosWR, Amo.Cas]
  -safe [PosWR, DMB.SYdRW] [PosWR, DMB.SYsRW]
  -safe [PosWR, DMB.LDdRW] [PosWR, DMB.LDsRW]
  -safe [PosWRPL, PodRWLA] [PosWRPL, PosRWLA]
  -safe [PosWRPA, PodRWAP] [PosWRPA, PosRWAP]
  -safe [PosWRPQ, PodRWQP] [PosWRPQ, PosRWQP]
  -safe [PosWR, PodRWPL] [PosWR, PosRWPL]
  -safe [DpAddrd*, DSB.SYd*W] [DpAddrs*, DSB.SYd*W] [DpAddrd*, DSB.SYs*W] [DpAddrs*, DSB.SYs*W]
  -safe [DpAddrdR, DSB.LDdRW] [DpAddrsR, DSB.LDdRW] [DpAddrdR, DSB.LDsRW] [DpAddrsR, DSB.LDsRW]
  -safe [DpAddrdW, DSB.STdWW] [DpAddrsW, DSB.STdWW] [DpAddrdW, DSB.STsWW] [DpAddrsW, DSB.STsWW]
  -safe [DpAddrdR, DpCtrldW, ISB] [DpAddrsR, DpCtrldW, ISB] [DpAddrdR, DpCtrlsW, ISB] [DpAddrsR, DpCtrlsW, ISB]
  -safe [DpAddrdR, DpCtrldW, ISB] [DpAddrsR, DpCtrldW, ISB] [DpAddrdR, DpCtrlsW, ISB] [DpAddrsR, DpCtrlsW, ISB]
  -safe [DpAddrdR, DpAddrd*, ISBd*W] [DpAddrsR, DpAddrd*, ISBd*W] [DpAddrdR, DpAddrs*, ISBd*W] [DpAddrsR, DpAddrs*, ISBd*W] [DpAddrdR, DpAddrd*, ISBs*W] [DpAddrsR, DpAddrd*, ISBs*W] [DpAddrdR, DpAddrs*, ISBs*W] [DpAddrsR, DpAddrs*, ISBs*W]
  -safe [DpAddrdR, DpAddrd*, ISBd*W] [DpAddrsR, DpAddrd*, ISBd*W] [DpAddrdR, DpAddrs*, ISBd*W] [DpAddrsR, DpAddrs*, ISBd*W] [DpAddrdR, DpAddrd*, ISBs*W] [DpAddrsR, DpAddrd*, ISBs*W] [DpAddrdR, DpAddrs*, ISBs*W] [DpAddrsR, DpAddrs*, ISBs*W]
  -safe [DpAddrd*, DSB.SYd*W, ISB] [DpAddrs*, DSB.SYd*W, ISB] [DpAddrd*, DSB.SYs*W, ISB] [DpAddrs*, DSB.SYs*W, ISB]
  -safe [DpAddrdR, DSB.LDdRW, ISB] [DpAddrsR, DSB.LDdRW, ISB] [DpAddrdR, DSB.LDsRW, ISB] [DpAddrsR, DSB.LDsRW, ISB]
  -safe [DpAddrdW, DSB.STdWW, ISB] [DpAddrsW, DSB.STdWW, ISB] [DpAddrdW, DSB.STsWW, ISB] [DpAddrsW, DSB.STsWW, ISB]
  -safe [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  -safe [DpAddrd*, LxSx] [DpAddrs*, LxSx] [DpAddrd*, Amo.Swp] [DpAddrs*, Amo.Swp] [DpAddrd*, Amo.Cas] [DpAddrs*, Amo.Cas]
  -safe [DpAddrd*, DMB.SYd*W] [DpAddrs*, DMB.SYd*W] [DpAddrd*, DMB.SYs*W] [DpAddrs*, DMB.SYs*W]
  -safe [DpAddrdR, DMB.LDdRW] [DpAddrsR, DMB.LDdRW] [DpAddrdR, DMB.LDsRW] [DpAddrsR, DMB.LDsRW]
  -safe [DpAddrdW, DMB.STdWW] [DpAddrsW, DMB.STdWW] [DpAddrdW, DMB.STsWW] [DpAddrsW, DMB.STsWW]
  -safe [DpAddrd*PA, Amo.SwpAL, Pod*WLP] [DpAddrs*PA, Amo.SwpAL, Pod*WLP] [DpAddrd*PA, Amo.CasAL, Pod*WLP] [DpAddrs*PA, Amo.CasAL, Pod*WLP] [DpAddrd*PA, Amo.SwpAL, Pos*WLP] [DpAddrs*PA, Amo.SwpAL, Pos*WLP] [DpAddrd*PA, Amo.CasAL, Pos*WLP] [DpAddrs*PA, Amo.CasAL, Pos*WLP]
  -safe [DpAddrd*PL, Pod*WLA] [DpAddrs*PL, Pod*WLA] [DpAddrd*PL, Pos*WLA] [DpAddrs*PL, Pos*WLA]
  -safe [DpAddrd*PA, Pod*WAP] [DpAddrs*PA, Pod*WAP] [DpAddrd*PA, Pos*WAP] [DpAddrs*PA, Pos*WAP]
  -safe [DpAddrd*PQ, Pod*WQP] [DpAddrs*PQ, Pod*WQP] [DpAddrd*PQ, Pos*WQP] [DpAddrs*PQ, Pos*WQP]
  -safe [DpAddrd*, Pod*WPL] [DpAddrs*, Pod*WPL] [DpAddrd*, Pos*WPL] [DpAddrs*, Pos*WPL]
  -safe [DpDatadW, DSB.SYd*W] [DpDatasW, DSB.SYd*W] [DpDatadW, DSB.SYs*W] [DpDatasW, DSB.SYs*W]
  -safe [DpDatadW, DSB.STdWW] [DpDatasW, DSB.STdWW] [DpDatadW, DSB.STsWW] [DpDatasW, DSB.STsWW]
  -safe [DpDatadW, DSB.SYd*W, ISB] [DpDatasW, DSB.SYd*W, ISB] [DpDatadW, DSB.SYs*W, ISB] [DpDatasW, DSB.SYs*W, ISB]
  -safe [DpDatadW, DSB.STdWW, ISB] [DpDatasW, DSB.STdWW, ISB] [DpDatadW, DSB.STsWW, ISB] [DpDatasW, DSB.STsWW, ISB]
  -safe [DpDatadW, Pos*W] [DpDatasW, Pos*W]
  -safe [DpDatadW, LxSx] [DpDatasW, LxSx] [DpDatadW, Amo.Swp] [DpDatasW, Amo.Swp] [DpDatadW, Amo.Cas] [DpDatasW, Amo.Cas]
  -safe [DpDatadW, DMB.SYd*W] [DpDatasW, DMB.SYd*W] [DpDatadW, DMB.SYs*W] [DpDatasW, DMB.SYs*W]
  -safe [DpDatadW, DMB.STdWW] [DpDatasW, DMB.STdWW] [DpDatadW, DMB.STsWW] [DpDatasW, DMB.STsWW]
  -safe [DpDatadWPA, Amo.SwpAL, Pod*WLP] [DpDatasWPA, Amo.SwpAL, Pod*WLP] [DpDatadWPA, Amo.CasAL, Pod*WLP] [DpDatasWPA, Amo.CasAL, Pod*WLP] [DpDatadWPA, Amo.SwpAL, Pos*WLP] [DpDatasWPA, Amo.SwpAL, Pos*WLP] [DpDatadWPA, Amo.CasAL, Pos*WLP] [DpDatasWPA, Amo.CasAL, Pos*WLP]
  -safe [DpDatadWPL, Pod*WLA] [DpDatasWPL, Pod*WLA] [DpDatadWPL, Pos*WLA] [DpDatasWPL, Pos*WLA]
  -safe [DpDatadWPA, Pod*WAP] [DpDatasWPA, Pod*WAP] [DpDatadWPA, Pos*WAP] [DpDatasWPA, Pos*WAP]
  -safe [DpDatadWPQ, Pod*WQP] [DpDatasWPQ, Pod*WQP] [DpDatadWPQ, Pos*WQP] [DpDatasWPQ, Pos*WQP]
  -safe [DpDatadW, Pod*WPL] [DpDatasW, Pod*WPL] [DpDatadW, Pos*WPL] [DpDatasW, Pos*WPL]
  -safe [DpCtrld*, DSB.SYd*W] [DpCtrls*, DSB.SYd*W] [DpCtrld*, DSB.SYs*W] [DpCtrls*, DSB.SYs*W]
  -safe [DpCtrldR, DSB.LDdRW] [DpCtrlsR, DSB.LDdRW] [DpCtrldR, DSB.LDsRW] [DpCtrlsR, DSB.LDsRW]
  -safe [DpCtrldW, DSB.STdWW] [DpCtrlsW, DSB.STdWW] [DpCtrldW, DSB.STsWW] [DpCtrlsW, DSB.STsWW]
  -safe [DpCtrldR, DpCtrldW, ISB] [DpCtrlsR, DpCtrldW, ISB] [DpCtrldR, DpCtrlsW, ISB] [DpCtrlsR, DpCtrlsW, ISB]
  -safe [DpCtrldR, DpCtrldW, ISB] [DpCtrlsR, DpCtrldW, ISB] [DpCtrldR, DpCtrlsW, ISB] [DpCtrlsR, DpCtrlsW, ISB]
  -safe [DpCtrldR, DpAddrd*, ISBd*W] [DpCtrlsR, DpAddrd*, ISBd*W] [DpCtrldR, DpAddrs*, ISBd*W] [DpCtrlsR, DpAddrs*, ISBd*W] [DpCtrldR, DpAddrd*, ISBs*W] [DpCtrlsR, DpAddrd*, ISBs*W] [DpCtrldR, DpAddrs*, ISBs*W] [DpCtrlsR, DpAddrs*, ISBs*W]
  -safe [DpCtrldR, DpAddrd*, ISBd*W] [DpCtrlsR, DpAddrd*, ISBd*W] [DpCtrldR, DpAddrs*, ISBd*W] [DpCtrlsR, DpAddrs*, ISBd*W] [DpCtrldR, DpAddrd*, ISBs*W] [DpCtrlsR, DpAddrd*, ISBs*W] [DpCtrldR, DpAddrs*, ISBs*W] [DpCtrlsR, DpAddrs*, ISBs*W]
  -safe [DpCtrld*, DSB.SYd*W, ISB] [DpCtrls*, DSB.SYd*W, ISB] [DpCtrld*, DSB.SYs*W, ISB] [DpCtrls*, DSB.SYs*W, ISB]
  -safe [DpCtrldR, DSB.LDdRW, ISB] [DpCtrlsR, DSB.LDdRW, ISB] [DpCtrldR, DSB.LDsRW, ISB] [DpCtrlsR, DSB.LDsRW, ISB]
  -safe [DpCtrldW, DSB.STdWW, ISB] [DpCtrlsW, DSB.STdWW, ISB] [DpCtrldW, DSB.STsWW, ISB] [DpCtrlsW, DSB.STsWW, ISB]
  -safe [DpCtrld*, Pos*W] [DpCtrls*, Pos*W]
  -safe [DpCtrld*, LxSx] [DpCtrls*, LxSx] [DpCtrld*, Amo.Swp] [DpCtrls*, Amo.Swp] [DpCtrld*, Amo.Cas] [DpCtrls*, Amo.Cas]
  -safe [DpCtrld*, DMB.SYd*W] [DpCtrls*, DMB.SYd*W] [DpCtrld*, DMB.SYs*W] [DpCtrls*, DMB.SYs*W]
  -safe [DpCtrldR, DMB.LDdRW] [DpCtrlsR, DMB.LDdRW] [DpCtrldR, DMB.LDsRW] [DpCtrlsR, DMB.LDsRW]
  -safe [DpCtrldW, DMB.STdWW] [DpCtrlsW, DMB.STdWW] [DpCtrldW, DMB.STsWW] [DpCtrlsW, DMB.STsWW]
  -safe [DpCtrld*PA, Amo.SwpAL, Pod*WLP] [DpCtrls*PA, Amo.SwpAL, Pod*WLP] [DpCtrld*PA, Amo.CasAL, Pod*WLP] [DpCtrls*PA, Amo.CasAL, Pod*WLP] [DpCtrld*PA, Amo.SwpAL, Pos*WLP] [DpCtrls*PA, Amo.SwpAL, Pos*WLP] [DpCtrld*PA, Amo.CasAL, Pos*WLP] [DpCtrls*PA, Amo.CasAL, Pos*WLP]
  -safe [DpCtrld*PL, Pod*WLA] [DpCtrls*PL, Pod*WLA] [DpCtrld*PL, Pos*WLA] [DpCtrls*PL, Pos*WLA]
  -safe [DpCtrld*PA, Pod*WAP] [DpCtrls*PA, Pod*WAP] [DpCtrld*PA, Pos*WAP] [DpCtrls*PA, Pos*WAP]
  -safe [DpCtrld*PQ, Pod*WQP] [DpCtrls*PQ, Pod*WQP] [DpCtrld*PQ, Pos*WQP] [DpCtrls*PQ, Pos*WQP]
  -safe [DpCtrld*, Pod*WPL] [DpCtrls*, Pod*WPL] [DpCtrld*, Pos*WPL] [DpCtrls*, Pos*WPL]
  
  ### hw-reqs
  -safe DSB.SYd** DSB.SYs**
  -safe DSB.LDdR* DSB.LDsR*
  -safe DSB.STdW* DSB.STsW*
  -safe [DpCtrld*, ISB] [DpCtrls*, ISB]
  -safe [DpCtrld*, ISB] [DpCtrls*, ISB]
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
  -safe [DSB.SYd**, ISB] [DSB.SYs**, ISB]
  -safe [DSB.LDdR*, ISB] [DSB.LDsR*, ISB]
  -safe [DSB.STdW*, ISB] [DSB.STsW*, ISB]
  -safe Pos*W
  -safe DpAddrd* DpAddrs*
  -safe DpDatadW DpDatasW
  -safe DpCtrldW DpCtrlsW
  -safe [DpAddrd*, Pod*W] [DpAddrs*, Pod*W] [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  -safe [DpAddrdW, PosWR] [DpAddrsW, PosWR]
  -safe [DpDatadW, PosWR] [DpDatasW, PosWR]
  -safe DpAddrdW DpAddrsW
  -safe DpDatadW DpDatasW
  -safe DpCtrldW DpCtrlsW
  -safe [DpAddrd*, Pod*W] [DpAddrs*, Pod*W] [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  -safe LxSx Amo.Swp Amo.Cas
  -safe [LxSx, PosWRPA] [Amo.Swp, PosWRPA] [Amo.Cas, PosWRPA]
  -safe [LxSx, PosWRPQ] [Amo.Swp, PosWRPQ] [Amo.Cas, PosWRPQ]
  -safe DMB.SYd** DMB.SYs**
  -safe DMB.LDdR* DMB.LDsR*
  -safe DMB.STdWW DMB.STsWW
  -safe [Amo.SwpAL, Pod**LP] [Amo.CasAL, Pod**LP] [Amo.SwpAL, Pos**LP] [Amo.CasAL, Pos**LP]
  -safe Pod**LA Pos**LA
  -safe Pod**AP Pos**AP
  -safe Pod**QP Pos**QP
  -safe Pod**PL Pos**PL
  -safe [PosWR, DSB.SYdRW] [PosWR, DSB.SYsRW]
  -safe [PosWR, DSB.LDdRW] [PosWR, DSB.LDsRW]
  -safe [PosWR, DpCtrldW, ISB] [PosWR, DpCtrlsW, ISB]
  -safe [PosWR, DpCtrldW, ISB] [PosWR, DpCtrlsW, ISB]
  -safe [PosWR, DpAddrd*, ISBd*W] [PosWR, DpAddrs*, ISBd*W] [PosWR, DpAddrd*, ISBs*W] [PosWR, DpAddrs*, ISBs*W]
  -safe [PosWR, DpAddrd*, ISBd*W] [PosWR, DpAddrs*, ISBd*W] [PosWR, DpAddrd*, ISBs*W] [PosWR, DpAddrs*, ISBs*W]
  -safe [PosWR, DSB.SYdRW, ISB] [PosWR, DSB.SYsRW, ISB]
  -safe [PosWR, DSB.LDdRW, ISB] [PosWR, DSB.LDsRW, ISB]
  -safe [PosWR, PosRW]
  -safe [PosWR, DpAddrdW] [PosWR, DpAddrsW]
  -safe [PosWR, DpDatadW] [PosWR, DpDatasW]
  -safe [PosWR, DpCtrldW] [PosWR, DpCtrlsW]
  -safe [PosWR, DpAddrd*, Pod*W] [PosWR, DpAddrs*, Pod*W] [PosWR, DpAddrd*, Pos*W] [PosWR, DpAddrs*, Pos*W]
  -safe [PosWR, DpAddrdW] [PosWR, DpAddrsW]
  -safe [PosWR, DpDatadW] [PosWR, DpDatasW]
  -safe [PosWR, DpCtrldW] [PosWR, DpCtrlsW]
  -safe [PosWR, DpAddrd*, Pod*W] [PosWR, DpAddrs*, Pod*W] [PosWR, DpAddrd*, Pos*W] [PosWR, DpAddrs*, Pos*W]
  -safe [PosWR, LxSx] [PosWR, Amo.Swp] [PosWR, Amo.Cas]
  -safe [PosWR, DMB.SYdRW] [PosWR, DMB.SYsRW]
  -safe [PosWR, DMB.LDdRW] [PosWR, DMB.LDsRW]
  -safe [PosWRPL, PodRWLA] [PosWRPL, PosRWLA]
  -safe [PosWRPA, PodRWAP] [PosWRPA, PosRWAP]
  -safe [PosWRPQ, PodRWQP] [PosWRPQ, PosRWQP]
  -safe [PosWR, PodRWPL] [PosWR, PosRWPL]
  -safe [DpAddrd*, DSB.SYd*W] [DpAddrs*, DSB.SYd*W] [DpAddrd*, DSB.SYs*W] [DpAddrs*, DSB.SYs*W]
  -safe [DpAddrdR, DSB.LDdRW] [DpAddrsR, DSB.LDdRW] [DpAddrdR, DSB.LDsRW] [DpAddrsR, DSB.LDsRW]
  -safe [DpAddrdW, DSB.STdWW] [DpAddrsW, DSB.STdWW] [DpAddrdW, DSB.STsWW] [DpAddrsW, DSB.STsWW]
  -safe [DpAddrdR, DpCtrldW, ISB] [DpAddrsR, DpCtrldW, ISB] [DpAddrdR, DpCtrlsW, ISB] [DpAddrsR, DpCtrlsW, ISB]
  -safe [DpAddrdR, DpCtrldW, ISB] [DpAddrsR, DpCtrldW, ISB] [DpAddrdR, DpCtrlsW, ISB] [DpAddrsR, DpCtrlsW, ISB]
  -safe [DpAddrdR, DpAddrd*, ISBd*W] [DpAddrsR, DpAddrd*, ISBd*W] [DpAddrdR, DpAddrs*, ISBd*W] [DpAddrsR, DpAddrs*, ISBd*W] [DpAddrdR, DpAddrd*, ISBs*W] [DpAddrsR, DpAddrd*, ISBs*W] [DpAddrdR, DpAddrs*, ISBs*W] [DpAddrsR, DpAddrs*, ISBs*W]
  -safe [DpAddrdR, DpAddrd*, ISBd*W] [DpAddrsR, DpAddrd*, ISBd*W] [DpAddrdR, DpAddrs*, ISBd*W] [DpAddrsR, DpAddrs*, ISBd*W] [DpAddrdR, DpAddrd*, ISBs*W] [DpAddrsR, DpAddrd*, ISBs*W] [DpAddrdR, DpAddrs*, ISBs*W] [DpAddrsR, DpAddrs*, ISBs*W]
  -safe [DpAddrd*, DSB.SYd*W, ISB] [DpAddrs*, DSB.SYd*W, ISB] [DpAddrd*, DSB.SYs*W, ISB] [DpAddrs*, DSB.SYs*W, ISB]
  -safe [DpAddrdR, DSB.LDdRW, ISB] [DpAddrsR, DSB.LDdRW, ISB] [DpAddrdR, DSB.LDsRW, ISB] [DpAddrsR, DSB.LDsRW, ISB]
  -safe [DpAddrdW, DSB.STdWW, ISB] [DpAddrsW, DSB.STdWW, ISB] [DpAddrdW, DSB.STsWW, ISB] [DpAddrsW, DSB.STsWW, ISB]
  -safe [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  -safe [DpAddrd*, LxSx] [DpAddrs*, LxSx] [DpAddrd*, Amo.Swp] [DpAddrs*, Amo.Swp] [DpAddrd*, Amo.Cas] [DpAddrs*, Amo.Cas]
  -safe [DpAddrd*, DMB.SYd*W] [DpAddrs*, DMB.SYd*W] [DpAddrd*, DMB.SYs*W] [DpAddrs*, DMB.SYs*W]
  -safe [DpAddrdR, DMB.LDdRW] [DpAddrsR, DMB.LDdRW] [DpAddrdR, DMB.LDsRW] [DpAddrsR, DMB.LDsRW]
  -safe [DpAddrdW, DMB.STdWW] [DpAddrsW, DMB.STdWW] [DpAddrdW, DMB.STsWW] [DpAddrsW, DMB.STsWW]
  -safe [DpAddrd*PA, Amo.SwpAL, Pod*WLP] [DpAddrs*PA, Amo.SwpAL, Pod*WLP] [DpAddrd*PA, Amo.CasAL, Pod*WLP] [DpAddrs*PA, Amo.CasAL, Pod*WLP] [DpAddrd*PA, Amo.SwpAL, Pos*WLP] [DpAddrs*PA, Amo.SwpAL, Pos*WLP] [DpAddrd*PA, Amo.CasAL, Pos*WLP] [DpAddrs*PA, Amo.CasAL, Pos*WLP]
  -safe [DpAddrd*PL, Pod*WLA] [DpAddrs*PL, Pod*WLA] [DpAddrd*PL, Pos*WLA] [DpAddrs*PL, Pos*WLA]
  -safe [DpAddrd*PA, Pod*WAP] [DpAddrs*PA, Pod*WAP] [DpAddrd*PA, Pos*WAP] [DpAddrs*PA, Pos*WAP]
  -safe [DpAddrd*PQ, Pod*WQP] [DpAddrs*PQ, Pod*WQP] [DpAddrd*PQ, Pos*WQP] [DpAddrs*PQ, Pos*WQP]
  -safe [DpAddrd*, Pod*WPL] [DpAddrs*, Pod*WPL] [DpAddrd*, Pos*WPL] [DpAddrs*, Pos*WPL]
  -safe [DpDatadW, DSB.SYd*W] [DpDatasW, DSB.SYd*W] [DpDatadW, DSB.SYs*W] [DpDatasW, DSB.SYs*W]
  -safe [DpDatadW, DSB.STdWW] [DpDatasW, DSB.STdWW] [DpDatadW, DSB.STsWW] [DpDatasW, DSB.STsWW]
  -safe [DpDatadW, DSB.SYd*W, ISB] [DpDatasW, DSB.SYd*W, ISB] [DpDatadW, DSB.SYs*W, ISB] [DpDatasW, DSB.SYs*W, ISB]
  -safe [DpDatadW, DSB.STdWW, ISB] [DpDatasW, DSB.STdWW, ISB] [DpDatadW, DSB.STsWW, ISB] [DpDatasW, DSB.STsWW, ISB]
  -safe [DpDatadW, Pos*W] [DpDatasW, Pos*W]
  -safe [DpDatadW, LxSx] [DpDatasW, LxSx] [DpDatadW, Amo.Swp] [DpDatasW, Amo.Swp] [DpDatadW, Amo.Cas] [DpDatasW, Amo.Cas]
  -safe [DpDatadW, DMB.SYd*W] [DpDatasW, DMB.SYd*W] [DpDatadW, DMB.SYs*W] [DpDatasW, DMB.SYs*W]
  -safe [DpDatadW, DMB.STdWW] [DpDatasW, DMB.STdWW] [DpDatadW, DMB.STsWW] [DpDatasW, DMB.STsWW]
  -safe [DpDatadWPA, Amo.SwpAL, Pod*WLP] [DpDatasWPA, Amo.SwpAL, Pod*WLP] [DpDatadWPA, Amo.CasAL, Pod*WLP] [DpDatasWPA, Amo.CasAL, Pod*WLP] [DpDatadWPA, Amo.SwpAL, Pos*WLP] [DpDatasWPA, Amo.SwpAL, Pos*WLP] [DpDatadWPA, Amo.CasAL, Pos*WLP] [DpDatasWPA, Amo.CasAL, Pos*WLP]
  -safe [DpDatadWPL, Pod*WLA] [DpDatasWPL, Pod*WLA] [DpDatadWPL, Pos*WLA] [DpDatasWPL, Pos*WLA]
  -safe [DpDatadWPA, Pod*WAP] [DpDatasWPA, Pod*WAP] [DpDatadWPA, Pos*WAP] [DpDatasWPA, Pos*WAP]
  -safe [DpDatadWPQ, Pod*WQP] [DpDatasWPQ, Pod*WQP] [DpDatadWPQ, Pos*WQP] [DpDatasWPQ, Pos*WQP]
  -safe [DpDatadW, Pod*WPL] [DpDatasW, Pod*WPL] [DpDatadW, Pos*WPL] [DpDatasW, Pos*WPL]
  -safe [DpCtrld*, DSB.SYd*W] [DpCtrls*, DSB.SYd*W] [DpCtrld*, DSB.SYs*W] [DpCtrls*, DSB.SYs*W]
  -safe [DpCtrldR, DSB.LDdRW] [DpCtrlsR, DSB.LDdRW] [DpCtrldR, DSB.LDsRW] [DpCtrlsR, DSB.LDsRW]
  -safe [DpCtrldW, DSB.STdWW] [DpCtrlsW, DSB.STdWW] [DpCtrldW, DSB.STsWW] [DpCtrlsW, DSB.STsWW]
  -safe [DpCtrldR, DpCtrldW, ISB] [DpCtrlsR, DpCtrldW, ISB] [DpCtrldR, DpCtrlsW, ISB] [DpCtrlsR, DpCtrlsW, ISB]
  -safe [DpCtrldR, DpCtrldW, ISB] [DpCtrlsR, DpCtrldW, ISB] [DpCtrldR, DpCtrlsW, ISB] [DpCtrlsR, DpCtrlsW, ISB]
  -safe [DpCtrldR, DpAddrd*, ISBd*W] [DpCtrlsR, DpAddrd*, ISBd*W] [DpCtrldR, DpAddrs*, ISBd*W] [DpCtrlsR, DpAddrs*, ISBd*W] [DpCtrldR, DpAddrd*, ISBs*W] [DpCtrlsR, DpAddrd*, ISBs*W] [DpCtrldR, DpAddrs*, ISBs*W] [DpCtrlsR, DpAddrs*, ISBs*W]
  -safe [DpCtrldR, DpAddrd*, ISBd*W] [DpCtrlsR, DpAddrd*, ISBd*W] [DpCtrldR, DpAddrs*, ISBd*W] [DpCtrlsR, DpAddrs*, ISBd*W] [DpCtrldR, DpAddrd*, ISBs*W] [DpCtrlsR, DpAddrd*, ISBs*W] [DpCtrldR, DpAddrs*, ISBs*W] [DpCtrlsR, DpAddrs*, ISBs*W]
  -safe [DpCtrld*, DSB.SYd*W, ISB] [DpCtrls*, DSB.SYd*W, ISB] [DpCtrld*, DSB.SYs*W, ISB] [DpCtrls*, DSB.SYs*W, ISB]
  -safe [DpCtrldR, DSB.LDdRW, ISB] [DpCtrlsR, DSB.LDdRW, ISB] [DpCtrldR, DSB.LDsRW, ISB] [DpCtrlsR, DSB.LDsRW, ISB]
  -safe [DpCtrldW, DSB.STdWW, ISB] [DpCtrlsW, DSB.STdWW, ISB] [DpCtrldW, DSB.STsWW, ISB] [DpCtrlsW, DSB.STsWW, ISB]
  -safe [DpCtrld*, Pos*W] [DpCtrls*, Pos*W]
  -safe [DpCtrld*, LxSx] [DpCtrls*, LxSx] [DpCtrld*, Amo.Swp] [DpCtrls*, Amo.Swp] [DpCtrld*, Amo.Cas] [DpCtrls*, Amo.Cas]
  -safe [DpCtrld*, DMB.SYd*W] [DpCtrls*, DMB.SYd*W] [DpCtrld*, DMB.SYs*W] [DpCtrls*, DMB.SYs*W]
  -safe [DpCtrldR, DMB.LDdRW] [DpCtrlsR, DMB.LDdRW] [DpCtrldR, DMB.LDsRW] [DpCtrlsR, DMB.LDsRW]
  -safe [DpCtrldW, DMB.STdWW] [DpCtrlsW, DMB.STdWW] [DpCtrldW, DMB.STsWW] [DpCtrlsW, DMB.STsWW]
  -safe [DpCtrld*PA, Amo.SwpAL, Pod*WLP] [DpCtrls*PA, Amo.SwpAL, Pod*WLP] [DpCtrld*PA, Amo.CasAL, Pod*WLP] [DpCtrls*PA, Amo.CasAL, Pod*WLP] [DpCtrld*PA, Amo.SwpAL, Pos*WLP] [DpCtrls*PA, Amo.SwpAL, Pos*WLP] [DpCtrld*PA, Amo.CasAL, Pos*WLP] [DpCtrls*PA, Amo.CasAL, Pos*WLP]
  -safe [DpCtrld*PL, Pod*WLA] [DpCtrls*PL, Pod*WLA] [DpCtrld*PL, Pos*WLA] [DpCtrls*PL, Pos*WLA]
  -safe [DpCtrld*PA, Pod*WAP] [DpCtrls*PA, Pod*WAP] [DpCtrld*PA, Pos*WAP] [DpCtrls*PA, Pos*WAP]
  -safe [DpCtrld*PQ, Pod*WQP] [DpCtrls*PQ, Pod*WQP] [DpCtrld*PQ, Pos*WQP] [DpCtrls*PQ, Pos*WQP]
  -safe [DpCtrld*, Pod*WPL] [DpCtrls*, Pod*WPL] [DpCtrld*, Pos*WPL] [DpCtrls*, Pos*WPL]
  -safe [PosRR, Fre]
  
  ### obs
