  $ mcat2config7 -set-libdir ./libdir -conf true -showrels true -let ca -let Exp-haz-ob -let haz-ob -let Exp-obs -let DSB-ob -let IFB-ob -let dob -let pob -let aob -let bob -let lwfs libdir/aarch64.cat
  
  ### ca
  ## fr
  -safe Fre Fri
  ## co
  -safe Coe Coi
  
  ### Exp-haz-ob
  ## [Exp & R]; (po & same-loc); [Exp & R]; (ca & ext); [Exp & W]
  -safe [PosRR, Fre]
  
  ### haz-ob
  ## Exp-haz-ob
  -safe [PosRR, Fre]
  
  ### Exp-obs
  ## [Exp & M]; (rf & ext); [Exp & M]
  -safe Rfe
  ## [Exp & M]; (ca & ext); [Exp & M]
  -safe Fre
  ## [Exp & M]; (ca & ext); [Exp & M]
  -safe Coe
  
  ### DSB-ob
  ## [M | DC.CVAU | IC]; po; [dsb.full]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe DSB.SYd** DSB.SYs**
  ## [((Exp & R) \ NoRet) | (Imp & (Tag & R))]; po; [dsb.ld]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe DSB.LDdR* DSB.LDsR*
  ## [Exp & W]; po; [dsb.st]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe DSB.STdW* DSB.STsW*
  
  ### IFB-ob
  ## [Exp & R]; ctrl; [IFB]; po
  -safe [DpCtrld*, ISB] [DpCtrls*, ISB]
  ## [Exp & R]; pick-ctrl-dep; [IFB]; po
  -safe [DpCtrlCseld*, ISB] [DpCtrlCsels*, ISB]
  ## [Exp & R]; addr; [Exp & M]; po; [IFB]; po
  -safe [DpAddrd*, ISBd**] [DpAddrs*, ISBd**] [DpAddrd*, ISBs**] [DpAddrs*, ISBs**]
  ## [Exp & R]; pick-addr-dep; [Exp & M]; po; [IFB]; po
  -safe [DpAddrCseld*, ISBd**] [DpAddrCsels*, ISBd**] [DpAddrCseld*, ISBs**] [DpAddrCsels*, ISBs**]
  ## DSB-ob; [IFB]; po
  -safe [DSB.SYd**, ISB] [DSB.SYs**, ISB]
  ## DSB-ob; [IFB]; po
  -safe [DSB.LDdR*, ISB] [DSB.LDsR*, ISB]
  ## DSB-ob; [IFB]; po
  -safe [DSB.STdW*, ISB] [DSB.STsW*, ISB]
  
  ### dob
  ## addr
  -safe DpAddrd* DpAddrs*
  ## data
  -safe DpDatadW DpDatasW
  ## ctrl; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpCtrldW DpCtrlsW
  ## addr; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddrd*, Pod*W] [DpAddrs*, Pod*W] [DpAddrd*, Pos*W] [DpAddrs*, Pos*W]
  ## addr; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpAddrdW, PosWR] [DpAddrsW, PosWR]
  ## data; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpDatadW, PosWR] [DpDatasW, PosWR]
  
  ### pob
  ## pick-addr-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpAddrCseldW DpAddrCselsW
  ## pick-data-dep
  -safe DpDataCseld* DpDataCsels*
  ## pick-ctrl-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpCtrlCseldW DpCtrlCselsW
  ## pick-addr-dep; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddrCseld*, Pod*W] [DpAddrCsels*, Pod*W] [DpAddrCseld*, Pos*W] [DpAddrCsels*, Pos*W]
  
  ### aob
  ## [Exp & M]; rmw; [Exp & M]
  -safe LxSx Amo.Swp Amo.Cas
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPA] [Amo.Swp, PosWRPA] [Amo.Cas, PosWRPA]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPA, Amo.SwpAP] [Amo.Swp, PosWRPA, Amo.SwpAP] [Amo.Cas, PosWRPA, Amo.SwpAP] [LxSx, PosWRPA, Amo.CasAP] [Amo.Swp, PosWRPA, Amo.CasAP] [Amo.Cas, PosWRPA, Amo.CasAP]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPQ] [Amo.Swp, PosWRPQ] [Amo.Cas, PosWRPQ]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPQ, Amo.SwpQP] [Amo.Swp, PosWRPQ, Amo.SwpQP] [Amo.Cas, PosWRPQ, Amo.SwpQP] [LxSx, PosWRPQ, Amo.CasQP] [Amo.Swp, PosWRPQ, Amo.CasQP] [Amo.Cas, PosWRPQ, Amo.CasQP]
  
  ### bob
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [dmb.full]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe DMB.SYd** DMB.SYs**
  ## [(Exp & (R \ NoRet)) | (Imp & (Tag & R))]; po; [dmb.ld]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe DMB.LDdR* DMB.LDsR*
  ## [Exp & W]; po; [dmb.st]; po; [(Exp & W) | (MMU & FAULT)]
  -safe DMB.STdWW DMB.STsWW
  ## [range([A]; amo; [L])]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [Amo.SwpAL, Pod**LP] [Amo.CasAL, Pod**LP] [Amo.SwpAL, Pos**LP] [Amo.CasAL, Pos**LP]
  ## [L]; po; [A]
  -safe Pod**LA Pos**LA
  ## [L]; po; [A]
  -safe [Pod**LA, Amo.SwpAP] [Pos**LA, Amo.SwpAP] [Pod**LA, Amo.CasAP] [Pos**LA, Amo.CasAP]
  ## [L]; po; [A]
  -safe [Amo.SwpPL, Pod**LA] [Amo.CasPL, Pod**LA] [Amo.SwpPL, Pos**LA] [Amo.CasPL, Pos**LA]
  ## [L]; po; [A]
  -safe [Amo.SwpPL, Pod**LA, Amo.SwpAP] [Amo.CasPL, Pod**LA, Amo.SwpAP] [Amo.SwpPL, Pos**LA, Amo.SwpAP] [Amo.CasPL, Pos**LA, Amo.SwpAP] [Amo.SwpPL, Pod**LA, Amo.CasAP] [Amo.CasPL, Pod**LA, Amo.CasAP] [Amo.SwpPL, Pos**LA, Amo.CasAP] [Amo.CasPL, Pos**LA, Amo.CasAP]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe Pod**AP Pos**AP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [Amo.SwpAP, Pod**] [Amo.CasAP, Pod**] [Amo.SwpAP, Pos**] [Amo.CasAP, Pos**]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe Pod**QP Pos**QP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [Amo.SwpQP, Pod**] [Amo.CasQP, Pod**] [Amo.SwpQP, Pos**] [Amo.CasQP, Pos**]
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe Pod**PL Pos**PL
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe [Pod**, Amo.SwpPL] [Pos**, Amo.SwpPL] [Pod**, Amo.CasPL] [Pos**, Amo.CasPL]
  
  ### lwfs
  ## [(Exp & M) | (Imp & (Tag & R))]; (po & same-loc); [Exp & W]
  -safe Pos*W
