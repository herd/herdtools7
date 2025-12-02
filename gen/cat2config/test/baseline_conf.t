  $ mcat2config7 -set-libdir ./libdir -conf true -dump origin -let ca -let Exp-haz-ob -let haz-ob -let Exp-obs -let DSB-ob -let IFB-ob -let dob -let pob -let aob -let bob -let lwfs libdir/aarch64.cat
  
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
  -safe LxSx Amo
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPA] [Amo, PosWRPA]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPA, AmoAP] [Amo, PosWRPA, AmoAP]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPQ] [Amo, PosWRPQ]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [LxSx, PosWRPQ, AmoQP] [Amo, PosWRPQ, AmoQP]
  
  ### bob
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [dmb.full]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe DMB.SYd** DMB.SYs**
  ## [(Exp & (R \ NoRet)) | (Imp & (Tag & R))]; po; [dmb.ld]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe DMB.LDdR* DMB.LDsR*
  ## [Exp & W]; po; [dmb.st]; po; [(Exp & W) | (MMU & FAULT)]
  -safe DMB.STdWW DMB.STsWW
  ## [range([A]; amo; [L])]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoAL, Pod**LP] [AmoAL, Pos**LP]
  ## [L]; po; [A]
  -safe Pod**LA Pos**LA
  ## [L]; po; [A]
  -safe [Pod**LA, AmoAP] [Pos**LA, AmoAP]
  ## [L]; po; [A]
  -safe [AmoPL, Pod**LA] [AmoPL, Pos**LA]
  ## [L]; po; [A]
  -safe [AmoPL, Pod**LA, AmoAP] [AmoPL, Pos**LA, AmoAP]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe Pod**AP Pos**AP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoAP, Pod**] [AmoAP, Pos**]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe Pod**QP Pos**QP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoQP, Pod**] [AmoQP, Pos**]
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe Pod**PL Pos**PL
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe [Pod**, AmoPL] [Pos**, AmoPL]
  
  ### lwfs
  ## [(Exp & M) | (Imp & (Tag & R))]; (po & same-loc); [Exp & W]
  -safe Pos*W

