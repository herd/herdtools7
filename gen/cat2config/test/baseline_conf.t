  $ mcat2config7 -set-libdir ./libdir -conf true -dump origin -let ca -let Exp-haz-ob -let haz-ob -let Exp-obs -let DSB-ob -let IFB-ob -let dob -let pob -let aob -let bob -let lwfs libdir/aarch64.cat
  
  ### ca
  ## fr
  -safe Fr
  ## co
  -safe Co
  
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
  -safe DSB.SY***
  ## [((Exp & R) \ NoRet) | (Imp & (Tag & R))]; po; [dsb.ld]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe DSB.LD*R*
  ## [Exp & W]; po; [dsb.st]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe DSB.ST*W*
  
  ### IFB-ob
  ## [Exp & R]; ctrl; [IFB]; po
  -safe [DpCtrl, ISB]
  ## [Exp & R]; pick-ctrl-dep; [IFB]; po
  -safe [DpCtrlCsel, ISB]
  ## [Exp & R]; addr; [Exp & M]; po; [IFB]; po
  -safe [DpAddr, ISB***]
  ## [Exp & R]; pick-addr-dep; [Exp & M]; po; [IFB]; po
  -safe [DpAddrCsel, ISB***]
  ## DSB-ob; [IFB]; po
  -safe [DSB.SY***, ISB]
  ## DSB-ob; [IFB]; po
  -safe [DSB.LD*R*, ISB]
  ## DSB-ob; [IFB]; po
  -safe [DSB.ST*W*, ISB]
  
  ### dob
  ## addr
  -safe DpAddr
  ## data
  -safe DpData*W
  ## ctrl; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpCtrl*W
  ## addr; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddr, Po**W]
  ## addr; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpAddr*W, PosWR]
  ## data; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpData*W, PosWR]
  
  ### pob
  ## pick-addr-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpAddrCsel*W
  ## pick-data-dep
  -safe DpDataCsel
  ## pick-ctrl-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe DpCtrlCsel*W
  ## pick-addr-dep; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddrCsel, Po**W]
  
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
  -safe DMB.SY***
  ## [(Exp & (R \ NoRet)) | (Imp & (Tag & R))]; po; [dmb.ld]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe DMB.LD*R*
  ## [Exp & W]; po; [dmb.st]; po; [(Exp & W) | (MMU & FAULT)]
  -safe DMB.ST*WW
  ## [range([A]; amo; [L])]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoAL, PoLP]
  ## [L]; po; [A]
  -safe PoLA
  ## [L]; po; [A]
  -safe [PoLA, AmoAP]
  ## [L]; po; [A]
  -safe [AmoPL, PoLA]
  ## [L]; po; [A]
  -safe [AmoPL, PoLA, AmoAP]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe PoAP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoAP, Po]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe PoQP
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [AmoQP, Po]
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe PoPL
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe [Po, AmoPL]
  
  ### lwfs
  ## [(Exp & M) | (Imp & (Tag & R))]; (po & same-loc); [Exp & W]
  -safe Pos*W

