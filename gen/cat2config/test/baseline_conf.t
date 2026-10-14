  $ mcat2config7 --set-libdir ./libdir --conf --dump origin --let ca --let Exp-haz-ob --let haz-ob --let Exp-obs --let DSB-ob --let IFB-ob --let dob --let pob --let aob --let bob --let lwfs libdir/aarch64.cat
  ### ca
  ## fr
  -safe Fr
  ## co
  -safe Co
  
  ### Exp-haz-ob
  ## [Exp & R]; (po & same-loc); [Exp & R]; (ca & ext); [Exp & W]
  -safe [PosRR,Fre] [PosRR,[LxSx|Amo.Safe],Coe] [[LxSx|Amo.Safe],PosWR,Fre] [[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe],Coe]
  
  ### haz-ob
  ## Exp-haz-ob
  -safe [PosRR,Fre] [PosRR,[LxSx|Amo.Safe],Coe] [[LxSx|Amo.Safe],PosWR,Fre] [[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe],Coe]
  
  ### Exp-obs
  ## [Exp & M]; (rf & ext); [Exp & M]
  -safe Rfe
  ## [Exp & M]; (ca & ext); [Exp & M]
  -safe [Fre|Coe]
  
  ### DSB-ob
  ## [M | DC.CVAU | IC]; po; [dsb.full]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe [[LxSx|Amo.Safe]?,DSB.SY***,[LxSx|Amo.Safe]?]
  ## [((Exp & R) \ NoRet) | (Imp & (Tag & R))]; po; [dsb.ld]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe [[LxSx|Amo.Safe],DSB.LD*W*,[LxSx|Amo.Safe]?] [DSB.LD*R*,[LxSx|Amo.Safe]?]
  ## [Exp & W]; po; [dsb.st]; po; [~((Imp & (TTD & M)) | (Imp & (Instr & R)))]
  -safe [[LxSx|Amo.Safe]?,DSB.ST*W*,[LxSx|Amo.Safe]?]
  
  ### IFB-ob
  ## [Exp & R]; ctrl; [IFB]; po
  -safe [[LxSx|Amo.Safe]?,DpCtrl,ISB,[LxSx|Amo.Safe]?]
  ## [Exp & R]; pick-ctrl-dep; [IFB]; po
  -safe [[LxSx|Amo.Safe]?,DpCtrlCsel,ISB,[LxSx|Amo.Safe]?]
  ## [Exp & R]; addr; [Exp & M]; po; [IFB]; po
  -safe [DpAddr,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  ## [Exp & R]; pick-addr-dep; [Exp & M]; po; [IFB]; po
  -safe [[LxSx|Amo.Safe]?,DpAddrCsel,[LxSx|Amo.Safe]?,ISB***,[LxSx|Amo.Safe]?]
  ## DSB-ob; [IFB]; po
  -safe [[LxSx|Amo.Safe],[DSB.LD*W*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?] [[DSB.LD*R*|DSB.ST*W*|DSB.SY***],ISB,[LxSx|Amo.Safe]?]
  
  ### dob
  ## addr
  -safe DpAddr
  ## data
  -safe DpData*W
  ## ctrl; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe [DpCtrl*R,[LxSx|Amo.Safe]] DpCtrl*W
  ## addr; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddr,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]] [DpAddr,[LxSx|Amo.Safe]?,Po**W]
  ## addr; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpAddr*R,[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe]?] [DpAddr*W,PosWR,[LxSx|Amo.Safe]?]
  ## data; [Exp & M]; lrs; [(Exp & R) | (Imp & (Tag & R))]
  -safe [DpData*W,PosWR,[LxSx|Amo.Safe]?]
  
  ### pob
  ## pick-addr-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe [DpAddrCsel*R,[LxSx|Amo.Safe]] DpAddrCsel*W
  ## pick-data-dep
  -safe DpDataCsel
  ## pick-ctrl-dep; [(Exp & W) | HU | TLBI | DC.CVAU | IC]
  -safe [DpCtrlCsel*R,[LxSx|Amo.Safe]] DpCtrlCsel*W
  ## pick-addr-dep; [Exp & M]; po; [(Exp & W) | HU]
  -safe [DpAddrCsel,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]] [DpAddrCsel,[LxSx|Amo.Safe]?,Po**W]
  
  ### aob
  ## [Exp & M]; rmw; [Exp & M]
  -safe [LxSx|Amo.Safe]
  ## [Exp & M]; rmw; lrs; [A | Q]
  -safe [[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  
  ### bob
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [dmb.full]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [[LxSx|Amo.Safe]?,DMB.SY***,[LxSx|Amo.Safe]?]
  ## [(Exp & (R \ NoRet)) | (Imp & (Tag & R))]; po; [dmb.ld]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [[LxSx|Amo.Safe],DMB.LD*W*,[LxSx|Amo.Safe]?] [DMB.LD*R*,[LxSx|Amo.Safe]?]
  ## [Exp & W]; po; [dmb.st]; po; [(Exp & W) | (MMU & FAULT)]
  -safe [[LxSx|Amo.Safe]?,DMB.ST*WR,[LxSx|Amo.Safe]] [[LxSx|Amo.Safe]?,DMB.ST*WW]
  ## [range([A]; amo; [L])]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [[LxSx|Amo.Safe]?,Po**R,A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?] [A,Amo.Safe,L,Po*W*,[LxSx|Amo.Safe]?]
  ## [L]; po; [A]
  -safe [[LxSx|Amo.Safe]?,L,Po*WR,A,[LxSx|Amo.Safe]?]
  ## [A | Q]; po; [(Exp & M) | (Imp & (Tag & R)) | (MMU & FAULT)]
  -safe [[A|Q],[LxSx|Amo.Safe],Po*W*,[LxSx|Amo.Safe]?] [[A|Q],Po*R*,[LxSx|Amo.Safe]?]
  ## [(Exp & M) | (Imp & (Tag & R))]; po; [L]
  -safe [[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe],L] [[LxSx|Amo.Safe]?,Po**W,L]
  
  ### lwfs
  ## [(Exp & M) | (Imp & (Tag & R))]; (po & same-loc); [Exp & W]
  -safe [[LxSx|Amo.Safe]?,Pos*R,[LxSx|Amo.Safe]] [[LxSx|Amo.Safe]?,Pos*W]

Pos*W pruning is opt-in.

  $ mcat2config7 -prune all --set-libdir ./libdir --conf --let dob libdir/aarch64.cat
  ### dob
  -safe [[DpAddrdW|DpAddr*R],@after([ExpObs|Hat])]
  -safe DpDatadW
  -safe DpCtrldW
  -safe [DpAddr,Po**W]
  -safe [DpAddr*W,PosWR]
  -safe [DpData*W,PosWR]

Without an explicit pruning option, all translated alternatives are retained.

  $ mcat2config7 --set-libdir ./libdir --conf --let dob libdir/aarch64.cat
  ### dob
  -safe DpAddr
  -safe DpData*W
  -safe [DpCtrl*R,[LxSx|Amo.Safe]] DpCtrl*W
  -safe [DpAddr,[LxSx|Amo.Safe]?,Po**R,[LxSx|Amo.Safe]] [DpAddr,[LxSx|Amo.Safe]?,Po**W]
  -safe [DpAddr*R,[LxSx|Amo.Safe],PosWR,[LxSx|Amo.Safe]?] [DpAddr*W,PosWR,[LxSx|Amo.Safe]?]
  -safe [DpData*W,PosWR,[LxSx|Amo.Safe]?]
