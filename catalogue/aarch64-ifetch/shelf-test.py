record = "AArch64-ifetch"

cats = [
    "cats/aarch64.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/mytest2.litmus",
    "tests/MP.RF+dc.cvau-dmb.ish+dsb.ish.ic.ivau.dsb-rfiINSTRISB.litmus",
    "tests/SM.udf+dc.cvau+dsb.ish-ic.vau-dsb.ish-isb.litmus",
    "tests/MP.FF+dc.cvau-dsb.ish-ic.ivau-dsb.ish+po.litmus",
    "tests/DIC1.MP.RF+dmb.st+addr-rfiINSTRNOP.litmus",
    "tests/MP.FF+dc.cvau-dmb.ish+dsb.ish-ic.ivau-dsb.ish-rfiINSTNOP.litmus",
    "tests/DIC1.MP.RF+dmb.st+addr-rfiINSTRISB.litmus",
    "tests/MP.FF+dc.cvau-dsb.ish-ic.ivau-dsb.ish+dmb.ish+rfiINST.litmus",
    "tests/MP+rel+blr.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+dc-dsb-isb.litmus",
    "tests/DIC0-IDC0/SB.RF+dmb+dmb.st-cachesync-isb.litmus",
    "tests/DIC0-IDC0/coFW-1.litmus",
    "tests/DIC0-IDC0/WRC-mmrb-1.litmus",
    "tests/DIC0-IDC0/MP.FF+dc-dsb+isb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/SM.B+isb.litmus",
    "tests/DIC0-IDC0/MP.FW+dmb.st+po.litmus",
    "tests/DIC0-IDC0/WRC-inst+cachesync+ctrlisb-2.litmus",
    "tests/DIC0-IDC0/MP+irfi+dmb.ld.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+addr-cachesyncisb.litmus",
    "tests/DIC0-IDC0/SM.B+dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/WRC-inst+po+dsb-cachesyncisb.litmus",
    "tests/DIC0-IDC0/coFF+cachesync.litmus",
    "tests/DIC0-IDC0/SB-bnop-cachesyncs2.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.FR+dsb.ish+cachesync.litmus",
    "tests/DIC0-IDC0/WRC-inst+cachesync+ctrlisb.litmus",
    "tests/DIC0-IDC0/SM.B+badcachesync-isb.litmus",
    "tests/DIC0-IDC0/MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish+isb.litmus",
    "tests/DIC0-IDC0/coFW-pa2.litmus",
    "tests/DIC0-IDC0/WRC-inst.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb+addr-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/2SM.B+dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dmb+addr-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/SB.RF+dmb+dmb.ish-cachesync-isb.litmus",
    "tests/DIC0-IDC0/SM.B+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/SM.B+cachesync-isb.litmus",
    "tests/DIC0-IDC0/WRC-inst-mod+cachesync+po+ctrlisb.litmus",
    "tests/DIC0-IDC0/WRC-inst-modified-1.litmus",
    "tests/DIC0-IDC0/WRC-mmrb-1-noic.litmus",
    "tests/DIC0-IDC0/WRC-inst+cachesync+dsb.ish.litmus",
    "tests/DIC0-IDC0/SM.B+dc.cvau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+cachesync+switch-ctrlisb.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+po.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+DC-DSB-IC-DSB+DSB-ISB.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+dmb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.FR+dsb.ish+po.litmus",
    "tests/DIC0-IDC0/MP.FR+dmb.st+po.litmus",
    "tests/DIC0-IDC0/S-self+dmb.st+fault.litmus",
    "tests/DIC0-IDC0/dcc2.litmus",
    "tests/DIC0-IDC0/MP.RF+DC-DSB+DSB-IC-DSB-ISB.litmus",
    "tests/DIC0-IDC0/MP.RF+cachesync+dsb-isb.litmus",
    "tests/DIC0-IDC0/miniJit02.litmus",
    "tests/DIC0-IDC0/SM-invalid-2.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb-ic-dsb+dsb.ld-isb.litmus",
    "tests/DIC0-IDC0/SM.B+dc.cvau-dsb.ish.litmus",
    "tests/DIC0-IDC0/MP.RF+cachesync+switch-ctrl.litmus",
    "tests/DIC0-IDC0/icc2.litmus",
    "tests/DIC0-IDC0/SM.B+dsb.ish.litmus",
    "tests/DIC0-IDC0/S.RF-dsb-iccvau-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.FF+dc.cvau-dsb.ish+po.litmus",
    "tests/DIC0-IDC0/2SM.B+cachesync-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+dmb.ish-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/SB-bnop-cachesyncs1-reordered.litmus",
    "tests/DIC0-IDC0/MP.FF+dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/WRC-inst+dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+cachesync-isb.litmus",
    "tests/DIC0-IDC0/WRC-mmrb-1-nodc.litmus",
    "tests/DIC0-IDC0/MP.FR+dmb.st+isb.litmus",
    "tests/DIC0-IDC0/MP.FR+dmb.st+ctrlisb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb-ic-dsb+addr.litmus",
    "tests/DIC0-IDC0/WRC-inst+ctrlisb.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC0-IDC0/WRC-inst+po+dsb-iccvau-dsb-isb.litmus",
    "tests/DIC0-IDC0/SB-bnop-cachesyncs1.litmus",
    "tests/DIC0-IDC0/SM+lob-fault.litmus",
    "tests/DIC0-IDC0/2SM.B+dmb.sy-dsb.ish-ic.vau-dsb.ish+dmb.sy.litmus",
    "tests/DIC0-IDC0/MP.RF+cachesync+ctrlisb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb+ctrlisb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/SB-bnop-cachesyncs3.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+isb-cachesync-isb.litmus",
    "tests/DIC0-IDC0/MP.FR+dsb.ish+dsb.ish.litmus",
    "tests/DIC0-IDC0/WRC-inst+cachesync+dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/coFW-2.litmus",
    "tests/DIC0-IDC0/LB.RF+dmb.sy+po.litmus",
    "tests/DIC0-IDC0/MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC0/MP.RF+dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+dsb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb+dmb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/miniJit01.litmus",
    "tests/DIC0-IDC0/SM.BLR+cachesync-isb.litmus",
    "tests/DIC0-IDC0/coFF.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC0/SM.B+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/SM.B+badcachesync-isb-2.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb-ic-dsb+ctrlisb.br.litmus",
    "tests/DIC0-IDC0/MP.RF+dc.cvau-dmb.ish+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC0/SB.RF+dmb+dsb.st-cachesync-isb.litmus",
    "tests/DIC0-IDC0/coFW-pa1.litmus",
    "tests/DIC0-IDC0/icc3.litmus",
    "tests/DIC0-IDC0/WRC-inst+dsb.ish.litmus",
    "tests/DIC0-IDC0/SB-bnop-cachesyncs.litmus",
    "tests/DIC0-IDC0/WRC-inst-modified-2.litmus",
    "tests/DIC0-IDC0/MP.FF+dsb.ish+po.litmus",
    "tests/DIC0-IDC0/dcc3.litmus",
    "tests/DIC0-IDC0/MP.FF+dmb.st+cachesync.litmus",
    "tests/DIC0-IDC0/MP.RF+ctrlisb-dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC0-IDC0/MP.RF+dmb.st+ctrlisb.litmus",
    #"tests/UDF+2FH.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-mmrb-1-nodc.litmus",
    "tests/DIC0-IDC1/IDC1.2SM.B+cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dc.cvau-dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb-ic-dsb+addr.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst.litmus",
    "tests/DIC0-IDC1/IDC1.coFW-pa2.litmus",
    "tests/DIC0-IDC1/IDC1.S-self+dmb.st+fault.litmus",
    "tests/DIC0-IDC1/IDC1.SB.RF+dmb+dmb.st-cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dsb.ish+po.litmus",
    "tests/DIC0-IDC1/IDC1.SB-bnop-cachesyncs1-reordered.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+dsb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+cachesync+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+dmb.ish-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.SB.RF+dmb+dmb.ish-cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst-modified-1.litmus",
    "tests/DIC0-IDC1/IDC1.SM+lob-fault.litmus",
    "tests/DIC0-IDC1/IDC1.LB.RF+dmb.sy+po.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.2SM.B+dmb.sy-dsb.ish-ic.vau-dsb.ish+dmb.sy.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+addr-cachesyncisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.dcc2.litmus",
    "tests/DIC0-IDC1/IDC1.SB-bnop-cachesyncs2.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst-mod+cachesync+po+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.icc2.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+badcachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.coFW-2.litmus",
    "tests/DIC0-IDC1/IDC1.miniJit02.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+badcachesync-isb-2.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb-ic-dsb+dsb.ld-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+cachesync+switch-ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dsb.ish+cachesync.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dsb.ish+po.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb+ctrlisb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dmb+addr-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+ctrlisb-dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+po+dsb-iccvau-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst-modified-2.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FW+dmb.st+po.litmus",
    "tests/DIC0-IDC1/IDC1.coFF+cachesync.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb+addr-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.2SM.B+dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+cachesync+dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.SM.BLR+cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.SB-bnop-cachesyncs.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+cachesync+dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb+dmb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dc-dsb+isb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.coFW-pa1.litmus",
    "tests/DIC0-IDC1/IDC1.MP+irfi+dmb.ld.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dsb.ish+dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+isb-cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+dc-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.icc3.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+cachesync+ctrlisb-2.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+cachesync.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+cachesync+switch-ctrl.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-mmrb-1.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+cachesync+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+dmb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.S.RF-dsb-iccvau-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc-dsb-ic-dsb+ctrlisb.br.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.miniJit01.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.coFW-1.litmus",
    "tests/DIC0-IDC1/IDC1.SB-bnop-cachesyncs3.litmus",
    "tests/DIC0-IDC1/IDC1.dcc3.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+DC-DSB-IC-DSB+DSB-ISB.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+cachesync+dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+DC-DSB+DSB-IC-DSB-ISB.litmus",
    "tests/DIC0-IDC1/IDC1.SM-invalid-2.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-mmrb-1-noic.litmus",
    "tests/DIC0-IDC1/IDC1.SM.B+dc.cvau-dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dmb.st+po.litmus",
    "tests/DIC0-IDC1/IDC1.MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish+isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dmb.st+po.litmus",
    "tests/DIC0-IDC1/IDC1.SB-bnop-cachesyncs1.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+dsb.ish-isb.litmus",
    "tests/DIC0-IDC1/IDC1.coFF.litmus",
    "tests/DIC0-IDC1/IDC1.MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dmb.st+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+ctrlisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FR+dmb.st+isb.litmus",
    "tests/DIC0-IDC1/IDC1.WRC-inst+po+dsb-cachesyncisb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.FF+dc.cvau-dsb.ish+po.litmus",
    "tests/DIC0-IDC1/IDC1.SB.RF+dmb+dsb.st-cachesync-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dc.cvau-dmb.ish+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/MP.FF+dc.cvau-dsb.ish-ic.vau-dsb.ish+dmb.ish+rfiINST.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+cachesync+dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.coFW-2.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+cachesync+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-mmrb-1-nodc.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dsb.ish+po.litmus",
    "tests/DIC1-IDC1/DIC1.SB-bnop-cachesyncs.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+isb-cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-mmrb-1.litmus",
    "tests/DIC1-IDC1/DIC1.MP+irfi+dmb.ld.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dsb.ish+cachesync.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst-modified-2.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.miniJit02.litmus",
    "tests/DIC1-IDC1/DIC1.SM-invalid-2.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dmb.st+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.coFF+cachesync.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+DC-DSB-IC-DSB+DSB-ISB.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst-mod+cachesync+po+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+DC-DSB+DSB-IC-DSB-ISB.litmus",
    "tests/DIC1-IDC1/DIC1.LB.RF+dmb.sy+po.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+dc-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+badcachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb-ic-dsb+addr.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+cachesync+switch-ctrl.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+po+dsb-iccvau-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dc-dsb+isb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+dmb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.S.RF-dsb-iccvau-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SB-bnop-cachesyncs3.litmus",
    "tests/DIC1-IDC1/DIC1.SB.RF+dmb+dsb.st-cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.BLR+cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+cachesync+dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish+isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dc.cvau-dsb.ish+ic.vau-dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+cachesync+dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.dcc2.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+cachesync.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.icc2.litmus",
    "tests/DIC1-IDC1/DIC1.SB.RF+dmb+dmb.ish-cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb-ic-dsb+ctrlisb.br.litmus",
    "tests/DIC1-IDC1/DIC1.SB-bnop-cachesyncs1.litmus",
    "tests/DIC1-IDC1/DIC1.coFW-pa1.litmus",
    "tests/DIC1-IDC1/DIC1.2SM.B+dmb.sy-dsb.ish-ic.vau-dsb.ish+dmb.sy.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb+addr-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+addr-cachesyncisb.litmus",
    "tests/DIC1-IDC1/DIC1.S-self+dmb.st+fault.litmus",
    "tests/DIC1-IDC1/DIC1.2SM.B+dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dsb.ish+po.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FW+dmb.st+po.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dmb+addr-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.2SM.B+cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+dsb.ld-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SB-bnop-cachesyncs1-reordered.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb+ctrlisb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SB.RF+dmb+dmb.st-cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.coFW-1.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+cachesync+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.miniJit01.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc.cvau-dmb.ish+dsb.ish-ic.ivau-dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst-modified-1.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+cachesync-isb.litmus",
    "tests/DIC1-IDC1/DIC1.coFF.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb-ic-dsb+dsb.ld-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+cachesync+switch-ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.SB-bnop-cachesyncs2.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+ctrlisb-dc-dsb+ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+cachesync+ctrlisb-2.litmus",
    "tests/DIC1-IDC1/DIC1.coFW-pa2.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dsb.ish+dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-mmrb-1-noic.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dc.cvau-dsb.ish+po.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dc-dsb-ic-dsb+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FF+dmb.st+po.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+badcachesync-isb-2.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dmb.st+isb.litmus",
    "tests/DIC1-IDC1/DIC1.MP.FR+dmb.st+po.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+po+dsb-cachesyncisb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC1-IDC1/DIC1.icc3.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+dmb.ish-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.SM+lob-fault.litmus",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb+dmb-dc-dsb-ic-dsb-isb.litmus",
    "tests/DIC1-IDC1/DIC1.dcc3.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dc.cvau-dsb.ish-isb.litmus",
    "tests/DIC1-IDC1/DIC1.WRC-inst+ctrlisb.litmus",
    "tests/DIC1-IDC1/DIC1.SM.B+dc.cvau-dsb.ish.litmus",
]
