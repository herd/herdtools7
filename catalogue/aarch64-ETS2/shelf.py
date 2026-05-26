record = "AArch64 ETS2"

cats = [
    "cats/aarch64.cat",
]

cfgs = [
    "cfgs/web.cfg",
]

illustrative_tests = [
    "tests/ETS2.MP+poptev1l+po.litmus",
    "tests/ETS2.CoWR0+posptev1p-fripptev1.litmus",
    "tests/ETS2.CoWR0+tlbi-sync.ishsptev0p-fripptev0.litmus",
    "tests/ETS2.CoWW+tlbi-sync.ishsptedb1p-coipptedb1.litmus",
    "tests/ETS2.MP+dsb.stpptehd+popteap.litmus",
    "tests/ETS2.SB+dsb.sy+dmb.syptev1p.litmus",
    "tests/ETS2.LB+dsb.ldpptehd+popteap.litmus",
]
