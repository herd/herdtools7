record = "AArch64"

cats = [
    "cats/aarch64.cat",
    "cats/aarch64-MTE.cat",
    ]

cfgs = [
    "cfgs/web-MTE.cfg",
]

illustrative_tests = [
    "tests/coWR-mte.litmus",
    "tests/MP+dmb.stPT+addr.litmus",
    "tests/MP+dmb.stTT+addr.litmus",
    "tests/S+dmb.stTT+addr.litmus",
    "tests/S+dmb.stTT+ctrl.litmus",
    "tests/S+dmb.stTT+data.litmus",
    "tests/S+dmb.stTT+rel.litmus",
    "tests/LDR-STZG1.litmus",
    "tests/LDR-STZG2.litmus",
    "tests/Wt+RtWtRt.litmus",
    "tests/RtWtRt+WztWzt.litmus",
    "tests/MTE-CoRR+addr+READS.litmus",
    "tests/MTE-CoRR+dmb.ld+READS.litmus",
    "tests/MTE-CoRR+READ+FAULT.litmus",
    "tests/MTE-CoRR+READS.litmus",
    "tests/MTE-CoRW1+FAULT.litmus",
    "tests/MTE-CoRW1+READ.litmus",
    "tests/MTE-CoRW2+FAULT.litmus",
    "tests/MTE-CoRW2+READ.litmus",
    "tests/MTE-CoWR+FAULT.litmus",
    "tests/MTE-CoWR+READ.litmus",
    "tests/MTE-CoWW.litmus",
    "tests/MTE-LDRLDR.litmus",
    "tests/MTE-LDRSTGLDR.litmus",
    "tests/MTE-RFI.litmus",
    "tests/MTE-RW+WR+amo.ldeoral-polp+dmb.sytq.litmus",
]
