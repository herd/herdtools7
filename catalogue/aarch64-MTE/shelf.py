record = "AArch64"

cats = [
    "cats/aarch64-MTE.cat",
    ]

cfgs = [
    "cfgs/web-MTE.cfg",
]

illustrative_tests = [
    "tests/coWR-mte.litmus",
    "tests/MP+dmb.stPT+addr.litmus",
    "tests/MP+dmb.stTT+addr.litmus",
    "tests/R+dmb.stTT+rel-acq.litmus",
    "tests/S+dmb.stTT+addr.litmus",
    "tests/S+dmb.stTT+ctrl.litmus",
    "tests/S+dmb.stTT+data.litmus",
    "tests/S+dmb.stTT+rel.litmus",
]
