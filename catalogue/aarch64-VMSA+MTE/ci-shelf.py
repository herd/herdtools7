record = "AArch64 VMSA"

cats = [
    "cats/aarch64.cat",
]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/LDR-TaggedNormal.litmus",
    "tests/STR-TaggedNormal.litmus",
    "tests/STR-Normal-red.litmus",
    "tests/STR-TaggedNormal-db0.litmus",
    "tests/STR-TaggedNormal-db0red.litmus",
    "tests/STR-TaggedNormal-HA.litmus",
    "tests/STR-TaggedNormal-HD.litmus",
    "tests/STR-TaggedNormal-red-async.litmus",
    "tests/STR-TaggedNormal-red.litmus",
    "tests/STR-TaggedNormal-v0.litmus",
    "tests/STRgreen-TaggedNormalXorV0.litmus",
    "tests/STRred-NormalXorRW-1-AF.litmus",
    "tests/STRred-NormalXorRW-1-BBM.litmus",
    "tests/STRred-NormalXorRW-1-DB-2.litmus",
    "tests/STRred-NormalXorRW-1-DB.litmus",
    "tests/STRred-NormalXorRW-1.litmus",
    "tests/STRred-NormalXorRW-2.litmus",
    "tests/STRred-NormalXorRW-DB+SWP-1.litmus",
    "tests/STRred-NormalXorRW-DB+SWP-2.litmus",
    "tests/STRred-NormalXorV0-async.litmus",
    "tests/STRred-NormalXorV0.litmus",
    "tests/STRred-TaggedNormalXorV0-async.litmus",
    "tests/STRred-TaggedNormalXorV0.litmus",
]
