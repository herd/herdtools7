record = "AArch64-faults"

cats = [
    "cats/aarch64.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/3faults.litmus",
    "tests/LDRaf0F.litmus",
    "tests/LDRredF.litmus",
    "tests/noUDF.litmus",
    "tests/STRdb0F.litmus",
    "tests/UDF.litmus",
]
