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
    "tests/SVC.litmus",
    "tests/MP+dmb.st+ctrl-svc.exs1eis0.litmus",
    "tests/MP+dmb.st+ctrl-svc.exs1eis1.litmus",
    "tests/SVC-ifetch.exs1eis0.litmus",
    "tests/SVC-ifetch.exs1eis1.litmus",
    # "tests/SVC-pte.exs1eis0.litmus",
    # "tests/SVC-pte.exs1eis1.litmus",
]
