record = "AArch64 CAS"

cats = [
    "cats/aarch64.cat",
]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/LB+rel+CAS.litmus",
    "tests/LB+rel+CAS-ok-MRs-addr.litmus",
    "tests/LB+rel+CAS-no-MRs-addr.litmus",
    "tests/LB+rel+CAS-ok-RsM-addr.litmus",
    "tests/LB+rel+CAS-no-RsM-addr.litmus",
    "tests/LB+rel+CAS-ok-RsRs-addr.litmus",
    "tests/LB+rel+CAS-no-RsRs-addr.litmus",
    "tests/LB+rel+CAS-ok-RnM-addr.litmus",
    "tests/LB+rel+CAS-no-RnM-addr.litmus",
    "tests/LB+rel+CAS-ok-RnRs-addr.litmus",
    "tests/LB+rel+CAS-no-RnRs-addr.litmus",
    "tests/LB+rel+CAS-ok-RtRs-addr.litmus",
    "tests/LB+rel+CAS-no-RtRs-addr.litmus",
    "tests/LB+rel+CAS-ok-RtM-data.litmus",
    "tests/LB+rel+CAS-no-RtM-data.litmus",
    "tests/MP+rel+CAS-addr.litmus",
    "tests/MP+rel+CAS-ok-RsRs-addr.litmus",
    "tests/MP+rel+CAS-no-RsRs-addr.litmus",
    "tests/MP+rel+CAS-ok-RsM-addr.litmus",
    "tests/MP+rel+CAS-no-RsM-addr.litmus",
    "tests/MP+rel+CAS-ok-RtM-addr.litmus",
    "tests/MP+rel+CAS-no-RtM-addr.litmus",
    "tests/MP+rel+CAS-ok-RnRs-addr.litmus",
    "tests/MP+rel+CAS-no-RnRs-addr.litmus",
    "tests/MP+rel+CAS-ok-RtRs-addr.litmus",
    "tests/MP+rel+CAS-no-RtRs-addr.litmus",
    "tests/MP+rel+CAS-ok-MRs-addr.litmus",
    "tests/MP+rel+CAS-no-MRs-addr.litmus",
    "tests/MP+rel+CAS-ok-RnM-addr.litmus",
    "tests/MP+rel+CAS-no-RnM-addr.litmus",
    "tests/MP+rel+CAS-ok-bothRs-addr.litmus",
]
