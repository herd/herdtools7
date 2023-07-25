record = "AArch64 ifetch"

cats = [
    "cats/aarch64.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/DIC0-IDC0/MP-inst+dc.cvau-dsb.ish-ic.vau-dsb.ish.litmus",
    "tests/DIC0-IDC0/MP.RF+cachesync+ctrlisb",
    "tests/DIC0-IDC0/SM.B+cachesync-isb.litmus",
    "tests/DIC0-IDC0/coFF+cachesync.litmus",
    "tests/DIC0-IDC0/MP.RF+dc-dsb+ctrlisb-ic-dsb-isb.litmus",
    "tests/DIC0-IDC1/IDC1.MP.RF+dmb.st+ctrlisb-ic-dsb-isb.litmus ",
    "tests/DIC1-IDC1/IDC1.MP.RF+dmb.st+ctrlisb-ic-dsb-isb.litmus ",
    "tests/DIC1-IDC1/DIC1.MP.RF+dmb.st+ctrlisb.litmus",
]
