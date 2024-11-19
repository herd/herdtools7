record = "AArch64-PAC"

cats = [
    "cats/aarch64.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/xpacd-basic-dep.litmus",
    "tests/pacda-basic-dep-pointer.litmus",
    "tests/pacda-basic-dep-modifier.litmus",
    "tests/pacda-disable-basic-dep-pointer.litmus",
    "tests/pacda-disable-no-basic-dep-modifier.litmus",
    "tests/pacda-disable-no-pick-basic-dep-modifier.litmus",
    "tests/autda-disable-basic-dep-pointer.litmus",
    "tests/autda-disable-no-basic-dep-modifier.litmus",
    "tests/autda-disable-no-pick-basic-dep-modifier.litmus",
    "tests/autda-success-basic-dep-pointer.litmus",
    "tests/autda-fpac-success-basic-dep-pointer.litmus",
    "tests/autda-failure-basic-dep-pointer.litmus",
    "tests/autda-success-pick-basic-dep-modifier.litmus",
    "tests/autda-success-no-basic-dep-modifier.litmus",
    "tests/autda-fpac-success-pick-basic-dep-modifier.litmus",
    "tests/autda-fpac-success-no-basic-dep-modifier.litmus",
    "tests/autda-failure-basic-dep-modifier.litmus",
    "tests/autda-fpac-failure-pick-basic-dep-pointer.litmus",
    "tests/autda-fpac-failure-pick-basic-dep-modifier.litmus",
    "tests/autda-fpac-basic-dep-branch.litmus",
    "tests/autda-fpac-pick-basic-dep-branch.litmus",
    "tests/pauth1-autda-success-basic-dep-pointer.litmus",
    "tests/pauth1-autda-success-no-basic-dep-modifier.litmus",
    "tests/pauth1-autda-success-pick-basic-dep-modifier.litmus",
    "tests/pauth1-autda-failure-pick-basic-dep-modifier.litmus",
    "tests/pauth1-autda-failure-no-basic-dep-modifier.litmus",
    "tests/pauth1-autda-failure-basic-dep-pointer.litmus",
]
