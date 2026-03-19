record = "AArch64-PAC"

cats = [
    "cats/aarch64.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg",
]

illustrative_tests = [
    "tests/pauth1+MP+dmb.sy+pacdza-autda.litmus",
    "tests/pauth1+MP+dmb.sy+pacdza-autdza.litmus",
    "tests/pauth1+S+dmb.sy+pacdza-autda.litmus",
    "tests/pauth1+WW+RR+R+dmb.sy+autda-xpacd+pacdza.litmus",
    "tests/pauth1+WW+RR+R+dmb.sy+autdza-xpacd+pacdza.litmus",
    "tests/pauth1+WW+RW+R+dmb.sy+autda-xpacd+pacdza.litmus",
    "tests/pauth2+MP+dmb.sy+add-pacdza-xpacd.litmus",
    "tests/pauth2+MP+dmb.sy+pacda-xpacd.litmus",
    "tests/pauth2+MP+dmb.sy+pacdza-add-xpacd.litmus",
    "tests/pauth2+MP+dmb.sy+pacdza-autda.litmus",
    "tests/pauth2+MP+dmb.sy+pacdza-autdza.litmus",
    "tests/pauth2+S+dmb.sy+pacdza-autda.litmus",
    "tests/pauth2+WW+RR+R+dmb.sy+autda+pacdza.litmus",
    "tests/pauth2+WW+RR+R+dmb.sy+autdza-xpacd+pacdza.litmus",
    "tests/pauth2-fpac+MP+dmb.sy+pacdza-autda.litmus",
    "tests/pauth2-fpac+MP+dmb.sy+pacdza-autdza.litmus",
    "tests/pauth2-fpac+S+dmb.sy+pacdza-autda.litmus",
    "tests/pauth2-fpac+S+dmb.sy+pacdza-autdza.litmus",
    "tests/pauth2-fpac+S+dmb.sy+pacdza-csel-autda.litmus",
    "tests/pauth2-fpac+WW+R+R+dmb.sy+autda-mov.litmus",
    "tests/pauth2-fpac+WW+R+R+dmb.sy+autdza-mov.litmus",
    "tests/pauth2-no-key-da+MP+dmb.sy+autda.litmus",
    "tests/pauth2-no-key-da+MP+dmb.sy+autdza.litmus",
    "tests/pauth2-no-key-da+MP+dmb.sy+pacda.litmus",
    "tests/pauth2-no-key-da+MP+dmb.sy+pacdza.litmus",
    "tests/pauth2-no-key-da+S+dmb.sy+pacda.litmus",
    "tests/pauth2-no-key-da+S+dmb.sy+po-autda.litmus",
]
