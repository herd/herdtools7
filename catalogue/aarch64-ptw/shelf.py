record = "ptw"

cats = [
    "cats/aarch64-ptw.cat",
    "cats/aarch64-ptw-ETS.cat",
]

cfgs = [
    "cfgs/ptw.cfg",
]

tests = [
]

illustrative_tests = [
  "tests/illustrative/D15347-M-load-DMBST+DMBLD.litmus",
  "tests/illustrative/D15347-M-load-DSB+DSB-ISB.litmus",
  "tests/illustrative/D15347-M-load-shoot+DMB.LD.litmus",
  "tests/illustrative/D15347-M-load-shoot+acq-po.litmus",
  "tests/illustrative/D15347-M-load-shoot+acqPC-po.litmus",
  "tests/illustrative/D15347-M-load.litmus",
  "tests/illustrative/D15347-M-store-DMBST+DMBLD.litmus",
  "tests/illustrative/D15347-M-store-DSB+DMBLD.litmus",
  "tests/illustrative/D15347-M-store-DSB+DSB-ISB.litmus",
  "tests/illustrative/D15347-M-store-shoot+DMB.ST.litmus",
  "tests/illustrative/D15347-load-DMB.litmus",
  "tests/illustrative/D15347-load-DSB-ISB.litmus",
  "tests/illustrative/D15347-load-DSB.litmus",
  "tests/illustrative/D15347-load-invalid.litmus",
  "tests/illustrative/D15347-load-rel-acq.litmus",
  "tests/illustrative/D15347-load-shoot.litmus",
  "tests/illustrative/D15347-load-valid.litmus",
  "tests/illustrative/D15347-load.litmus",
  "tests/illustrative/D15347-store-DMB.litmus",
  "tests/illustrative/D15347-store-DMBST.litmus",
  "tests/illustrative/D15347-store-DSB-ISB.litmus",
  "tests/illustrative/D15347-store-DSB.litmus",
  "tests/illustrative/D15347-store-rel.litmus",
  "tests/illustrative/D15347-store.litmus",
  "tests/illustrative/VIS01-load.litmus",
  "tests/illustrative/VIS01-store.litmus",
  "tests/illustrative/VIS7+DSB.litmus",
  "tests/illustrative/VIS7+SY.litmus",
  "tests/illustrative/VIS7+TLBI.litmus",
  "tests/illustrative/VIS7.litmus",
  "tests/illustrative/coRR-pte.litmus",
  "tests/illustrative/coRR-pte2.litmus",
  "tests/illustrative/coRR-pte3.litmus",
  "tests/illustrative/coRR-pte4.litmus",
  "tests/illustrative/ldr-invalid-pte.litmus",
  "tests/illustrative/ldr-valid-pte.litmus",
  "tests/illustrative/ldrAF0.litmus",
  "tests/illustrative/str-invalid-pte.litmus",
  "tests/illustrative/str-valid-pte.litmus",
  "tests/illustrative/strAF0.litmus",
]

campaign = []

references = []

notes = ""
