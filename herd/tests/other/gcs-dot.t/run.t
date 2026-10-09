  $ run_herd () {
  >   variant="$1"
  >   test="$2"
  >   herd7 -set-libdir ../libdir -variant "$variant" "$test"   \
  >     -show all -showevents all -showinitwrites false -o . > /dev/null && \
  >   grep eiid "${test%.litmus}.dot"
  > }

Check GCSPUSHM dot output under shadowstack.

  $ run_herd shadowstack GCSPUSHM.litmus
  eiid0 [label="a: W[x]GCSq=4\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid3 [label="d: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid4 [label="e: R0:X0q=4 (data)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];

Check GCSPUSHM dot output under shadowstack,vmsa.

  $ run_herd shadowstack,vmsa GCSPUSHM.litmus
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid1 [label="b: W[PA(x)]GCSq=4\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid6 [label="g: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid7 [label="h: R0:X0q=4 (data)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];


Check GCSPOPM dot output under shadowstack.

  $ run_herd shadowstack GCSPOPM.litmus
  eiid0 [label="a: R[x]GCSq=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid3 [label="d: Branching(pred)(PCAligned)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid4 [label="e: W0:X1q=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid5 [label="f: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid3 -> eiid5 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check GCSPOPM dot output under shadowstack,vmsa.

  $ run_herd shadowstack,vmsa GCSPOPM.litmus
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]GCSq=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(PCAligned)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid7 [label="h: W0:X1q=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid8 [label="i: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check BLR dot output under shadowstack.

  $ run_herd shadowstack BLR.litmus
  eiid0 [label="a: W[x]GCSq=100004\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid3 [label="d: R0:X0q=label:\"P0:L1\"\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid4 [label="e: Branching(bcc)\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid5 [label="f: W0:X30q=100004\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid6 [label="g: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_data", color="black", fontcolor="black"];

Check BLR dot output under shadowstack,vmsa.

  $ run_herd shadowstack,vmsa BLR.litmus
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid1 [label="b: W[PA(x)]GCSq=100004\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid6 [label="g: R0:X0q=label:\"P0:L1\"\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid7 [label="h: Branching(bcc)\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid8 [label="i: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid9 [label="j: W0:X30q=100004\lproc:P0 poi:0\lBLR X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check RET dot output under shadowstack.

  $ run_herd shadowstack RET.litmus
  eiid0 [label="a: R[x]GCSq=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid3 [label="d: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid4 [label="e: Branching(pred)(target==0:X29)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid5 [label="f: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: Branching(bcc)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid4 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid0 [label="a: R[x]GCSq=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid3 [label="d: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid4 [label="e: Branching(pred)(target==0:X29)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid5 [label="f: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: Branching(bcc)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid4 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check failed RET dot output under shadowstack.

  $ run_herd shadowstack RET-fault.litmus
  eiid0 [label="a: R[x]GCSq=label:\"P0:L0\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid3 [label="d: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid4 [label="e: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(GCSCheck PRET)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: W0:ELR_EL1q=100000\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(R,GCS:PRET)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check RET dot output under shadowstack,vmsa.

  $ run_herd shadowstack,vmsa RET.litmus
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]GCSq=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(target==0:X29)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid8 [label="i: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid9 [label="j: Branching(bcc)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]GCSq=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(target==0:X29)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid8 [label="i: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid9 [label="j: Branching(bcc)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];

Check failed RET dot output under shadowstack,vmsa.

  $ run_herd shadowstack,vmsa RET-fault.litmus
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]GCSq=label:\"P0:L0\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid6 [label="g: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid8 [label="i: R0:X29q=label:\"P0:L1\"\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(GCSCheck PRET)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid10 [label="k: W0:ELR_EL1q=100000\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid11 [label="l: ExcEntry(R,GCS:PRET)\lproc:P0 poi:0\lRET X29", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid10 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
