  $ run_herd () {
  >   variant="$1"
  >   test="$2"
  >   herd7 -exit true -set-libdir ../libdir -variant "$variant" "$test" \
  >     -show all -showevents all -showinitwrites false -o . > /dev/null &&
  >   grep -E '^digraph |eiid' "${test%.litmus}.dot"
  > }

Successful GCSSS1, no VMSA.

  $ run_herd shadowstack GCSSS1-success.litmus
  digraph G {
  eiid0 [label="a: R[y]*GCSq=y+1\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid1 [label="b: W[y]*GCSq=x+5\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid4 [label="e: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: R0:GCSPR_EL1q=x (data)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:GCSPR_EL1q=y\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid7 [label="iico_order", color="darkgrey", fontcolor="darkgrey"];
  eiid0 -> eiid1 [label="ca", color="blue", fontcolor="blue"];

Successful GCSSS1 under VMSA.

  $ run_herd shadowstack,vmsa GCSSS1-success.litmus
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid1 [label="b: R[PA(y)]*GCSq=y+1\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid2 [label="c: W[PA(y)]*GCSq=x+5\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid10 [label="k: R0:GCSPR_EL1q=x (data)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid11 [label="l: W0:GCSPR_EL1q=y\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid10 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid11 [label="iico_order", color="darkgrey", fontcolor="darkgrey"];
  eiid1 -> eiid2 [label="ca", color="blue", fontcolor="blue"];

MMU fault on access to the incoming stack.

  $ run_herd shadowstack,vmsa GCSSS1-translation-fault.litmus
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y), valid:0)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(W,loc:y,D-MMU:Translation)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y), valid:0)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(W,loc:y,D-MMU:Translation)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y), valid:0)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(W,loc:y,D-MMU:Translation)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];

SS1 Data Check exception, no VMSA, both with and without writeback.

  $ run_herd shadowstack GCSSS1-ss1-fault.litmus
  digraph G {
  eiid0 [label="a: R[y]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid1 [label="b: W[y]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid4 [label="e: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(R,GCS:SS1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid0 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  digraph G {
  eiid0 [label="a: R[y]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid3 [label="d: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid4 [label="e: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid5 [label="f: Empty\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(R,GCS:SS1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid4 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid4 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];

SS1 Data Check exception, with VMSA, both with and without writeback.

  $ run_herd shadowstack,vmsa GCSSS1-ss1-fault.litmus
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid1 [label="b: R[PA(y)]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid2 [label="c: W[PA(y)]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid10 [label="k: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid11 [label="l: ExcEntry(R,GCS:SS1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid10 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid1 -> eiid2 [label="ca", color="blue", fontcolor="blue"];
  digraph G {
  eiid0 [label="a: R[PTE(y)]NExpq=(oa:PA(y))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid1 [label="b: R[PA(y)]*GCSq=0\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid6 [label="g: R0:X0q=y (addr)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(Valid([y]))\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid9 [label="j: Empty\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid10 [label="k: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid11 [label="l: ExcEntry(R,GCS:SS1)\lproc:P0 poi:0\lGCSSS1 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid10 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
