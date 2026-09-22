Successful switch

  $ herd7 -set-libdir ../libdir -show all -showevents all -showinitwrites false -o . GCSSS2-success.litmus > /dev/null
  $ grep eiid GCSSS2-success.dot
  eiid0 [label="a: R[x]AcqGCSq=y+13\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid1 [label="b: W[y]RelGCSq=y+1\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(InProgress([x]))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid6 [label="g: W0:X0q=y\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];

Successful switch under kvm.

  $ herd7 -set-libdir ../libdir -show all -showevents all -showinitwrites false -o . -variant kvm -suffix .kvm GCSSS2-success.litmus > /dev/null
  $ grep eiid GCSSS2-success.kvm.dot
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]AcqGCSq=y+13\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid2 [label="c: R[PTE(y)]NExpq=(oa:PA(y))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid3 [label="d: W[PA(y)]RelGCSq=y+1\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid8 [label="i: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(InProgress([S9]))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid12 [label="m: W0:X0q=y\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid13 [label="n: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid13 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid13 [label="iico_ctrl", color="grey", fontcolor="grey"];

Translation fault on read of incoming stack.

  $ herd7 -set-libdir ../libdir -show all -showevents all -showinitwrites false -o . GCSSS2-read-translation-fault.litmus > /dev/null
  $ grep eiid GCSSS2-read-translation-fault.dot
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), valid:0)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid5 [label="f: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(R,loc:x,D-MMU:Translation)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];

Translation fault on write to outgoing stack.

  $ herd7 -set-libdir ../libdir -show all -showevents all -showinitwrites false -o . GCSSS2-write-translation-fault.litmus > /dev/null
  $ grep eiid GCSSS2-write-translation-fault.dot
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]AcqGCSq=y+13\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid2 [label="c: R[PTE(y)]NExpq=(oa:PA(y), valid:0)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid7 [label="h: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(InProgress([S9]))\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid11 [label="l: W0:ELR_EL1q=100000\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid12 [label="m: ExcEntry(W,loc:y,D-MMU:Translation)\lproc:P0 poi:0\lGCSSS2 X0", shape="box", color="blue"];
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
