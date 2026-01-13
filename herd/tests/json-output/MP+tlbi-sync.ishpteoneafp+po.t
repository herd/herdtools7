  $ herd7 -set-libdir ./libdir -variant vmsa -show prop -through all -showevents all -o - -output-format json fixtures/MP+tlbi-sync.ishpteoneafp+po.litmus | sed '/^JSONEND /q'
  
  JSONBEGIN MP+tlbi-sync.ishpteoneafp+po
  [
    {
      "events": [
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x))",
                "as_physical": "x"
              }
            }
          },
          "eiid": 0,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            },
            "is_implicit": true
          },
          "eiid": 1,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 2,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            },
            "is_implicit": true
          },
          "eiid": 3,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 4,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x), af:0)",
                "as_physical": "x"
              }
            },
            "is_implicit": true
          },
          "eiid": 5,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            }
          },
          "eiid": 6,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x), af:0)",
                "as_physical": "x"
              }
            }
          },
          "eiid": 7,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "5" }
            }
          },
          "eiid": 8,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(x)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 9,
          "iiid": "init"
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "PTE(x)" }
            }
          },
          "eiid": 10,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x))",
                "as_physical": "x"
              }
            }
          },
          "eiid": 11,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 12,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "LSR X5,X4,#12" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "TLB(x)" }
            }
          },
          "eiid": 13,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "LSR X5,X4,#12" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DSB ISH" },
          "eiid": 14,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "DSB ISH" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "TLB(x)" }
            }
          },
          "eiid": 15,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "TLBI VAAE1IS,X5" }
        },
        {
          "act": null,
          "eiid": 16,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "TLBI VAAE1IS,X5" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DSB ISH" },
          "eiid": 17,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "DSB ISH" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 18,
          "iiid": { "proc": 0, "poi": 5, "inst_pretty": "MOV W2,#6" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 19,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 20,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 21,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 22,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 23,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 24,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 25,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 26,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "ELR_EL1" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "1:L00" }
            }
          },
          "eiid": 27,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": { "type": "fauult", "fault": null },
          "eiid": 28,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        }
      ],
      "iico_data": [
        { "src": { "eiid": 1 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 1 }, "tgt": { "eiid": 20 } },
        { "src": { "eiid": 3 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 3 }, "tgt": { "eiid": 23 } },
        { "src": { "eiid": 4 }, "tgt": { "eiid": 24 } },
        { "src": { "eiid": 5 }, "tgt": { "eiid": 26 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 11 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 12 }, "tgt": { "eiid": 13 } },
        { "src": { "eiid": 15 }, "tgt": { "eiid": 16 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 21 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 25 }, "tgt": { "eiid": 5 } }
      ],
      "iico_ctrl": [
        { "src": { "eiid": 20 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 23 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 27 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
      ],
      "speculated": [
        { "eiid": 0 },
        { "eiid": 1 },
        { "eiid": 2 },
        { "eiid": 3 },
        { "eiid": 4 },
        { "eiid": 5 },
        { "eiid": 6 },
        { "eiid": 7 },
        { "eiid": 8 },
        { "eiid": 9 },
        { "eiid": 10 },
        { "eiid": 11 },
        { "eiid": 12 },
        { "eiid": 13 },
        { "eiid": 14 },
        { "eiid": 15 },
        { "eiid": 16 },
        { "eiid": 17 },
        { "eiid": 18 },
        { "eiid": 19 },
        { "eiid": 20 },
        { "eiid": 21 },
        { "eiid": 22 },
        { "eiid": 23 },
        { "eiid": 24 },
        { "eiid": 25 },
        { "eiid": 26 },
        { "eiid": 27 },
        { "eiid": 28 }
      ],
      "rfmap": [
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 18 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" }
          },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 24 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "ELR_EL1" }
          },
          "tgt": { "type": "store", "event": { "eiid": 27 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 1 } },
          "tgt": { "type": "store", "event": { "eiid": 6 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 3 } },
          "tgt": { "type": "store", "event": { "eiid": 6 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 4 } },
          "tgt": { "type": "store", "event": { "eiid": 2 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 5 } },
          "tgt": { "type": "store", "event": { "eiid": 7 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 10 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 11 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 12 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 15 } },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 19 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 21 } },
          "tgt": { "type": "store", "event": { "eiid": 18 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 22 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 25 } },
          "tgt": { "type": "init" }
        }
      ],
      "viewed_before": {
        "rf-reg": [
          { "src": { "eiid": 13 }, "tgt": { "eiid": 15 } },
          { "src": { "eiid": 18 }, "tgt": { "eiid": 21 } }
        ],
        "rf": [
          { "src": { "eiid": 2 }, "tgt": { "eiid": 4 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 5 } }
        ],
        "isb": [],
        "dsb.sy": [],
        "dsb.ld": [],
        "dmb.sy": [],
        "dmb.st": [],
        "dmb.ld": [],
        "data": [],
        "ctrlisb": [],
        "ctrl": [],
        "ca": [
          { "src": { "eiid": 5 }, "tgt": { "eiid": 0 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 0 } },
          { "src": { "eiid": 8 }, "tgt": { "eiid": 2 } }
        ],
        "addr": []
      },
      "visible_po": [
        { "src": { "eiid": 0 }, "tgt": { "eiid": 12 } },
        { "src": { "eiid": 13 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 14 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
        { "src": { "eiid": 17 }, "tgt": { "eiid": 18 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 19 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 21 } },
        { "src": { "eiid": 24 }, "tgt": { "eiid": 25 } }
      ]
    },
    {
      "events": [
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x))",
                "as_physical": "x"
              }
            }
          },
          "eiid": 0,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            },
            "is_implicit": true
          },
          "eiid": 1,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 2,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            },
            "is_implicit": true
          },
          "eiid": 3,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 4,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x), af:0)",
                "as_physical": "x"
              }
            },
            "is_implicit": true
          },
          "eiid": 5,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(y)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(y))",
                "as_physical": "y"
              }
            }
          },
          "eiid": 6,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PTE(x)" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x), af:0)",
                "as_physical": "x"
              }
            }
          },
          "eiid": 7,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(y)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "5" }
            }
          },
          "eiid": 8,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "PA(x)" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 9,
          "iiid": "init"
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "PTE(x)" }
            }
          },
          "eiid": 10,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": {
                "type": "pteval",
                "pteval_pretty": "(oa:PA(x))",
                "as_physical": "x"
              }
            }
          },
          "eiid": 11,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "STR X1,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 12,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "LSR X5,X4,#12" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "TLB(x)" }
            }
          },
          "eiid": 13,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "LSR X5,X4,#12" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DSB ISH" },
          "eiid": 14,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "DSB ISH" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "TLB(x)" }
            }
          },
          "eiid": 15,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "TLBI VAAE1IS,X5" }
        },
        {
          "act": null,
          "eiid": 16,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "TLBI VAAE1IS,X5" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DSB ISH" },
          "eiid": 17,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "DSB ISH" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 18,
          "iiid": { "proc": 0, "poi": 5, "inst_pretty": "MOV W2,#6" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 19,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 20,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 21,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 22,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 23,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "6" }
            }
          },
          "eiid": 24,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "LDR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 25,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 26,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "ELR_EL1" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "1:L00" }
            }
          },
          "eiid": 27,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        },
        {
          "act": { "type": "fauult", "fault": null },
          "eiid": 28,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "LDR W5,[X4]" }
        }
      ],
      "iico_data": [
        { "src": { "eiid": 1 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 1 }, "tgt": { "eiid": 20 } },
        { "src": { "eiid": 3 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 3 }, "tgt": { "eiid": 23 } },
        { "src": { "eiid": 4 }, "tgt": { "eiid": 24 } },
        { "src": { "eiid": 5 }, "tgt": { "eiid": 26 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 11 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 12 }, "tgt": { "eiid": 13 } },
        { "src": { "eiid": 15 }, "tgt": { "eiid": 16 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 21 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 25 }, "tgt": { "eiid": 5 } }
      ],
      "iico_ctrl": [
        { "src": { "eiid": 20 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 23 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 27 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
      ],
      "speculated": [
        { "eiid": 0 },
        { "eiid": 1 },
        { "eiid": 2 },
        { "eiid": 3 },
        { "eiid": 4 },
        { "eiid": 5 },
        { "eiid": 6 },
        { "eiid": 7 },
        { "eiid": 8 },
        { "eiid": 9 },
        { "eiid": 10 },
        { "eiid": 11 },
        { "eiid": 12 },
        { "eiid": 13 },
        { "eiid": 14 },
        { "eiid": 15 },
        { "eiid": 16 },
        { "eiid": 17 },
        { "eiid": 18 },
        { "eiid": 19 },
        { "eiid": 20 },
        { "eiid": 21 },
        { "eiid": 22 },
        { "eiid": 23 },
        { "eiid": 24 },
        { "eiid": 25 },
        { "eiid": 26 },
        { "eiid": 27 },
        { "eiid": 28 }
      ],
      "rfmap": [
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 18 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" }
          },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 24 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "ELR_EL1" }
          },
          "tgt": { "type": "store", "event": { "eiid": 27 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 1 } },
          "tgt": { "type": "store", "event": { "eiid": 6 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 3 } },
          "tgt": { "type": "store", "event": { "eiid": 6 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 4 } },
          "tgt": { "type": "store", "event": { "eiid": 2 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 5 } },
          "tgt": { "type": "store", "event": { "eiid": 7 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 10 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 11 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 12 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 15 } },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 19 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 21 } },
          "tgt": { "type": "store", "event": { "eiid": 18 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 22 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 25 } },
          "tgt": { "type": "init" }
        }
      ],
      "viewed_before": {
        "rf-reg": [
          { "src": { "eiid": 13 }, "tgt": { "eiid": 15 } },
          { "src": { "eiid": 18 }, "tgt": { "eiid": 21 } }
        ],
        "rf": [
          { "src": { "eiid": 2 }, "tgt": { "eiid": 4 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 5 } }
        ],
        "isb": [],
        "dsb.sy": [],
        "dsb.ld": [],
        "dmb.sy": [],
        "dmb.st": [],
        "dmb.ld": [],
        "data": [],
        "ctrlisb": [],
        "ctrl": [],
        "ca": [
          { "src": { "eiid": 5 }, "tgt": { "eiid": 0 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 0 } },
          { "src": { "eiid": 8 }, "tgt": { "eiid": 2 } }
        ],
        "addr": []
      },
      "visible_po": [
        { "src": { "eiid": 0 }, "tgt": { "eiid": 12 } },
        { "src": { "eiid": 13 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 14 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
        { "src": { "eiid": 17 }, "tgt": { "eiid": 18 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 19 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 21 } },
        { "src": { "eiid": 24 }, "tgt": { "eiid": 25 } }
      ]
    }
  ]
  
  JSONEND MP+tlbi-sync.ishpteoneafp+po
