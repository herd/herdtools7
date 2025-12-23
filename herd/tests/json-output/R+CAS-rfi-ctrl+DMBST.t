  $ herd7 -show prop -through all -showevents all -o - -output-format json fixtures/R+CAS-rfi-ctrl+DMBST.litmus
  
  JSONBEGIN R+CAS-rfi-ctrl+DMBST
  [
    {
      "events": [
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            },
            "is_atomic": true
          },
          "eiid": 0,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            },
            "is_atomic": true
          },
          "eiid": 1,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 2,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 3,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 4,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 5,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "0" }
            }
          },
          "eiid": 6,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "0" }
            }
          },
          "eiid": 7,
          "iiid": "init"
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 8,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "MOV W1,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 9,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "MOV W2,#2" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 10,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 11,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 12,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 13,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 14,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 15,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 16,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 17,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "CBNZ W3,.+4" }
        },
        {
          "act": { "type": "commit", "commit": "bcc" },
          "eiid": 18,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "CBNZ W3,.+4" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 19,
          "iiid": { "proc": 0, "poi": 5, "inst_pretty": "MOV W4,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 20,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 21,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 22,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "MOV W0,#2" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 23,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 24,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DMB ST" },
          "eiid": 25,
          "iiid": { "proc": 1, "poi": 2, "inst_pretty": "DMB ST" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 26,
          "iiid": { "proc": 1, "poi": 3, "inst_pretty": "MOV W2,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 27,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 28,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        }
      ],
      "iico_data": [
        { "src": { "eiid": 0 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 2 }, "tgt": { "eiid": 16 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 11 }, "tgt": { "eiid": 13 } },
        { "src": { "eiid": 11 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 12 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 15 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 17 }, "tgt": { "eiid": 18 } },
        { "src": { "eiid": 20 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 21 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 23 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 24 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 27 }, "tgt": { "eiid": 5 } },
        { "src": { "eiid": 28 }, "tgt": { "eiid": 5 } }
      ],
      "iico_ctrl": [
        { "src": { "eiid": 14 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 14 }, "tgt": { "eiid": 13 } }
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
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" }
          },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 9 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" }
          },
          "tgt": { "type": "store", "event": { "eiid": 16 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" }
          },
          "tgt": { "type": "store", "event": { "eiid": 19 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" }
          },
          "tgt": { "type": "store", "event": { "eiid": 22 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 26 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "global", "global_pretty": "x" }
          },
          "tgt": { "type": "store", "event": { "eiid": 1 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "global", "global_pretty": "y" }
          },
          "tgt": { "type": "store", "event": { "eiid": 4 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 0 } },
          "tgt": { "type": "store", "event": { "eiid": 5 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 2 } },
          "tgt": { "type": "store", "event": { "eiid": 1 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 10 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 11 } },
          "tgt": { "type": "store", "event": { "eiid": 8 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 12 } },
          "tgt": { "type": "store", "event": { "eiid": 9 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 15 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 17 } },
          "tgt": { "type": "store", "event": { "eiid": 16 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 20 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 21 } },
          "tgt": { "type": "store", "event": { "eiid": 19 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 23 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 24 } },
          "tgt": { "type": "store", "event": { "eiid": 22 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 27 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 28 } },
          "tgt": { "type": "store", "event": { "eiid": 26 } }
        }
      ],
      "viewed_before": {
        "rf-reg": [
          { "src": { "eiid": 8 }, "tgt": { "eiid": 11 } },
          { "src": { "eiid": 9 }, "tgt": { "eiid": 12 } },
          { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
          { "src": { "eiid": 19 }, "tgt": { "eiid": 21 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 24 } },
          { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
        ],
        "rf": [
          { "src": { "eiid": 1 }, "tgt": { "eiid": 2 } },
          { "src": { "eiid": 5 }, "tgt": { "eiid": 0 } }
        ],
        "isb": [],
        "dsb.sy": [],
        "dsb.ld": [],
        "dmb.sy": [],
        "dmb.st": [
          { "src": { "eiid": 4 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 28 } }
        ],
        "dmb.ld": [],
        "data": [],
        "ctrlisb": [],
        "ctrl": [
          { "src": { "eiid": 2 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 19 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 20 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 21 } }
        ],
        "ca": [
          { "src": { "eiid": 0 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 3 }, "tgt": { "eiid": 4 } },
          { "src": { "eiid": 5 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 5 } }
        ],
        "addr": []
      },
      "visible_po": [
        { "src": { "eiid": 1 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 4 }, "tgt": { "eiid": 25 } },
        { "src": { "eiid": 8 }, "tgt": { "eiid": 9 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 10 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 11 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 12 } },
        { "src": { "eiid": 13 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 19 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 20 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 21 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 23 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 24 } },
        { "src": { "eiid": 25 }, "tgt": { "eiid": 26 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 27 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
      ]
    },
    {
      "events": [
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            },
            "is_atomic": true
          },
          "eiid": 0,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            },
            "is_atomic": true
          },
          "eiid": 1,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "R",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 2,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 3,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 4,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 5,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "y" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "0" }
            }
          },
          "eiid": 6,
          "iiid": "init"
        },
        {
          "act": {
            "type": "mem",
            "dir": "W",
            "loc": { "type": "global", "global_pretty": "x" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "0" }
            }
          },
          "eiid": 7,
          "iiid": "init"
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 8,
          "iiid": { "proc": 0, "poi": 0, "inst_pretty": "MOV W1,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 9,
          "iiid": { "proc": 0, "poi": 1, "inst_pretty": "MOV W2,#2" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 10,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 11,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 12,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 13,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": { "type": "commit", "commit": "pred" },
          "eiid": 14,
          "iiid": { "proc": 0, "poi": 2, "inst_pretty": "CAS W1,W2,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 15,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 16,
          "iiid": { "proc": 0, "poi": 3, "inst_pretty": "LDR W3,[X0]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 17,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "CBNZ W3,.+4" }
        },
        {
          "act": { "type": "commit", "commit": "bcc" },
          "eiid": 18,
          "iiid": { "proc": 0, "poi": 4, "inst_pretty": "CBNZ W3,.+4" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 19,
          "iiid": { "proc": 0, "poi": 5, "inst_pretty": "MOV W4,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X5" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 20,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 21,
          "iiid": { "proc": 0, "poi": 6, "inst_pretty": "STR W4,[X5]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 22,
          "iiid": { "proc": 1, "poi": 0, "inst_pretty": "MOV W0,#2" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X1" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "y" }
            }
          },
          "eiid": 23,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "2" }
            }
          },
          "eiid": 24,
          "iiid": { "proc": 1, "poi": 1, "inst_pretty": "STR W0,[X1]" }
        },
        {
          "act": { "type": "barrier", "barrier_pretty": "DMB ST" },
          "eiid": 25,
          "iiid": { "proc": 1, "poi": 2, "inst_pretty": "DMB ST" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "W",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 26,
          "iiid": { "proc": 1, "poi": 3, "inst_pretty": "MOV W2,#1" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X3" },
            "value": {
              "type": "val",
              "val": { "type": "symbolic", "symbol_pretty": "x" }
            }
          },
          "eiid": 27,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        },
        {
          "act": {
            "type": "reg",
            "dir": "R",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" },
            "value": {
              "type": "val",
              "val": { "type": "concrete", "scalar_pretty": "1" }
            }
          },
          "eiid": 28,
          "iiid": { "proc": 1, "poi": 4, "inst_pretty": "STR W2,[X3]" }
        }
      ],
      "iico_data": [
        { "src": { "eiid": 0 }, "tgt": { "eiid": 13 } },
        { "src": { "eiid": 0 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 2 }, "tgt": { "eiid": 16 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 0 } },
        { "src": { "eiid": 10 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 11 }, "tgt": { "eiid": 14 } },
        { "src": { "eiid": 12 }, "tgt": { "eiid": 1 } },
        { "src": { "eiid": 15 }, "tgt": { "eiid": 2 } },
        { "src": { "eiid": 17 }, "tgt": { "eiid": 18 } },
        { "src": { "eiid": 20 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 21 }, "tgt": { "eiid": 3 } },
        { "src": { "eiid": 23 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 24 }, "tgt": { "eiid": 4 } },
        { "src": { "eiid": 27 }, "tgt": { "eiid": 5 } },
        { "src": { "eiid": 28 }, "tgt": { "eiid": 5 } }
      ],
      "iico_ctrl": [ { "src": { "eiid": 14 }, "tgt": { "eiid": 1 } } ],
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
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X1" }
          },
          "tgt": { "type": "store", "event": { "eiid": 13 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 9 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X3" }
          },
          "tgt": { "type": "store", "event": { "eiid": 16 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 0, "reg_pretty": "X4" }
          },
          "tgt": { "type": "store", "event": { "eiid": 19 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X0" }
          },
          "tgt": { "type": "store", "event": { "eiid": 22 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "reg", "proc": 1, "reg_pretty": "X2" }
          },
          "tgt": { "type": "store", "event": { "eiid": 26 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "global", "global_pretty": "x" }
          },
          "tgt": { "type": "store", "event": { "eiid": 1 } }
        },
        {
          "src": {
            "type": "final",
            "loc": { "type": "global", "global_pretty": "y" }
          },
          "tgt": { "type": "store", "event": { "eiid": 4 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 0 } },
          "tgt": { "type": "store", "event": { "eiid": 5 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 2 } },
          "tgt": { "type": "store", "event": { "eiid": 1 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 10 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 11 } },
          "tgt": { "type": "store", "event": { "eiid": 8 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 12 } },
          "tgt": { "type": "store", "event": { "eiid": 9 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 15 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 17 } },
          "tgt": { "type": "store", "event": { "eiid": 16 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 20 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 21 } },
          "tgt": { "type": "store", "event": { "eiid": 19 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 23 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 24 } },
          "tgt": { "type": "store", "event": { "eiid": 22 } }
        },
        {
          "src": { "type": "load", "event": { "eiid": 27 } },
          "tgt": { "type": "init" }
        },
        {
          "src": { "type": "load", "event": { "eiid": 28 } },
          "tgt": { "type": "store", "event": { "eiid": 26 } }
        }
      ],
      "viewed_before": {
        "rf-reg": [
          { "src": { "eiid": 8 }, "tgt": { "eiid": 11 } },
          { "src": { "eiid": 9 }, "tgt": { "eiid": 12 } },
          { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
          { "src": { "eiid": 19 }, "tgt": { "eiid": 21 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 24 } },
          { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
        ],
        "rf": [
          { "src": { "eiid": 1 }, "tgt": { "eiid": 2 } },
          { "src": { "eiid": 5 }, "tgt": { "eiid": 0 } }
        ],
        "isb": [],
        "dsb.sy": [],
        "dsb.ld": [],
        "dmb.sy": [],
        "dmb.st": [
          { "src": { "eiid": 4 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 4 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 22 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 23 }, "tgt": { "eiid": 28 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 5 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 26 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 27 } },
          { "src": { "eiid": 24 }, "tgt": { "eiid": 28 } }
        ],
        "dmb.ld": [],
        "data": [],
        "ctrlisb": [],
        "ctrl": [
          { "src": { "eiid": 2 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 19 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 20 } },
          { "src": { "eiid": 2 }, "tgt": { "eiid": 21 } }
        ],
        "ca": [
          { "src": { "eiid": 0 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 3 }, "tgt": { "eiid": 4 } },
          { "src": { "eiid": 5 }, "tgt": { "eiid": 1 } },
          { "src": { "eiid": 6 }, "tgt": { "eiid": 3 } },
          { "src": { "eiid": 7 }, "tgt": { "eiid": 5 } }
        ],
        "addr": []
      },
      "visible_po": [
        { "src": { "eiid": 1 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 4 }, "tgt": { "eiid": 25 } },
        { "src": { "eiid": 8 }, "tgt": { "eiid": 9 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 10 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 11 } },
        { "src": { "eiid": 9 }, "tgt": { "eiid": 12 } },
        { "src": { "eiid": 13 }, "tgt": { "eiid": 15 } },
        { "src": { "eiid": 16 }, "tgt": { "eiid": 17 } },
        { "src": { "eiid": 18 }, "tgt": { "eiid": 19 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 20 } },
        { "src": { "eiid": 19 }, "tgt": { "eiid": 21 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 23 } },
        { "src": { "eiid": 22 }, "tgt": { "eiid": 24 } },
        { "src": { "eiid": 25 }, "tgt": { "eiid": 26 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 27 } },
        { "src": { "eiid": 26 }, "tgt": { "eiid": 28 } }
      ]
    }
  ]
  
  JSONEND R+CAS-rfi-ctrl+DMBST
  Test R+CAS-rfi-ctrl+DMBST Allowed
  States 20
  0:X1=0; 0:X3=0; [x]=0; [y]=1;
  0:X1=0; 0:X3=0; [x]=0; [y]=2;
  0:X1=0; 0:X3=0; [x]=1; [y]=1;
  0:X1=0; 0:X3=0; [x]=1; [y]=2;
  0:X1=0; 0:X3=1; [x]=0; [y]=1;
  0:X1=0; 0:X3=1; [x]=0; [y]=2;
  0:X1=0; 0:X3=1; [x]=1; [y]=1;
  0:X1=0; 0:X3=1; [x]=1; [y]=2;
  0:X1=1; 0:X3=0; [x]=1; [y]=1;
  0:X1=1; 0:X3=0; [x]=1; [y]=2;
  0:X1=1; 0:X3=0; [x]=2; [y]=1;
  0:X1=1; 0:X3=0; [x]=2; [y]=2;
  0:X1=1; 0:X3=1; [x]=1; [y]=1;
  0:X1=1; 0:X3=1; [x]=1; [y]=2;
  0:X1=1; 0:X3=1; [x]=2; [y]=1;
  0:X1=1; 0:X3=1; [x]=2; [y]=2;
  0:X1=1; 0:X3=2; [x]=1; [y]=1;
  0:X1=1; 0:X3=2; [x]=1; [y]=2;
  0:X1=1; 0:X3=2; [x]=2; [y]=1;
  0:X1=1; 0:X3=2; [x]=2; [y]=2;
  Ok
  Witnesses
  Positive: 2 Negative: 38
  Condition exists ([x]=2 /\ [y]=2 /\ 0:X1=1 /\ 0:X3=2)
  Observation R+CAS-rfi-ctrl+DMBST Sometimes 2 38
  Time R+CAS-rfi-ctrl+DMBST 0.05
  Hash=b589428dc0b391a69fd4b31fe524a5d0
  

