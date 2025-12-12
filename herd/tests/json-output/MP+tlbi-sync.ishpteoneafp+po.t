  $ herd7 -set-libdir ./libdir -variant vmsa -show prop -through all -showevents all -o - -output-format json fixtures/MP+tlbi-sync.ishpteoneafp+po.litmus | sed -n '/^JSONBEGIN /,$p'
  JSONBEGIN MP+tlbi-sync.ishpteoneafp+po
  {
    "schema_version": 1,
    "test": {
      "name": "MP+tlbi-sync.ishpteoneafp+po",
      "kind": "Allowed",
      "architecture": "AArch64",
      "info": [
        { "key": "Hash", "value": "8a8c5630707de4202e6a6851a0525afc" },
        { "key": "Generator", "value": "diyone.exe (version 7.58+1)" },
        { "key": "Prefetch", "value": "0:x=F,0:y=W,1:y=F,1:x=T" },
        { "key": "Com", "value": "Rf Fr" },
        {
          "key": "Orig",
          "value": "TLBI-sync.ISHdWWPteOneAFP Rfe PodRR FrePPteOneAF"
        }
      ],
      "program": [
        {
          "proc": 0,
          "function": "main",
          "instructions": [
            { "static_poi": 0, "instruction": "STR X1,[X0]" },
            { "static_poi": 1, "instruction": "LSR X5,X4,#12" },
            { "static_poi": 2, "instruction": "DSB ISH" },
            { "static_poi": 3, "instruction": "TLBI VAAE1IS,X5" },
            { "static_poi": 4, "instruction": "DSB ISH" },
            { "static_poi": 5, "instruction": "MOV W2,#6" },
            { "static_poi": 6, "instruction": "STR W2,[X3]" }
          ]
        },
        {
          "proc": 1,
          "function": "main",
          "instructions": [
            { "static_poi": 0, "instruction": "LDR W2,[X3]" },
            {
              "static_poi": 1,
              "instruction": "LDR W5,[X4]",
              "labels": [ "L00" ]
            }
          ]
        }
      ],
      "init": "0:X0=PTE(x); 0:X1=(oa:PA(x)); 0:X3=y; 0:X4=x; 1:X3=y; 1:X4=x; [x]=1; [y]=5; [PTE(x)]=(oa:PA(x), af:0);",
      "condition": "exists (1:X2=6 /\\ fault(P1:L00,x))"
    },
    "result": {
      "model": "Generic[withcatdep](Unknown)",
      "verdict": "Ok",
      "observation": "Sometimes",
      "positive": 2,
      "negative": 6,
      "candidates": 8,
      "failed_candidates": 0,
      "execution_graph_count": 2,
      "final_states": [
        { "id": "state-0", "value": "1:X2=5; ~Fault(P1:L00,x);" },
        {
          "id": "state-1",
          "value": "1:X2=5; Fault(P1:L00,x,D-MMU:AccessFlag);"
        },
        { "id": "state-2", "value": "1:X2=6; ~Fault(P1:L00,x);" },
        {
          "id": "state-3",
          "value": "1:X2=6; Fault(P1:L00,x,D-MMU:AccessFlag);"
        }
      ],
      "invoked_with_cli": [
        "-set-libdir", "./libdir", "-variant", "vmsa", "-show", "prop",
        "-through", "all", "-showevents", "all", "-o", "-", "-output-format",
        "json", "fixtures/MP+tlbi-sync.ishpteoneafp+po.litmus"
      ]
    },
    "execution_graphs": [
      {
        "id": "execution-0",
        "is_valid": true,
        "satisfies_post_condition": true,
        "final_state_id": "state-3",
        "pretty_conf": {
          "events": {
            "showevents": "all",
            "showinitwrites": true,
            "showinitrf": false,
            "showfinalrf": false,
            "showpo": true
          },
          "relations": {
            "doshow": [],
            "unshow": [],
            "showraw": [],
            "initwrites": true
          }
        },
        "events": [
          {
            "id": 0,
            "action": "W[PTE(x)]q=(oa:PA(x))",
            "action_details": {
              "direction": "W",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x))"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 1,
            "action": "R[PTE(y)]NExpq=(oa:PA(y))",
            "action_details": {
              "direction": "R",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 2,
            "action": "W[PA(y)]=6",
            "action_details": {
              "direction": "W",
              "location": "[PA(y)]",
              "size": "word",
              "value": "6"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 3,
            "action": "R[PTE(y)]NExpq=(oa:PA(y))",
            "action_details": {
              "direction": "R",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 4,
            "action": "R[PA(y)]=6",
            "action_details": {
              "direction": "R",
              "location": "[PA(y)]",
              "size": "word",
              "value": "6"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 5,
            "action": "R[PTE(x)]NExpq=(oa:PA(x), af:0)",
            "action_details": {
              "direction": "R",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x), af:0)"
            },
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 6,
            "action": "W[PTE(y)]q=(oa:PA(y))",
            "action_details": {
              "direction": "W",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 7,
            "action": "W[PTE(x)]q=(oa:PA(x), af:0)",
            "action_details": {
              "direction": "W",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x), af:0)"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 8,
            "action": "W[PA(y)]=5",
            "action_details": {
              "direction": "W",
              "location": "[PA(y)]",
              "size": "word",
              "value": "5"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 9,
            "action": "W[PA(x)]=1",
            "action_details": {
              "direction": "W",
              "location": "[PA(x)]",
              "size": "word",
              "value": "1"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 10,
            "action": "R0:X0q=PTE(x)",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "PTE(x)"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 11,
            "action": "R0:X1q=(oa:PA(x))",
            "action_details": {
              "direction": "R",
              "location": "0:X1",
              "size": "quad",
              "value": "(oa:PA(x))"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 12,
            "action": "R0:X4q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X4",
              "size": "quad",
              "value": "x"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LSR X5,X4,#12",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 13,
            "action": "W0:X5q=TLB(x)",
            "action_details": {
              "direction": "W",
              "location": "0:X5",
              "size": "quad",
              "value": "TLB(x)"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LSR X5,X4,#12",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 14,
            "action": "DSB ISH",
            "poi": 2,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DSB ISH",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 15,
            "action": "R0:X5q=TLB(x)",
            "action_details": {
              "direction": "R",
              "location": "0:X5",
              "size": "quad",
              "value": "TLB(x)"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "TLBI VAAE1IS,X5",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 16,
            "action": "TLBI(VAAE1IS,[TLB(x)])",
            "poi": 3,
            "event_labels": [ "noregs", "nobranches" ],
            "instruction": "TLBI VAAE1IS,X5",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 17,
            "action": "DSB ISH",
            "poi": 4,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DSB ISH",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 18,
            "action": "W0:X2q=6",
            "action_details": {
              "direction": "W",
              "location": "0:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 5,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#6",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 5
            }
          },
          {
            "id": 19,
            "action": "R0:X3q=y",
            "action_details": {
              "direction": "R",
              "location": "0:X3",
              "size": "quad",
              "value": "y"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 20,
            "action": "Branching(pred)(valid:1 && af:1 && db:1)",
            "poi": 6,
            "event_labels": [ "noregs" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 21,
            "action": "R0:X2q=6",
            "action_details": {
              "direction": "R",
              "location": "0:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 22,
            "action": "R1:X3q=y",
            "action_details": {
              "direction": "R",
              "location": "1:X3",
              "size": "quad",
              "value": "y"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 23,
            "action": "Branching(pred)(valid:1 && af:1)",
            "poi": 0,
            "event_labels": [ "noregs" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 24,
            "action": "W1:X2q=6",
            "action_details": {
              "direction": "W",
              "location": "1:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 25,
            "action": "R1:X4q=x",
            "action_details": {
              "direction": "R",
              "location": "1:X4",
              "size": "quad",
              "value": "x"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 26,
            "action": "Branching(pred)",
            "poi": 1,
            "event_labels": [ "noregs" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 27,
            "action": "W1:ELR_EL1q=label:\"P1:L00\"",
            "action_details": {
              "direction": "W",
              "location": "1:ELR_EL1",
              "size": "quad",
              "value": "label:\"P1:L00\""
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 28,
            "action": "ExcEntry(R,loc:x,D-MMU:AccessFlag)",
            "poi": 1,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          }
        ],
        "edges": [
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 23 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 24 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 11 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 12 },
            "target": { "type": "event", "id": 13 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 15 },
            "target": { "type": "event", "id": 16 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 21 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 25 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 20 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 12 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 14 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 16 },
            "target": { "type": "event", "id": 17 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 17 },
            "target": { "type": "event", "id": 18 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 25 }
          }
        ]
      },
      {
        "id": "execution-1",
        "is_valid": false,
        "satisfies_post_condition": true,
        "final_state_id": "state-3",
        "pretty_conf": {
          "events": {
            "showevents": "all",
            "showinitwrites": true,
            "showinitrf": false,
            "showfinalrf": false,
            "showpo": true
          },
          "relations": {
            "doshow": [],
            "unshow": [],
            "showraw": [],
            "initwrites": true
          }
        },
        "events": [
          {
            "id": 0,
            "action": "W[PTE(x)]q=(oa:PA(x))",
            "action_details": {
              "direction": "W",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x))"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 1,
            "action": "R[PTE(y)]NExpq=(oa:PA(y))",
            "action_details": {
              "direction": "R",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 2,
            "action": "W[PA(y)]=6",
            "action_details": {
              "direction": "W",
              "location": "[PA(y)]",
              "size": "word",
              "value": "6"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 3,
            "action": "R[PTE(y)]NExpq=(oa:PA(y))",
            "action_details": {
              "direction": "R",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 4,
            "action": "R[PA(y)]=6",
            "action_details": {
              "direction": "R",
              "location": "[PA(y)]",
              "size": "word",
              "value": "6"
            },
            "poi": 0,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 5,
            "action": "R[PTE(x)]NExpq=(oa:PA(x), af:0)",
            "action_details": {
              "direction": "R",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x), af:0)"
            },
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 6,
            "action": "W[PTE(y)]q=(oa:PA(y))",
            "action_details": {
              "direction": "W",
              "location": "[PTE(y)]",
              "size": "quad",
              "value": "(oa:PA(y))"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 7,
            "action": "W[PTE(x)]q=(oa:PA(x), af:0)",
            "action_details": {
              "direction": "W",
              "location": "[PTE(x)]",
              "size": "quad",
              "value": "(oa:PA(x), af:0)"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 8,
            "action": "W[PA(y)]=5",
            "action_details": {
              "direction": "W",
              "location": "[PA(y)]",
              "size": "word",
              "value": "5"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 9,
            "action": "W[PA(x)]=1",
            "action_details": {
              "direction": "W",
              "location": "[PA(x)]",
              "size": "word",
              "value": "1"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 10,
            "action": "R0:X0q=PTE(x)",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "PTE(x)"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 11,
            "action": "R0:X1q=(oa:PA(x))",
            "action_details": {
              "direction": "R",
              "location": "0:X1",
              "size": "quad",
              "value": "(oa:PA(x))"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "STR X1,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 12,
            "action": "R0:X4q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X4",
              "size": "quad",
              "value": "x"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LSR X5,X4,#12",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 13,
            "action": "W0:X5q=TLB(x)",
            "action_details": {
              "direction": "W",
              "location": "0:X5",
              "size": "quad",
              "value": "TLB(x)"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LSR X5,X4,#12",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 14,
            "action": "DSB ISH",
            "poi": 2,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DSB ISH",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 15,
            "action": "R0:X5q=TLB(x)",
            "action_details": {
              "direction": "R",
              "location": "0:X5",
              "size": "quad",
              "value": "TLB(x)"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "TLBI VAAE1IS,X5",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 16,
            "action": "TLBI(VAAE1IS,[TLB(x)])",
            "poi": 3,
            "event_labels": [ "noregs", "nobranches" ],
            "instruction": "TLBI VAAE1IS,X5",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 17,
            "action": "DSB ISH",
            "poi": 4,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DSB ISH",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 18,
            "action": "W0:X2q=6",
            "action_details": {
              "direction": "W",
              "location": "0:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 5,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#6",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 5
            }
          },
          {
            "id": 19,
            "action": "R0:X3q=y",
            "action_details": {
              "direction": "R",
              "location": "0:X3",
              "size": "quad",
              "value": "y"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 20,
            "action": "Branching(pred)(valid:1 && af:1 && db:1)",
            "poi": 6,
            "event_labels": [ "noregs" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 21,
            "action": "R0:X2q=6",
            "action_details": {
              "direction": "R",
              "location": "0:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 22,
            "action": "R1:X3q=y",
            "action_details": {
              "direction": "R",
              "location": "1:X3",
              "size": "quad",
              "value": "y"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 23,
            "action": "Branching(pred)(valid:1 && af:1)",
            "poi": 0,
            "event_labels": [ "noregs" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 24,
            "action": "W1:X2q=6",
            "action_details": {
              "direction": "W",
              "location": "1:X2",
              "size": "quad",
              "value": "6"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 25,
            "action": "R1:X4q=x",
            "action_details": {
              "direction": "R",
              "location": "1:X4",
              "size": "quad",
              "value": "x"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 26,
            "action": "Branching(pred)",
            "poi": 1,
            "event_labels": [ "noregs" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 27,
            "action": "W1:ELR_EL1q=label:\"P1:L00\"",
            "action_details": {
              "direction": "W",
              "location": "1:ELR_EL1",
              "size": "quad",
              "value": "label:\"P1:L00\""
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          },
          {
            "id": 28,
            "action": "ExcEntry(R,loc:x,D-MMU:AccessFlag)",
            "poi": 1,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "LDR W5,[X4]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1,
              "instruction_labels": [ "L00" ]
            }
          }
        ],
        "edges": [
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 23 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 24 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 11 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 12 },
            "target": { "type": "event", "id": 13 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 15 },
            "target": { "type": "event", "id": 16 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 21 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 25 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 20 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 12 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 14 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 16 },
            "target": { "type": "event", "id": 17 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 17 },
            "target": { "type": "event", "id": 18 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 25 }
          }
        ]
      }
    ]
  }
  JSONEND MP+tlbi-sync.ishpteoneafp+po
