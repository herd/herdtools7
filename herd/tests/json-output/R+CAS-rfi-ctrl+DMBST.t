  $ herd7 -set-libdir ./libdir -show prop -through all -showevents all -o - -output-format json fixtures/R+CAS-rfi-ctrl+DMBST.litmus | sed -n '/^JSONBEGIN /,$p'
  JSONBEGIN R+CAS-rfi-ctrl+DMBST
  {
    "schema_version": 1,
    "test": {
      "name": "R+CAS-rfi-ctrl+DMBST",
      "kind": "Allowed",
      "architecture": "AArch64",
      "info": [
        { "key": "Hash", "value": "b589428dc0b391a69fd4b31fe524a5d0" }
      ],
      "program": [
        {
          "proc": 0,
          "function": "main",
          "instructions": [
            { "static_poi": 0, "instruction": "MOV W1,#1" },
            { "static_poi": 1, "instruction": "MOV W2,#2" },
            { "static_poi": 2, "instruction": "CAS W1,W2,[X0]" },
            { "static_poi": 3, "instruction": "LDR W3,[X0]" },
            { "static_poi": 4, "instruction": "CBNZ W3,LC00" },
            {
              "static_poi": 5,
              "instruction": "MOV W4,#1",
              "labels": [ "LC00" ]
            },
            { "static_poi": 6, "instruction": "STR W4,[X5]" }
          ]
        },
        {
          "proc": 1,
          "function": "main",
          "instructions": [
            { "static_poi": 0, "instruction": "MOV W0,#2" },
            { "static_poi": 1, "instruction": "STR W0,[X1]" },
            { "static_poi": 2, "instruction": "DMB ST" },
            { "static_poi": 3, "instruction": "MOV W2,#1" },
            { "static_poi": 4, "instruction": "STR W2,[X3]" }
          ]
        }
      ],
      "init": "0:X0=x; 0:X5=y; 1:X1=y; 1:X3=x;",
      "condition": "exists ([x]=2 /\\ [y]=2 /\\ 0:X1=1 /\\ 0:X3=2)"
    },
    "result": {
      "model": "Generic[withcatdep](Unknown)",
      "verdict": "Ok",
      "observation": "Sometimes",
      "positive": 2,
      "negative": 38,
      "candidates": 40,
      "failed_candidates": 0,
      "execution_graph_count": 2,
      "final_states": [
        { "id": "state-0", "value": "0:X1=0; 0:X3=0; [x]=0; [y]=1;" },
        { "id": "state-1", "value": "0:X1=0; 0:X3=0; [x]=0; [y]=2;" },
        { "id": "state-2", "value": "0:X1=0; 0:X3=0; [x]=1; [y]=1;" },
        { "id": "state-3", "value": "0:X1=0; 0:X3=0; [x]=1; [y]=2;" },
        { "id": "state-4", "value": "0:X1=0; 0:X3=1; [x]=0; [y]=1;" },
        { "id": "state-5", "value": "0:X1=0; 0:X3=1; [x]=0; [y]=2;" },
        { "id": "state-6", "value": "0:X1=0; 0:X3=1; [x]=1; [y]=1;" },
        { "id": "state-7", "value": "0:X1=0; 0:X3=1; [x]=1; [y]=2;" },
        { "id": "state-8", "value": "0:X1=1; 0:X3=0; [x]=1; [y]=1;" },
        { "id": "state-9", "value": "0:X1=1; 0:X3=0; [x]=1; [y]=2;" },
        { "id": "state-10", "value": "0:X1=1; 0:X3=0; [x]=2; [y]=1;" },
        { "id": "state-11", "value": "0:X1=1; 0:X3=0; [x]=2; [y]=2;" },
        { "id": "state-12", "value": "0:X1=1; 0:X3=1; [x]=1; [y]=1;" },
        { "id": "state-13", "value": "0:X1=1; 0:X3=1; [x]=1; [y]=2;" },
        { "id": "state-14", "value": "0:X1=1; 0:X3=1; [x]=2; [y]=1;" },
        { "id": "state-15", "value": "0:X1=1; 0:X3=1; [x]=2; [y]=2;" },
        { "id": "state-16", "value": "0:X1=1; 0:X3=2; [x]=1; [y]=1;" },
        { "id": "state-17", "value": "0:X1=1; 0:X3=2; [x]=1; [y]=2;" },
        { "id": "state-18", "value": "0:X1=1; 0:X3=2; [x]=2; [y]=1;" },
        { "id": "state-19", "value": "0:X1=1; 0:X3=2; [x]=2; [y]=2;" }
      ],
      "invoked_with_cli": [
        "-set-libdir", "./libdir", "-show", "prop", "-through", "all",
        "-showevents", "all", "-o", "-", "-output-format", "json",
        "fixtures/R+CAS-rfi-ctrl+DMBST.litmus"
      ]
    },
    "execution_graphs": [
      {
        "id": "execution-0",
        "is_valid": false,
        "satisfies_post_condition": true,
        "final_state_id": "state-19",
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
            "action": "R[x]*=1",
            "action_details": {
              "direction": "R",
              "location": "[x]",
              "size": "word",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 1,
            "action": "W[x]*=2",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "2"
            },
            "poi": 2,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 2,
            "action": "R[x]=2",
            "action_details": {
              "direction": "R",
              "location": "[x]",
              "size": "word",
              "value": "2"
            },
            "poi": 3,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 3,
            "action": "W[y]=1",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "1"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 4,
            "action": "W[y]=2",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 5,
            "action": "W[x]=1",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "1"
            },
            "poi": 4,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 6,
            "action": "W[y]=0",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "0"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 7,
            "action": "W[x]=0",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "0"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 8,
            "action": "W0:X1q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W1,#1",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 9,
            "action": "W0:X2q=2",
            "action_details": {
              "direction": "W",
              "location": "0:X2",
              "size": "quad",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#2",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 10,
            "action": "R0:X0q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "x"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 11,
            "action": "R0:X1q=1",
            "action_details": {
              "direction": "R",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 12,
            "action": "R0:X2q=2",
            "action_details": {
              "direction": "R",
              "location": "0:X2",
              "size": "quad",
              "value": "2"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 13,
            "action": "W0:X1q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 14,
            "action": "Branching(pred)([x]==0:X1)",
            "poi": 2,
            "event_labels": [ "noregs" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 15,
            "action": "R0:X0q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "x"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 16,
            "action": "W0:X3q=2",
            "action_details": {
              "direction": "W",
              "location": "0:X3",
              "size": "quad",
              "value": "2"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 17,
            "action": "R0:X3q=2",
            "action_details": {
              "direction": "R",
              "location": "0:X3",
              "size": "quad",
              "value": "2"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "CBNZ W3,.+4",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 18,
            "action": "Branching(bcc)",
            "poi": 4,
            "event_labels": [ "noregs" ],
            "instruction": "CBNZ W3,.+4",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 19,
            "action": "W0:X4q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X4",
              "size": "quad",
              "value": "1"
            },
            "poi": 5,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W4,#1",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 5,
              "instruction_labels": [ "LC00" ]
            }
          },
          {
            "id": 20,
            "action": "R0:X5q=y",
            "action_details": {
              "direction": "R",
              "location": "0:X5",
              "size": "quad",
              "value": "y"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 21,
            "action": "R0:X4q=1",
            "action_details": {
              "direction": "R",
              "location": "0:X4",
              "size": "quad",
              "value": "1"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 22,
            "action": "W1:X0q=2",
            "action_details": {
              "direction": "W",
              "location": "1:X0",
              "size": "quad",
              "value": "2"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W0,#2",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 23,
            "action": "R1:X1q=y",
            "action_details": {
              "direction": "R",
              "location": "1:X1",
              "size": "quad",
              "value": "y"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 24,
            "action": "R1:X0q=2",
            "action_details": {
              "direction": "R",
              "location": "1:X0",
              "size": "quad",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 25,
            "action": "DMB ST",
            "poi": 2,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DMB ST",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 26,
            "action": "W1:X2q=1",
            "action_details": {
              "direction": "W",
              "location": "1:X2",
              "size": "quad",
              "value": "1"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#1",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 27,
            "action": "R1:X3q=x",
            "action_details": {
              "direction": "R",
              "location": "1:X3",
              "size": "quad",
              "value": "x"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 28,
            "action": "R1:X2q=1",
            "action_details": {
              "direction": "R",
              "location": "1:X2",
              "size": "quad",
              "value": "1"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          }
        ],
        "edges": [
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 11 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 12 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 16 },
            "target": { "type": "event", "id": 17 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 24 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 13 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 16 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 11 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 12 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 15 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 17 },
            "target": { "type": "event", "id": 18 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 20 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 21 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 27 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 28 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 14 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 25 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 9 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 10 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 11 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 23 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 25 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 27 }
          }
        ]
      },
      {
        "id": "execution-1",
        "is_valid": false,
        "satisfies_post_condition": true,
        "final_state_id": "state-19",
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
            "action": "R[x]*=1",
            "action_details": {
              "direction": "R",
              "location": "[x]",
              "size": "word",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 1,
            "action": "W[x]*=2",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "2"
            },
            "poi": 2,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 2,
            "action": "R[x]=2",
            "action_details": {
              "direction": "R",
              "location": "[x]",
              "size": "word",
              "value": "2"
            },
            "poi": 3,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 3,
            "action": "W[y]=1",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "1"
            },
            "poi": 6,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 4,
            "action": "W[y]=2",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 5,
            "action": "W[x]=1",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "1"
            },
            "poi": 4,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 6,
            "action": "W[y]=0",
            "action_details": {
              "direction": "W",
              "location": "[y]",
              "size": "word",
              "value": "0"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 7,
            "action": "W[x]=0",
            "action_details": {
              "direction": "W",
              "location": "[x]",
              "size": "word",
              "value": "0"
            },
            "event_labels": [
              "mem", "noregs", "memf", "nobranches", "initwrites"
            ]
          },
          {
            "id": 8,
            "action": "W0:X1q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W1,#1",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 9,
            "action": "W0:X2q=2",
            "action_details": {
              "direction": "W",
              "location": "0:X2",
              "size": "quad",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#2",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 10,
            "action": "R0:X0q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "x"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 11,
            "action": "R0:X1q=1",
            "action_details": {
              "direction": "R",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 12,
            "action": "R0:X2q=2",
            "action_details": {
              "direction": "R",
              "location": "0:X2",
              "size": "quad",
              "value": "2"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 13,
            "action": "W0:X1q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X1",
              "size": "quad",
              "value": "1"
            },
            "poi": 2,
            "event_labels": [ "nobranches" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 14,
            "action": "Branching(pred)([x]==0:X1)",
            "poi": 2,
            "event_labels": [ "noregs" ],
            "instruction": "CAS W1,W2,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 15,
            "action": "R0:X0q=x",
            "action_details": {
              "direction": "R",
              "location": "0:X0",
              "size": "quad",
              "value": "x"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 16,
            "action": "W0:X3q=2",
            "action_details": {
              "direction": "W",
              "location": "0:X3",
              "size": "quad",
              "value": "2"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "LDR W3,[X0]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 17,
            "action": "R0:X3q=2",
            "action_details": {
              "direction": "R",
              "location": "0:X3",
              "size": "quad",
              "value": "2"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "CBNZ W3,.+4",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 18,
            "action": "Branching(bcc)",
            "poi": 4,
            "event_labels": [ "noregs" ],
            "instruction": "CBNZ W3,.+4",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 19,
            "action": "W0:X4q=1",
            "action_details": {
              "direction": "W",
              "location": "0:X4",
              "size": "quad",
              "value": "1"
            },
            "poi": 5,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W4,#1",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 5,
              "instruction_labels": [ "LC00" ]
            }
          },
          {
            "id": 20,
            "action": "R0:X5q=y",
            "action_details": {
              "direction": "R",
              "location": "0:X5",
              "size": "quad",
              "value": "y"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 21,
            "action": "R0:X4q=1",
            "action_details": {
              "direction": "R",
              "location": "0:X4",
              "size": "quad",
              "value": "1"
            },
            "poi": 6,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W4,[X5]",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 6
            }
          },
          {
            "id": 22,
            "action": "W1:X0q=2",
            "action_details": {
              "direction": "W",
              "location": "1:X0",
              "size": "quad",
              "value": "2"
            },
            "poi": 0,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W0,#2",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 23,
            "action": "R1:X1q=y",
            "action_details": {
              "direction": "R",
              "location": "1:X1",
              "size": "quad",
              "value": "y"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 24,
            "action": "R1:X0q=2",
            "action_details": {
              "direction": "R",
              "location": "1:X0",
              "size": "quad",
              "value": "2"
            },
            "poi": 1,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W0,[X1]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 1
            }
          },
          {
            "id": 25,
            "action": "DMB ST",
            "poi": 2,
            "event_labels": [ "noregs", "memf", "nobranches" ],
            "instruction": "DMB ST",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 2
            }
          },
          {
            "id": 26,
            "action": "W1:X2q=1",
            "action_details": {
              "direction": "W",
              "location": "1:X2",
              "size": "quad",
              "value": "1"
            },
            "poi": 3,
            "event_labels": [ "nobranches" ],
            "instruction": "MOV W2,#1",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 3
            }
          },
          {
            "id": 27,
            "action": "R1:X3q=x",
            "action_details": {
              "direction": "R",
              "location": "1:X3",
              "size": "quad",
              "value": "x"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          },
          {
            "id": 28,
            "action": "R1:X2q=1",
            "action_details": {
              "direction": "R",
              "location": "1:X2",
              "size": "quad",
              "value": "1"
            },
            "poi": 4,
            "event_labels": [ "nobranches" ],
            "instruction": "STR W2,[X3]",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 4
            }
          }
        ],
        "edges": [
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 11 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 12 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 16 },
            "target": { "type": "event", "id": 17 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 24 }
          },
          {
            "relation": "rf-reg",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 27 }
          },
          {
            "relation": "dmb.st",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 28 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "ctrl",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 21 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 3 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 5 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 6 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "ca",
            "source": { "type": "event", "id": 7 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 16 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 10 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 11 },
            "target": { "type": "event", "id": 13 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 11 },
            "target": { "type": "event", "id": 14 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 12 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 15 },
            "target": { "type": "event", "id": 2 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 17 },
            "target": { "type": "event", "id": 18 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 20 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 21 },
            "target": { "type": "event", "id": 3 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 23 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 24 },
            "target": { "type": "event", "id": 4 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 27 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_data",
            "source": { "type": "event", "id": 28 },
            "target": { "type": "event", "id": 5 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 14 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "iico_ctrl",
            "source": { "type": "event", "id": 14 },
            "target": { "type": "event", "id": 13 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 4 },
            "target": { "type": "event", "id": 25 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 8 },
            "target": { "type": "event", "id": 9 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 10 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 9 },
            "target": { "type": "event", "id": 11 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 13 },
            "target": { "type": "event", "id": 15 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 18 },
            "target": { "type": "event", "id": 19 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 19 },
            "target": { "type": "event", "id": 20 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 22 },
            "target": { "type": "event", "id": 23 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 25 },
            "target": { "type": "event", "id": 26 }
          },
          {
            "relation": "po",
            "source": { "type": "event", "id": 26 },
            "target": { "type": "event", "id": 27 }
          }
        ]
      }
    ]
  }
  JSONEND R+CAS-rfi-ctrl+DMBST
