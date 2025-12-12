Combined C RMW events retain their action text but omit action_details, which
currently only describes plain memory and register accesses.

  $ herd7 -set-libdir ../../libdir -show all -o - -output-format json fixtures/C14.litmus | sed -n '/^JSONBEGIN /,$p'
  JSONBEGIN C14
  {
    "schema_version": 1,
    "test": {
      "name": "C14",
      "kind": "Required",
      "architecture": "C",
      "info": [
        { "key": "Hash", "value": "4ba3dd96c70ac3dd5dac4c7337261858" }
      ],
      "program": [
        {
          "proc": 0,
          "function": "main",
          "instructions": [
            {
              "static_poi": 0,
              "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;"
            }
          ]
        },
        {
          "proc": 1,
          "function": "main",
          "instructions": [
            {
              "static_poi": 0,
              "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;"
            }
          ]
        }
      ],
      "init": "0:y=y; 1:y=y;",
      "condition": "forall ([y]=2)"
    },
    "result": {
      "model": "Generic(C++11)",
      "verdict": "Ok",
      "observation": "Always",
      "positive": 2,
      "negative": 0,
      "candidates": 2,
      "failed_candidates": 4,
      "execution_graph_count": 2,
      "final_states": [ { "id": "state-0", "value": "[y]=2;" } ],
      "invoked_with_cli": [
        "-set-libdir", "../../libdir", "-show", "all", "-o", "-",
        "-output-format", "json", "fixtures/C14.litmus"
      ]
    },
    "execution_graphs": [
      {
        "id": "execution-0",
        "is_valid": true,
        "satisfies_post_condition": true,
        "final_state_id": "state-0",
        "pretty_conf": {
          "events": {
            "showevents": "noregs",
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
            "action": "RMW(Rlx)[y](0>1)",
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 1,
            "action": "RMW(Rlx)[y](1>2)",
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 2,
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
          }
        ],
        "edges": [
          {
            "relation": "scp",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "scp",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "scp",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "mo",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "mo",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "fr",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 0 },
            "target": { "type": "event", "id": 1 }
          }
        ]
      },
      {
        "id": "execution-1",
        "is_valid": true,
        "satisfies_post_condition": true,
        "final_state_id": "state-0",
        "pretty_conf": {
          "events": {
            "showevents": "noregs",
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
            "action": "RMW(Rlx)[y](1>2)",
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;",
            "instruction_origin": {
              "proc": 0,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 1,
            "action": "RMW(Rlx)[y](0>1)",
            "poi": 1,
            "event_labels": [ "mem", "noregs", "memf", "nobranches" ],
            "instruction": "atomic_fetch_add_explicit(y,1,memory_order_relaxed);;",
            "instruction_origin": {
              "proc": 1,
              "function": "main",
              "static_poi": 0
            }
          },
          {
            "id": 2,
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
          }
        ],
        "edges": [
          {
            "relation": "scp",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "scp",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "scp",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "mo",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "mo",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 1 }
          },
          {
            "relation": "fr",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 1 },
            "target": { "type": "event", "id": 0 }
          },
          {
            "relation": "rf",
            "source": { "type": "event", "id": 2 },
            "target": { "type": "event", "id": 1 }
          }
        ]
      }
    ]
  }
  JSONEND C14
