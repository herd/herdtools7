Each execution has a nested proc/poi instruction map. Instruction-backed events
reference entries shared by events of that occurrence.
For self-modifying code, the fetched instruction can differ between executions
at the same static position. Fetch, decode and execution events share the entry.

  $ herd7 -set-libdir ./libdir -variant self -show all -showevents all -o - -output-format json fixtures/self-modifying.litmus > self.json
  $ sed -n '/"instructions": {/,/"events": \[/p' self.json
        "instructions": {
          "0": { "0": { "instruction": "B .+8", "static_poi": 0 } },
          "1": { "0": { "instruction": "STR W0,[X1]", "static_poi": 0 } }
        },
        "events": [
        "instructions": {
          "0": {
            "0": { "instruction": "NOP", "static_poi": 0 },
            "1": { "instruction": "MOV W2,#1", "static_poi": 1 }
          },
          "1": { "0": { "instruction": "STR W0,[X1]", "static_poi": 0 } }
        },
        "events": [

Fetch, decode and execution events reference the same proc/poi. Initial writes
have no instruction reference. Instruction text and origin metadata are absent
from events.

  $ sed -n '/"events": \[/,/"edges": \[/p' self.json | grep -E '"(action|proc|poi|instruction|instruction_origin|static_poi|instruction_labels|function)":'
            "action": "R[label:\"P0:L1\"]IFetch=instr:\"B .+8\"",
            "proc": 0,
            "poi": 0,
            "action": "W[label:\"P0:L1\"]=NOP",
            "proc": 1,
            "poi": 0,
            "action": "W[label:\"P0:L1\"]=instr:\"B .+8\"",
            "action": "Branching(pred)(decode)",
            "proc": 0,
            "poi": 0,
            "action": "Empty",
            "proc": 0,
            "poi": 0,
            "action": "R1:X1q=label:\"P0:L1\"",
            "proc": 1,
            "poi": 0,
            "action": "R1:X0q=NOP",
            "proc": 1,
            "poi": 0,
            "action": "R[label:\"P0:L1\"]IFetch=NOP",
            "proc": 0,
            "poi": 0,
            "action": "W[label:\"P0:L1\"]=NOP",
            "proc": 1,
            "poi": 0,
            "action": "W[label:\"P0:L1\"]=instr:\"B .+8\"",
            "action": "Branching(pred)(decode)",
            "proc": 0,
            "poi": 0,
            "action": "Empty",
            "proc": 0,
            "poi": 0,
            "action": "W0:X2q=1",
            "proc": 0,
            "poi": 1,
            "action": "R1:X1q=label:\"P0:L1\"",
            "proc": 1,
            "poi": 0,
            "action": "R1:X0q=NOP",
            "proc": 1,
            "poi": 0,

The map still reports the fetched NOP when only memory events are displayed.

  $ herd7 -set-libdir ./libdir -variant self -show all -showevents mem -o - -output-format json fixtures/self-modifying.litmus > self-mem.json
  $ sed -n '/"instructions": {/,/"events": \[/p' self-mem.json
        "instructions": {
          "0": { "0": { "instruction": "B .+8", "static_poi": 0 } },
          "1": { "0": { "instruction": "STR W0,[X1]", "static_poi": 0 } }
        },
        "events": [
        "instructions": {
          "0": { "0": { "instruction": "NOP", "static_poi": 0 } },
          "1": { "0": { "instruction": "STR W0,[X1]", "static_poi": 0 } }
        },
        "events": [

Generated padding instructions need no matching source program row.

  $ herd7 -set-libdir ./libdir -show all -showevents all -o - -output-format json fixtures/static-poi-pagealign.litmus > pagealign.json
  $ sed -n '/"instructions": {/,/"events": \[/p' pagealign.json
        "instructions": {
          "0": {
            "0": { "instruction": "MOV W0,#1", "static_poi": 0 },
            "1": { "instruction": "B .+4092", "static_poi": 1 },
            "2": { "instruction": "ADD W0,W0,#1", "static_poi": 2 }
          }
        },
        "events": [

Dynamic poi continues through fault handlers, while static_poi restarts.
Only handler entries have a function marker.

  $ herd7 -set-libdir ./libdir -variant vmsa -show all -showevents all -o - -output-format json fixtures/fault-handler.litmus > handler.json
  $ sed -n '/"instructions": {/,/"events": \[/p' handler.json
        "instructions": {
          "0": {
            "0": { "instruction": "SVC #0", "static_poi": 0 },
            "1": {
              "instruction": "MOV W2,#1",
              "static_poi": 0,
              "function": "fault_handler"
            },
            "2": {
              "instruction": "ERET",
              "static_poi": 1,
              "function": "fault_handler"
            },
            "3": { "instruction": "ADD W2,W2,#2", "static_poi": 1 }
          }
        },
        "events": [

Loop iterations have distinct dynamic entries with repeated static indices.

  $ herd7 -set-libdir ./libdir -unroll 3 -show all -showevents all -o - -output-format json fixtures/unrolling-limit.litmus > loop.json
  $ sed -n '/"instructions": {/,/"events": \[/p' loop.json
        "instructions": {
          "0": {
            "0": { "instruction": "MOV W0,#0", "static_poi": 0 },
            "1": { "instruction": "ADD W0,W0,#1", "static_poi": 1 },
            "2": { "instruction": "LDR W2,[X1]", "static_poi": 2 },
            "3": { "instruction": "CMP W0,W2", "static_poi": 3 },
            "4": { "instruction": "B.NE .-12", "static_poi": 4 },
            "5": { "instruction": "ADD W0,W0,#1", "static_poi": 1 },
            "6": { "instruction": "LDR W2,[X1]", "static_poi": 2 },
            "7": { "instruction": "CMP W0,W2", "static_poi": 3 },
            "8": { "instruction": "B.NE .-12", "static_poi": 4 },
            "9": { "instruction": "ADD W0,W0,#1", "static_poi": 1 },
            "10": { "instruction": "LDR W2,[X1]", "static_poi": 2 },
            "11": { "instruction": "CMP W0,W2", "static_poi": 3 },
            "12": { "instruction": "B.NE .-12", "static_poi": 4 }
          }
        },
        "events": [
