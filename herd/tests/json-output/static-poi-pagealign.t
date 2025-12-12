The loader inserts a branch for page alignment, so the next source instruction
has static_poi 2. Labels remain attached to that source instruction.
Trailing labels are emitted in source order without an instruction or static_poi.

  $ herd7 -set-libdir ./libdir -show none -o - -output-format json fixtures/static-poi-pagealign.litmus | sed -n '/"program": \[/,/"init":/p'
      "program": [
        {
          "proc": 0,
          "function": "main",
          "instructions": [
            { "static_poi": 0, "instruction": "MOV W0,#1" },
            {
              "static_poi": 2,
              "instruction": "ADD W0,W0,#1",
              "labels": [ "first", "second" ]
            },
            { "labels": [ "end_first", "end_second" ] }
          ]
        }
      ],
      "init": "",
