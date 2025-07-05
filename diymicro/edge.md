# Adding a standard edge

To add a standard edge, you will need to modify several pieces of code.
It consists mainly in filling the pattern matchings, adding entries to the lexer/parser and writing the relevant compilation code.

1. Add your edge in [`Edge.t`](edge.ml#t)
2. Fill the [`edge_direction`](edge.ml#edge_direction), [`edge_location`](edge.ml#edge_location), [`edge_int_ext`](edge.ml#edge_int_ext) functions
3. Fill the [`pp_edge`](edge.ml#pp_edge) function, this is expected to be accepted and recognized as the same edge by the parser
4. If the source or destination event of your edge is expected to be significant (eg Fr, Rf), modify [`set_significant_reads`](cycle.ml#set_significant_reads) in `cycle.ml`
5. Add your edge syntax in the lexer and parser
6. Fill the [`compile_edge`](compile.ml#compile_edge) function.
   Unless you have specified that the edge compiles its source or destination event (with `Rm true` or `Wm true`), 
   this is only supposed to take a dependency, do any instruction if relevant, and return a dependency for the next event.

For reference, `b3bf423` introduces Dmb to diymicro and should show the relevant edits.


# Adding an iico edge

## Required Information

1. **`instruction_name: string`**
   - The name of the instruction you are targeting. Suffix it with `:tag` if you have different variations of this instruction (for instance, `csel:ok` & `csel:no`)
   - You can then refer to it using `iico[${instruction_name} src->dst]` from diymicro's command line.

2. **`inputs: string list`**
   - Possible inputs events. (for instance, `["Rd"; "Rm"; "Rn"; "M"]` for swp)

3. **`outputs: string list`**
   - Similar to `inputs`, possible output events.

4. **`to_edge: string -> string -> iico_edge`**
   - Given a source and destination event defined respectively in `inputs` and `outputs`, this returns an iico_edge, which consist of:

   1. `repr: string`
      - Set this to `""`, it will later be set to `${instruction_name} ${src}->${dst}`

   2. `direction: direction * direction`
      - Specifies the type of events occurring before and after the edge.
      - If no memory events are involved (e.g., with `csel`), use `RegEvent`.
      - If you want the edge to compile its source or destination memory event, specify `Rm true` or `Wm true`  
        The `compile_edge` function will then receive these events details as input and will need to compile them properly.

   3. `ie: Internal,External`
      - Specifies whether changing the proc is necessary after this edge.  
      Note: `External` does not make sense and will raise an exception if the destination direction involves a register event.

   4. `sd: Same,Different`
      - Specifies whether the two memory events of the edge should occur at different locations.

   5. `significant_{source,dest}`
      - Indicates whether to check that the **memory event** at the source/destination of this edge has the expected value.  
      This expected value is the one that comes from the computed cycle (see `diymicro -v -v ...`)

   6. `compile_edge: state -> node_dep -> event_data option -> event_data option -> instructions list * node_dep * state`
      - Takes a compilation state and a dependency as input.
      - If the edge has specified that it wants to compile its source or destination event, it will also receive `event_data` objects.
      - Example dependency:  
        If the preceding event is a memory read like `LDR W1,[X0]`, the dependency will be `DepReg (A.Zreg 1) (Some expected_read_value)`.
      - This function returns instructions, a node dependency (as above), and the new compilation state.
      - Functions to get a new register, location, set final conditions, etc., are available in the [`AArch64_Compile.St`](AArch64_compile.ml#St) module.

With these elements, an iico edge can finally be added in [iico.ml](iico.ml) with:
```ocaml
  add_iico
    {
      instruction_name = _;
      to_edge =
        (fun src dst ->
          {
            repr = "";
            compile_edge = _ src dst;
            direction = _, _;
            ie = _;
            sd = _;
            significant_source = _;
            significant_dest = _;
          });
      inputs = _;
      outputs = _;
    };
```

## Examples

See [`Iico.Swp`](iico.ml#Swp)
