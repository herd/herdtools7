# Introducing new languages in herdtools7

A language can be a high-level programming language (e.g. C++, Java), an
assembly language (e.g. AArch64, x86), or a specialised language (e.g. BPF).

To run litmus tests for a target language (TL) it is required to introduce the
following files in the lib/ folder:

- TLBase.ml.
- TLLexer.{ml,mll}.
- TLParser.mly.

The lib/ folder contains modules that are shared between the three other
relevant directories: herd/, gen/ and litmus/.

The TLBase.ml file defines the basic types that all tools (herd, diy and
litmus) use. Using AArch64Base.ml as an example, it defines the following:

- Register formats: type gpr, type reg.
- Barriers: type barrier.
- Instruction: type kinstruction.
- Datatype of the AST.
- Various pretty printing functions: do_pp_instruction.

Once the lib/ modules are in place, it is possible to run hand written litmus tests with herd.

It is further required to define memory orders for the language, equivalent to lib/memOrder.ml
which is relative to C++.

After defining the lib/ files, the next steps are:

1. herd/: Define the semantics of commands which link to cat files.
2. gen/: Enable multiple tests generation.
3. litmus/: Enable running litmus tests.

## The herd tool

In herd/, three files are required:

- TLArch_herd.ml: Define the annotations that the language Effects can have and the sets that the
  cat file uses. (example: type annot and type annot_sets in AArch64Arch_herd.ml)
- TLMem.ml: Simple call to machModelChecker. It might be further required to define
  TLBarrier.mli which has the same type declaration as TLBase.ml.
- TLSem.ml: Using the already defined monadic operators, such as ">>=" or ">>|", assemble the
  semantics of a given instruction. See function build_semantics in herd/AArch64Sem.ml.

### Monadic operators in herd

Monadic operations are defined in herd/eventsMonad.ml and are used to combine
different Effects (sometimes called events) and add relations among them. Memory accesses
and ordering constraints enforced by synchronization operations are examples of Effects.

The monadic operations use various base operators, such as "=**=", "=*$=", "=$$=", "+|+", "=|=",
which are defined in herd/event.ml. These operators provide various ways of combining two Effect
graphs.

Event graphs can either represent the execution of a whole litmus test or be very small,
corresponding to some of the Effects contributing to the semantics of a single instruction or
command.

The operators build on three definitions:

- para_comp: Unions Effects from both Effect graphs, and their relations.
    - For example, para_comp is used in the semantics of a store (see do_str and its call in the
      definition of str in herd/AArch64Sem.ml). A store takes as input a source register rs, which
      contains the value v to be stored, and a target register rd, which contains the address a to
      be written to. In do_str, two things happen in parallel using >>|, which intrinsically uses
      para_comp: an address a is extracted from a register rd (ma in do_str, instantiated to get_ea
      in str) and a value v is extracted from register rs (via read_reg_data in do_str).
    - Base operators that build on para_comp:
        - "=|=": Corresponds to para_comp over disjoint graphs.
        - "+|+": Applies para_comp if there are two disjoint graphs, and asserts false
          otherwise (via check_both).
- data_comp: Introduces a causality link between the first and second Effect graphs (see
  intra_causality_data). This means that the Effect(s) of es1 must be before the Effect(s) of es2 in
  the order of operation of the instruction or command described.
    - Using the same str example as above, after extracting the value v and the address a in
      parallel, the value v is written to the address a via do_write_mem in do_str. This last Effect
      do_write_mem *must* occur after the two extracting Effects. In the herd diagrams, this is
      depicted with an iico arrow, which stands for intra-instruction causality order. This is used
      to indicate whether there is data dependency.
    - Base operators that build on data_comp:
        - "=*$=": Applies data_comp over disjoint graphs. This means that two graphs es1 and es2
          are composed and an iico causality link from es1 to es2 is added.
        - "=$$=": Similar to "=*$=", but additionally performs some transitive closure on the
          iico links (via get_output).
- control_comp: Introduces another type of causality link between the first and second Effect
  graphs (see intra_causality_control). This is similar to data_comp, but it distinguishes which
  type of causality is at stake. This is used to indicate whether there is a control-flow
  dependency.
    - Base operators that build on control_comp:
        - "=**=": Applies control_comp over disjoint graphs, meaning that two graphs es1 and
          es2 are composed and an iico_control causality link from es1 to es2 is added.
        - "=*$$=": Similar to "=**=", but additionally performs some transitive closure on the
          iico_control links (via get_output).

+-----------+----------------------------------------------------------------+
|Operator   |Description                                                     |
+===========+================================================================+
|>>|        |Intuitively a >>| b means "do a in parallel with b". This is    |
|>>||       |used when accesses do not need to be ordered. The code snippet  |
|           |below reads the register rs and in parallel reads the register  |
|           |rd, gets a value v from rs and a location x from rd, and writes |
|           |that value v back to that memory location x.                    |
|           |                                                                |
|           |```                                                             |
|           |(read_reg rs >>| read_reg rd) >>= fun (v,x) -> write_mem x v    |
|           |```                                                             |
|           |                                                                |
|           |">>||" is similar to ">>|", but this operator does not check    |
|           |whether the two argument Effect graphs are disjoint.            |
+-----------+----------------------------------------------------------------+
|>>=        |Intuitively a >>= b means "do a, collect its result and feed it |
|>>==       |into b". The code snippet below reads the register rs, gets a   |
|           |value v, and writes that value v back to memory location x.     |
|           |                                                                |
|           |```                                                             |
|           |read_reg rs >>= fun v -> write_mem x v                          |
|           |```                                                             |
|           |                                                                |
|           |The operator ">>=" corresponds to "=*$=", while ">>=="          |
|           |corresponds to "=$$=". They both do data_comp over disjoint     |
|           |graphs, with the latter performing transitive closure on        |
|           |iico causality link.                                            |
+-----------+----------------------------------------------------------------+
|>>*=       |The operator ">>*=" corresponds to "=**=", while ">>*=="        |
|>>*==      |corresponds to "=*$$=". They both do control_comp over disjoint |
|           |graphs, with the latter performing transitive closure on        |
|           |iico_control links.                                             |
+-----------+----------------------------------------------------------------+

## The diy tool

The gen/ directory is used for the diy7 tool suite which enables one or multiple tests generation.

The algorithm that generates litmus tests from cycles of relaxations is parametric in the
architecture, resulting in little to no additions required when introducing a new language.

To add a new backend to the general test generation algorithm, two modules are required:

- TLArch_gen.ml: Define the syntax of the tests. For example, what is the syntax of a load?
- TLCompile_gen.ml: Define how the relaxations of a given cycle combine, and what sequence of
  code to output on a given relaxation.

To better understand TLArch_gen.ml, an example of such file is CArch_gen.ml, with function dump_typ
and type exp as example candidates.

To better understand TLCompile_gen.ml, an example of such file is CCompile_gen.ml. It contains function
type_event, which checks whether an extremity is atomic or not, in the sense defined by the
architecture. It also contains the function compile_access, which could, for example, output a
store using function compile_store as per the architecture definition, when
given W as an input. See also functions dump_exp, dump_ins, dump_c_test which print the right
syntax. There needs to be an equivalent of these function for each new language.

## The litmus tool

The litmus/ directory is used for the litmus7 tool which enables running litmus tests.

To enable the litmus7 tool to run litmus tests on hardware, it is required to add new skeletons
that the litmus assembly can be slotted into only when the language is a programming language.
In this case, two files are required:

- TLArch_litmus
- TLCompile_Litmus: Performs the code emission.

These enable the litmus7 tool to create a program in the programming language
that is being introduced and then run it on the target machine.
