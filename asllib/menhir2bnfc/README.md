# `menhir2bnfc` tool

## Input data
To use this tool you need the following input files:
    * A Menhir `.cmly` file (see `menhir --help` on instructions on building a cmly file from mly file(s))
    * For the lexer an `ocamllex` `.ml` file. Speicifically one built with the `ocamllex -ml` flag.

## Usage with AslRef

To build and run this script in `aslref` make sure you have `ppxlib` installed.

Then from the `herdtools7` root directory you can run the following:
```
make
# Note: The following command succeds with errors related to type inference.
# Since we don't care about the backend this is not relevant.
menhir --cmly --base Parser asllib/Parser.mly asllib/Tokens.mly
ocamllex -ml asllib/Lexer.mll -o Lexer.ml

# At this point you should have a Parser.cmly and Lexer.ml file
# To build the grammar run:
./_build/install/default/bin/menhir2bnfc --ml Lexer.ml Parser.cmly grammar.cf

# You should now have a bnfc compliant grammar.cf file!
```

## Building and running the script

To build you simply run `dune build`.

The script can be run using:
```
menhir2bnfc <cmly file> <output file>
```

For example:
```
menhir2bnfc Parser.cmly out.cf
```

See `menhir2bnfc --help` for more details.

## Transforming the Grammar to BNFC

### Terms used

 * `terminal` - a reference to a lexer token
 * `nonterminal` - a reference to the name of a set of `productions`
 * `production` - a recipe describing one of the ways a `nonterminal` is built (reduced) from a set of terminal and nonterminal symbols
 * `LR(1) state machine` - A graph where each node has a set of `shift` rules which map the next `terminal` or `nonterminal` to the next node in the
   state machine and `reduce` rules which map the next `terminal` or `nonterminal` to the `production` it results in. The state machine chooses whether
   to shift or reduce based on the next token given by the lexer until the parse succeeds (a reduction marking the complete parse happens) or fails
   (no valid next step exists)

### Algorithm outline

 1. Collect entry points directly from the `cmly` data.
    * These produce an `entrypoints` bnfc entry.

 2. Infer operator precedence from the lr(1) state machine.

    2.1. Create an associative list linking each production to the set of terminals which cause the parser to reduce to that production.
    2.2. Determine the nonterminals with ambiguous precedence
        * This is done by grouping productions by their nonterminal identifiers. Nonterminals with ambiguous precedence have varying sets of
          tokens following different productions
    2.3. (TODO) Verify that the nonterminals are actually related (are part of the same component on the parse graph)
        * The next steps likely want to be done for each component
    2.4. (TODO) Determine the associativity of each of the operands in the ambiguous productions
        * The script assumes left associativity for now
    2.5. Determine if any of the detected nonterminals can be removed and remove them
        * In cases where a nonterminal has a "final" production (all but one production end with a recursive reference) we can infer that the set of
          terminals which follow that final case must follow all other cases. Similarly, any other rule which terminates by that nonterminal is followed
          by the same set. This feels like an inefficiency in the LR(1) state machine menhir generates?
    2.6. Generate a precedence lists by sorting the productions by the length of terminals following them - shortest meaning lowest precedence
        * In order to compare the different nonterminals in the same component for each nonterminal's productions we subtract the shortest set from
          all productions. This is done to remove terminals which come from outside the component.
    2.7. Split off unary ops as their own precedence group (if necessary)
        * Since unary ops don't usually produce other unary ops (`!Expr !Expr` is not usually valid syntax) any production which starts
          with leading terminals and ends with a recursive reference will appear a level above its precedence level unless the unary op is
          the highest precedence
    2.8. Correct any binary expr productions which are in the wrong level
        * Cases such as `Expr Op <terminal>` appear to mismatch the set of tokens which follow `Expr Op Expr` for the same `Op`.
          Such cases are grouped with ther corresponding `Op` to avoid conflicts

 3. Generate bnfc-style lines from the grammar productions
    3.1. For the precedence order productions - update their terms such that
	    * Unary ops recurse to their own precedence level
	    * (assumption) Binary ops are left associative.
    3.2. All remaining productions are trivially mapped onto bnfc equivallents

### Limitations

The algorithm implemented has some limitations worth noting:
    1. It assumes that ambiguous precedence expressions follow a `expr op expr`/`<terminals> expr`/`expr <non op> ...` structure
       If somebody was to create an in-between production `expr opexpr` where `opexpr := op expr` the algorithm may not be sufficient to detect this.
    2. Associativity is assumed to be left.

Possible future work/ideas for each of these
    1. Identifying which operands actually control precedence and associativity would be a more scalable approach than working on productions only
        * Actually calculating where shift/reduce conflicts would happen if the productions generated a parser without precedence rules might give us this
    2. If we know which operands control the precedence rules, we could see where in the LR(1) table the reductions happen.
        * For left associativity a reduction `Expr Op Expr` will happen at  `Expr . Op Expr` first
        * For right associativity a reduction `Expr Op Expr` should not happen at `Expr . Op Expr` since Op should be a `shift` and not a reduce
        * This may need more detailed investigation. `menhir`'s SDK does have some of this data in it's LR0 components of the LR(1) table
