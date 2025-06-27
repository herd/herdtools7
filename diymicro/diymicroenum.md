# Diymicro-enum

`diymicroenum iico[instr src->dst]` produces a LB and MP associated with the iico edge going from src to dst in instr.  
For instance, `diymicroenum iico[swp M->Rn]` will produce "LB+rel+SWP-MRn" that corresponds to the following diymicro cycle: `Rfe PodRW:L Rfe 'iico[swp M->Rn]' DpAddrdrW` and "MP+rel+SWP-MRn" which corresponds to `Fre PodWW:L Rfe 'iico[swp M->Rn]' DpAddrdrR`.

If wildcards are used in src or dst, `diymicroenum` will generate a LB and MP for each possible combination of src or dst.
For convenience, `iico[instr]` is an alias to `iico[instr *->*]`.
