From the HSA documentation, using published version 1.0 final.

HSA01: This is IRIW
HSA02: This is MP, classical release-acquire idiom.
HSA03: This is isa2, transitive releas-acquire idiom
HSA04: THis is isa2, with 2 workgroups (wg 0 1) (wg 2)
NOTE: The variable Z is defined in the group segment and used to
synchronize 1 and 2, which are not in the same work-group.
NB segnents not implememted yet
HSA05: This is MP, identical to HS02 up to wg -> agent on read acquire.
HSA06: This is HSA04, with two differences: all variables in the global segment.
       The read-acquire by P1 has scope annotation agent (wg for HSA04)
HSA07: THis is MP, with fre  -> W by P0
HSA08: OMITTED About group segment
HSA09: MP with fences.
HSA10: LB+ctrl ordinary accesses.
HSA11: LB+datas relaxed accesses.
HSA12: SB relaxed accesses
HSA13: Ordinary race
HSA14: Special race


Bonus
HSA12+fences: show that fences do not restore SC.


