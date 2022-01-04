Test option -variant mixed,MixedStrictOverlap

Strict overalap of mixed-size accesses is enforced on
both sides of teh genric same access relaxation Fre,
but not on both sides of Amo.Cas and LxSx.

Indeed, such a configuration of accesses is the only possibility
for Amo nstructions (by nature) and for Load/Store exclusive pairs
(* because UB are disallowed *)
