type SuperRec of record{-};
type SubRec subtypes SuperRec;
func structured_procedure(r: SuperRec) begin pass; end;
// Illegal as `SubRec` subtype-satisfies `SuperRec`.
func structured_procedure(r: SubRec) begin pass; end;
