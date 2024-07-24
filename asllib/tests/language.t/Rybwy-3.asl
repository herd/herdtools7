//R_YBWY: Named type declarations must not be recursive.

type T1 of integer;        // the named type "T1" whose structure is integer
type T2 of (integer, T1);  // the named type "T2" whose structure is (integer, integer)

//The following type declarations are recursive and therefore illegal.

type base of record {one: other};
type other of base;
