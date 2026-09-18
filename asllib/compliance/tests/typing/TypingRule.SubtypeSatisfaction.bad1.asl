let Int12: integer{1..2} = 2;

// The following declaration is illegal,
// since the right-hand-side expression has the type integer{1..2},
// which means both 1 and 2 can be assigned, whereas the left-hand-side
// has type integer{Int12} which is can hold exactly one value ---
// the runtime value of Int12.
var x : integer{Int12} = Int12;
