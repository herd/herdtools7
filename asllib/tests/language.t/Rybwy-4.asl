//R_YBWY: Named type declarations must not be recursive.

type T1 of integer;        // the named type "T1" whose structure is integer
type T2 of (integer, T1);  // the named type "T2" whose structure is (integer, integer)

//The following code declares two unique types with the same structure. Note
//that the two types are not related in any way and are not interchangeable.

type qualifiedData of bits(16) { [4]        flag,
                                 [3:0, 8:5] data,
                                 [9:0]      value };
type DatawithFlag of qualifiedData;

