//I_GQYG: This means that constant and let identifiers can be used to
//demonstrate that a bitvector’s width is acceptable.
//Comparing bitvector widths involves maintaining a mapping from immutable
//identifiers to their values. Where the value of an immutable identifier is
//not given as a statically evaluable expression, the mapping should simply
//map the identifier to itself.  Comparisons of bitvector width requires the
//ability to use these mappings to reduce immutable terms in an expression
//using arithmetic rewrites and normalization.
//Since the type-checker uses statically evaluable expressions when
//determining bitvector widths, immutable values of type integer are treated
//as width constraints by the type-checker and used during width inference.
//For example, the declaration let F: integer = (N - E)- 1; in the following
//function is used as a width constraint This is a minor, but important,
//extension to allow the floating point functions to be written in a more
//natural style. Consider the following function and how we can type-check
//the final return statement.

func FPZero(sign: bit, N: integer {16,32,64}) => bits(N)
begin
    // type checker knows N-->N
let E: integer = if N == 16 then 5 elsif N == 32 then 8 else 11;
// type checker knows E-->if N == 16 then 5 elsif N == 32 then 8 else 11
let F: integer = (N - E) - 1;
// type checker knows F-->N-(if N == 16 then  5 elsif N == 32 then  8 else    11) - 1
// which is F-->(if N == 16 then (N -5) elsif N == 32 then ( N-8) else (N-11)) - 1
// which is F-->(if N == 16 then (16-5) elsif N == 32 then (32-8) else (N-11)) - 1
//whichisF-->(ifN==16then( 11)elsifN==32then( 24)else(N-11))-1 //whichisF-->(ifN==16then( 10)elsifN==32then( 23)else(N-12))-1
var exp  = Zeros(E);
var frac = Zeros(F);
return [sign, exp, frac];
// type checker knows width of return expression is 1 + E + F
// which is 1
//    + (if N == 16 then  5 elsif N == 32 then  8 else    11 )
//    + (if N == 16 then 10 elsif N == 32 then 23 else (N-12))
// which is 1
//    + (if N == 16 then 15 elsif N == 32 then 31 else (N- 1))
// which is
//      (if N == 16 then 16 elsif N == 32 then 32 else     N )
// which is N
end

