//R_FZSD: The syntax
// bits( <constraint> )
//is syntactic sugar for
//    bits( -: integer <constraint> )

// RUN: interp %s | FileCheck %s

type a of bits({0..10});
type b of bits(-: integer{0..10});

func main() => integer
begin
    return 0;
end