//R_HDDS: The syntax
//  cexpr as <constraint>
//is syntactic sugar for
//  cexpr as integer <constraint>


// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10} = 5;

    var b: integer{5..6} = a as {5..6};

    return 0;
end