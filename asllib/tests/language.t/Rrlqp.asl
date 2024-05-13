//R_RLQP: The domain of a constrained integer type with a constraint whose
//constraint ranges contain only well-constrained integer expressions is the
//union of the domain of its constraint ranges.


// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10, 20..30};

    return 0;
end
