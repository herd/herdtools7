//I_FZMS: A corollary of R_YYPN is that a bitvector declared with an
//empty bitfield_list is identical to a bitvector declared with no
//bitfields. E.g. bits(8) and bits(8){} are the same type.
//R_YYPN: Two anonymous bitvector types are identical if they have the same
//width and they have bitfields with the same names and constituent bits,
//irrespective of the expressions used in their definition.
 
// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : bits(8) = '1111 1111';
    var b : bits(8){} = '1111 1111';
    var c = a == b;

    return 0;
end
