//R_KDKS: Thetypedeclarationtype id of ty ; declares a new type id which has
//the type of ty

// RUN: interp %s | FileCheck %s

type id of integer;

var a: id;


func main() => integer
begin
    return 0;
end
