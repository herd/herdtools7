//R_QKXV: The optional parameter_list declares the parameters of the getter
//or setter. ASL permits the declaration of subprograms with formals which
//are parameterized such that where a formal or return type is a bitvector,
//its width may depend on the value of the subprogram parameters. 

// RUN: interp %s | FileCheck %s

var _a: integer;

getter a[] => integer
begin
    return _a;
end

setter a[] = value: integer
begin
    _a = value;
    return;
end

getter b[index: integer] => integer
begin
    return _a * index;
end

setter b[index: integer] = value: integer
begin
    _a = value * index;
    return;
end

func main() => integer
begin
    return 0;
end
