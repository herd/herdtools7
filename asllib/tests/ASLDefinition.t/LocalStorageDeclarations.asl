func main() => integer
begin
    // Declaration of a local variable with identifier size of type integer
    // and initial value 256:
    var size: integer = 256;

    // The type may be omitted from the declaration if it can be unambiguously
    // inferred from the initializer expression
    var size_implicitly_typed = 256; // Automatically typed as integer{256}

    // Multiple variables may be declared in a single declaration,
    // but no initial value(s) may be specified using this form:
    var x, y, z: integer;

    // The following declares four local mutable variables:
    // * myF    - an integer, initialized with the result of invoking f()
    // * length - initialized with the integer 256
    // * myH    - initialized with the result of invoking h()
    // *          Since the type of myH is not specified, it is derived
    //            from the return type of h()
    // * The invocation of g() occurs but the result is discarded.
    var (myF, -, length, myH) = (f(), g(), 256, h());
    return 0;
end;

func f() => integer begin return 1; end;
func g() => integer begin return 2; end;
func h() => integer begin return 2; end;
