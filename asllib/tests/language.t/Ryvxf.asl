//R_YVXF: When an exception is caught by a catcher or an otherwise_opt, the
//stmt_list corresponding to that catcher or the otherwise_opt is executed
//respectively.

// RUN: interp %s | FileCheck %s
// CHECK: a

type a of exception{};
type b of exception{};
type c of exception{};

func main() => integer
begin
    try
        throw a{};
    catch
        when a => print("a");
        when b => print("b");
        otherwise => print("other");
    end

    return 0;
end
