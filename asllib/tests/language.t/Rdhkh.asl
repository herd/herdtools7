//R_DHKH: When a catcher’s stmt_list begins executing, the catcher’s
//exception is declared (if provided) and denotes a local, immutable,
//execution-time storage element which is initialised with the value of the
//exception which the catcher caught.

// RUN: not interp %s | FileCheck %s

type a of exception{
    aa: integer
};

func main() => integer
begin
    try
        throw a {
            aa=10
        };
    catch
        when aa : a => aa.aa = 20;
    end

    return 0;
end
