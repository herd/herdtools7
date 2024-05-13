//I_GZVM: The catcher’s exception may have a caught type which is not the
//same as the thrown type of the original exception which was caught.

// RUN: interp %s | FileCheck %s
// CHECK: 10

type a of exception{
    aa: integer
};

type b subtypes a;

func main() => integer
begin
    try
        throw b {
            aa=10
        };
    catch
        when aa : a => print(aa.aa);

    end

    return 0;
end
