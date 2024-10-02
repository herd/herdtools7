//D_JTDG: The caught type of a catcher’s exception is the type it is
//annotated with in the catcher.

// RUN: interp %s | FileCheck %s
// CHECK: 10

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
        when aa : a => print(aa.aa);
    end

    return 0;
end
