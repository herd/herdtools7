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
        when aa : a => pass;
    end

    print(aa.aa);

    return 0;
end
