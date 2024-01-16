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
