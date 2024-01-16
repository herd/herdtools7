// RUN: interp %s | FileCheck %s

type a of exception{
    aa: integer
};

func main() => integer
begin
    try
        pass;
    catch
        when aa: a => var b: integer = aa.aa;
    end
    return 0;
end
