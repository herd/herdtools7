// RUN: not interp %s | FileCheck %s

type a of exception{
    b: integer,
    b: integer
};

func main() => integer
begin
    return 0;
end
