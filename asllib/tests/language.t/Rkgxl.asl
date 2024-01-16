// RUN: interp %s | FileCheck %s

type a of exception;
type b of exception{};

func main() => integer
begin
    return 0;
end
