//D_QXYC: An exception is a structured type consisting of a list of field
//identifiers which denote individual storage elements.

// RUN: interp %s | FileCheck %s

type a of exception{};

func main() => integer
begin
    return 0;
end
