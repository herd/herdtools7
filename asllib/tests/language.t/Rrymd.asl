// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var result = '1000' IN '1xxx';

    return 0;
end
