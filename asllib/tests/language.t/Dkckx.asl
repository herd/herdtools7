// RUN: interp %s | FileCheck %s

func localassigns()
begin
    var a = 0;
    var b = 0;
end

func compiletimeexpressions()
begin
    var a = 0 + 0 + 0 + 0 + 0;
end

func compiletimecalls()
begin
    localassigns();
end

func main() => integer
begin
    return 0;
end
