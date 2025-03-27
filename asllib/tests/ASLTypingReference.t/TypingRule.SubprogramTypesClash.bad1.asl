accessor X() <=> bits (4)
begin
    getter begin
        return '1000';
    end;
    setter = value_in begin
        Unreachable();
    end;
end;

func X() // Illegal as accessor `X` also declared.
begin pass; end;
