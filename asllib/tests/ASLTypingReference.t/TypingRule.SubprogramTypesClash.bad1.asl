accessor X() <=> value_in: bits (4)
begin
    getter
        return '1000';
    end;
    setter
        Unreachable();
    end;
end;

func X() // Illegal as accessor `X` also declared.
begin pass; end;
