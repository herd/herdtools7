type Ity of integer {2,4,8};

func not_subset()
begin
    var A: integer {2,4,8};
    var B: integer {2,4};
    B = A; // illegal: {2,4,8} is not a subset of {2,4}.
end;
