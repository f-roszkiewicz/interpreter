fun: main()
begin
    var: int i := 1;
    var: int sum := 0;
    while i <= 10 do
    begin
        if i % 2 == 0 then
            printInt(i)
		else
		    skip;
		sum := sum + i;
		i := i + 1
    end;
    printInt(sum)
end
