fun: int silnia(var: int n)
begin
    var: int i := 1;
    var: int r := 1;
    while i < n+1 do
    begin
        r := r*i;
        i := i+1
    end;
    return r
end;

fun: int silniaRek(var: int n)
begin
    if n < 2 then
        return 1
    else
        return n * silniaRek(n-1)
end;

fun: main()
begin
    printInt(silnia(6));
    printInt(silniaRek(7))
end
