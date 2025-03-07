fun: int a(ref: int b, var: string c)
begin
    return 0
end;

fun: main()
begin
    var: int d := 1;
    printInt(a(ref(d), 0))
end
