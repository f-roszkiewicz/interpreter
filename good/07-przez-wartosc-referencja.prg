fun: int inc(var: int a, ref: int b)
begin
    a := a+1;
    b := b+1;
    return a
end;

fun: main()
begin
    var: int a := 5;
    var: int b := 10;
    var: int c := inc(a, ref(b));
    printInt(a);
    printInt(b);
    printInt(c)
end
