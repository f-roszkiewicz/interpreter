fun: bool yes(var: int pr)
begin
    printInt(pr);
    return 1
end;

fun: main()
begin
    var: int i := 0;
    printBool(yes(i))
end
