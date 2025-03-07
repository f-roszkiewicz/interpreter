fun: int getInt(var: int a, var: string b, var: bool c)
begin
    return a
end;

fun: string getString(var: int a, var: string b, var: bool c)
begin
    return b
end;

fun: bool getBool(var: int a, var: string b, var: bool c)
begin
    return c
end;

fun: main()
begin
    var: int a := 1;
    var: string b := "a";
    var: bool c := true;
    printInt(getInt(a,b,c));
    printString(getString(a,b,c));
    printBool(getBool(a,b,c))
end
