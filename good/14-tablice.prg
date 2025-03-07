arr: int a := [1,2,3,4,5];

fun: main()
begin
    arr: int b := [11,12,13,14,15];
    arr: string c := ["ala", "ma", "kota"];
    var: int i := 0;
    while i < 10 do
    begin
        if i < 5 then
            printInt(a[i])
        else
            printInt(b[i-5]);
        if i < 3 then
            printString(c[i])
        else
            skip;
        i := i+1
    end
end
