var: int a := 1;

fun: int aa()
begin
    printInt(a);
    var: int a := 0;
    printInt(a);
    return -1
end;

fun: main()
begin
    begin
        var: int a := 2;
        begin
            var: int a := 3;
            begin
                var: int a := 4;
                printInt(a)
            end;
            printInt(a)
        end;
        printInt(a)
    end;
    printInt(aa());
    printInt(a)
end
