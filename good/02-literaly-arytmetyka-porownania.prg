fun: main()
begin
    var: int i := 2;
    var: int j := 3;
    if i < j then
        printInt(i)
    else
        printInt(j);
    printInt(i+j);
    printInt(i*j);
    printInt(i-j);
    printInt((j+2)/i);
    printInt((j+2)%i);
    printBool(i >= j)
end
