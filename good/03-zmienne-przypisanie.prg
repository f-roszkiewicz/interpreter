var: int i := 1;

fun: main()
begin
    var: int j := 4;
    i := j*j;
    j := i+j;
    printInt(i);
    printInt(j)
end
