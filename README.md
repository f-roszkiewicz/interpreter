Kompilacja: make
Uruchomienie: ./Interpreter [plik]

Jêzyk imperatywny: po³¹czenie Tiny(z przedmiotu semantyka i weryfikacja programów) z Pascalem.

Gramatyka z BNFC jest za³¹czona w pliku Tiny.cf

Tabelka cech jêzyka:

  Na 15 punktów
+ 01 (trzy typy): int, string, bool
+ 02 (litera³y, arytmetyka, porównania)
+ 03 (zmienne, przypisanie)
+ 04 (print)
+ 05 (while, if)
+ 06 (funkcje lub procedury, rekurencja)
+ 07 (przez zmienn¹ / przez wartoœæ / in/out): przez zmienn¹(ref) i wartoœæ(var)
  08 (zmienne read-only i pêtla for)
  Na 20 punktów
+ 09 (przes³anianie i statyczne wi¹zanie)
+ 10 (obs³uga b³êdów wykonania)
+ 11 (funkcje zwracaj¹ce wartoœæ): tylko funkcje zwracaj¹ce wartoœæ, bez procedur
  Na 30 punktów
+ 12 (4) (statyczne typowanie)
+ 13 (2) (funkcje zagnie¿d¿one ze statycznym wi¹zaniem) - nie zaimplementowane
+ 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe): tablice(arr) indeksowane int za 1 punkt
  15 (2) (krotki z przypisaniem)
+ 16 (1) (break, continue) - nie zaimplementowane
  17 (4) (funkcje wy¿szego rzêdu, anonimowe, domkniêcia)
  18 (3) (generatory)

Razem: 28 pkt

Przyk³ady programów:

1.
# Hello world

fun : main ( )
begin
    printString ( "Hello world!" ) ;
    return 0 ;
end

2.
# wypisz liczby parzyste do 10

fun : main ( )
begin
    var : int i := 0 ;
    while i < 10 do
    begin
        if i % 2 == 0 then printInt ( i ) else skip ;
        i := i + 1
    end ;
    printInt ( i ) ;
    return 0
end

3. silnia na dwa sposoby
# iteracyjnie
fun : int fact ( var : int n )      # dla referencji ( ref : int n )
begin
    var : int i := 1 ;
    var : int r := 1 ;
    while i < n + 1 do
    begin
        r := r * i ;
        i := i + 1 ;
    end ;
    return r
end ;

# rekurencyjnie
fun : int factr ( var : int n )
begin
    if n < 2 then return 1 else return n * factr ( n - 1 )
end ;

fun : main ( )
begin
    printInt ( fact ( 7 ) ) ;
    printInt ( factr ( 7 ) ) ;
    return 0
end

4. inicjalizowanie tablic
arr : int tab1 := [ 1 , 2 , 3 , 4 , 5 ] ;
arr : string tab2 := [ "ala" , "ma" , "kota" ] ;

fun : main ( )
begin
    if tab1 [ 2 ] == 3 then printString ( tab2 [ 0 ] ) else skip ;
    return 0
end
