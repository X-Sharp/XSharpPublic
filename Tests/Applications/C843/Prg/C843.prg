// 843. Code snippets causing the compiler to freeze
// test with /vo4 , /vo11 and without
FUNCTION Start() AS VOID
?  CToD( "01/01/" + Str( Year( Today() ) + 1, 4 ) ) - ( Today() ) 

LOCAL nBetrag AS USUAL
LOCAL nRet AS FLOAT
LOCAL nKurs AS FLOAT
LOCAL nBasis AS FLOAT    
nKurs := 1
nRet := Round( (nBetrag * nBasis ) / nKurs, 2 )

LOCAL _dwHPointUp AS INT
LOCAL nStartX := 0 AS INT
? _dwHPointUp - (nStartX - LoWord( 123 ) ) 
