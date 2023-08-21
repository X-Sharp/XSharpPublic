// R772: Cannot use DATE in VOSTRUCT

VOSTRUCT MyStruct
MEMBER n AS INT
MEMBER s AS SYMBOL // ok
MEMBER d AS DATE // error           
MEMBER l AS LOGIC       
MEMBER p AS PSZ
MEMBER pt AS PTR  
MEMBER u IS utest


UNION utest
    MEMBER l AS LONG
    MEMBER d AS DWORD
    MEMBER s AS SYMBOL
    MEMBER dt AS DATE    
    MEMBER r8 AS REAL8
    MEMBER r4 AS REAL4

FUNCTION Start() AS VOID
    LOCAL s IS MyStruct
    s:n := 42
    s:u:l := 4242
    RETURN

