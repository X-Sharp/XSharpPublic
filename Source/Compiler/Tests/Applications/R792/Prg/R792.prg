// https://github.com/X-Sharp/XSharpPublic/issues/725
FUNCTION Start( ) AS VOID
    LOCAL o AS OBJECT
    o := (DECIMAL)1.01
    
    SWITCH (DECIMAL)o
    CASE 0
        ? "0"
    CASE 1
        ? "1"
    CASE 2
        ? "2"
    CASE 3
        ? "3"
    CASE 4
        ? "4"
    CASE 5
        ? "5"
    CASE 1000
        ? "1000"
    CASE 1.1m
        ? "1.1"
    CASE 1.01m
        ? "1.01"
    END SWITCH
RETURN
