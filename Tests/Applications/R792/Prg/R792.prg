
FUNCTION Start( ) AS VOID
    LOCAL o AS OBJECT
    o := (DECIMAL)1
    
    SWITCH (DECIMAL)o
    CASE 1
        ? "1"
    CASE 2
        ? "2"
    CASE 3
        ? "3"
    END SWITCH
RETURN
