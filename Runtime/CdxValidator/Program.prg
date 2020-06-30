USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    VAR files := {"Corrupt1.Dbf", "Corrupt2.Dbf", "Corrupt3.Dbf"}
    RddSetDefault("DBFCDX")
    FOREACH VAR cFile IN files
        DbCloseAll()
        ? cFile
        DbUseArea(TRUE, "DBFCDX", cFile)
        FOR VAR i := 1 TO DbOrderInfo(DBOI_ORDERCOUNT)
            DbSetOrder(i)
            DbOrderInfo(DBOI_DUMP)
            ? OrdName(), "Valid " ,DbOrderInfo(DBOI_VALIDATE)
        NEXT
        DbCloseArea()
    NEXT

    WAIT
	RETURN	
