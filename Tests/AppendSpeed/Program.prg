USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID
    LOCAL cDBF AS STRING 
    LOCAL aFields AS ARRAY 
    LOCAL i AS DWORD
    
    LOCAL nStart, nElapsed AS FLOAT
    
    RddSetDefault ( "DBFNTX" ) 
    SetAnsi(TRUE)
    cDbf := "C:\test\mytest3x" 
    //FErase ( cDbf + ".cdx" )
    
    aFields := { { "LAST" , "C" , 20 , 0 } } 
    
    
    DbCreate( cDBF , AFields)
    DbUseArea( ,,cDBF )	
    
    
    nStart := Seconds()
    
    ? "start : " + ntrim(nStart)
    //   DBCreateOrder("last", cDbf, "last")
    
    FOR i := 1 UPTO 100_000
        DbAppend() 		
        fieldput(1, Str(i,10,0))
    NEXT
    //	DBCreateOrder("last", cDbf, "last")
    //	DbOrderInfo(DBOI_USER+42)
    
    nElapsed := Seconds() - nStart
    
    ? "Elapsed: " + NTrim(nElapsed) + " seconds"
    
    
    DbCloseArea()
    DbCloseAll()
    
    wait
    RETURN
