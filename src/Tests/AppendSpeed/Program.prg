USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Startx() AS VOID
    SetCollation(#Clipper)
    SetNatDLL("GERMAN")
    UseBufferedFileStream(TRUE)
    ? DbUseArea(TRUE, "DBFCDX", "C:\SeekTester\artikel.dbf",,FALSE)
    ? OrdSetFocus("ARTNR" )
    ? DbSeek("SA50_FX1000", FALSE)
//    ? DbSeek("SA50_FX_Z", FALSE)
//    ? DbSeek("SA50_P_1", FALSE)
    WAIT
	RETURN


FUNCTION Start AS VOID
    TRY
    ? "Test buffered"
    UseBufferedFileStream(TRUE)
    Test()
    ? "Test unbuffered"
    UseBufferedFileStream(FALSE)
    Test()

    CATCH e AS Exception
        ? e:ToString()
    END TRY
        WAIT
    RETURN

FUNCTION Test() AS VOID
    LOCAL cDBF AS STRING
    LOCAL aFields AS ARRAY
    LOCAL i AS DWORD
    LOCAL nStart, nElapsed AS FLOAT

    RddSetDefault ( "DBFNTX" )
    SetAnsi(TRUE)
    cDbf := "C:\test\mytestx"
    //FErase ( cDbf + ".cdx" )

    aFields := { { "LAST" , "C" , 20 , 0 } }


    DbCreate( cDBF , AFields)
    DbUseArea( ,,cDBF )


    nStart := Seconds()

    ? "start : " + ntrim(nStart)
    //   DBCreateOrder("last", cDbf, "last")

    FOR i := 1 UPTO 500_000
        DbAppend()
        fieldput(1, Str(i,10,0))
    NEXT
    //	DBCreateOrder("last", cDbf, "last")
    //	DbOrderInfo(DBOI_USER+42)

    nElapsed := Seconds() - nStart

    ? "Elapsed: " + NTrim(nElapsed) + " seconds"
    ? "# of rows added", LastRec()


    DbCloseArea()
    DbCloseAll()

    RETURN
