FUNCTION Start() AS VOID
	LOCAL cDBF AS STRING
	LOCAL aFields AS ARRAY
	LOCAL i AS DWORD
        LOCAL nStart, nElapsed AS FLOAT
	RddSetDefault ( "DBFCDX" )

	cDbf := "C:\test\mytestxs"    // change the dbf to "C:\test\mytestVO"  to test in VO 2.8.
	FErase ( cDbf + ".cdx" )

	aFields := { { "LAST" , "C" , 20 , 0 } }


	DbCreate( cDBF , AFields)
	DbUseArea( ,,cDBF,,FALSE )

        nStart := Seconds()

       ? "start : " + ntrim(nStart)

	FOR i := 1 UPTO 1000000
		DbAppend()
        FIELDPUT(1, Str(i,10,0))
	NEXT

       nElapsed := Seconds() - nStart

       ? "Elapsed: " + NTrim(nElapsed) + " seconds"

	DbCloseArea()

    wait
RETURN
