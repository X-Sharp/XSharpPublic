USING System.Globalization

function Start as void
	LOCAL cA, cB AS STRING
	LOCAL nI, nMax AS Int64
	LOCAL lOk AS LOGIC
	LOCAL p AS PTR
	p := MemAlloc(100)
	MemFree(p)
	SetInternational(#Clipper)
	SetNatDLL("ITALIAN")
	? DateTime.Now:DayOfWeek:ToString()
	? (INT) DateTime.Now:DayOfWeek
	? DOW(Today())
	? CDOW(Today())
	? CMonth(Today())
	wait

	nMax := 10000000// 40000000
	cA := "THE QUICK BROWN FOX JUMP OVER THE LAZY DOG"
	//cB := Left(CA, Slen(Ca)-1)+Lower(right(cA,1))
	cb := lower(ca)
	LOCAL aColl AS STRING[]
	aColl := <STRING>{"Windows", "Clipper","Unicode", "Ordinal"}
	FOREACH VAR s IN aColl
		SetCollation(s)
		LOCAL nSecs AS float
		nSecs := Seconds()
		FOR nI := 1 TO nMax 
			lOk := cA <= cB
		NEXT
		? SetCollation(), transform(nMax,"999,999,999"), seconds() - nSecs
	NEXT
	_wait()
	return 

function Start2 as void
	LOCAL aDir AS ARRAY
	aDir := Directory("C:\XSharp\DevRt\Runtime", "AHSD")
	FOREACH VAR aFile IN aDir
		? aFile[1], aFile[2], aFile[3], aFile[4], aFile[5]
	NEXT
	LOCAL aFiles AS ARRAY

	aFiles := ArrayCreate(ADir("C:\XSharp\DevRt\Runtime\XSharp.Core\*.prg"))

	ADir("C:\XSharp\DevRt\Runtime\XSharp.Core\*.prg", aFiles)

	AEval(aFiles, {|element| QOut(element)})

	_Wait()
	RETURN

function StartA() as void
	LOCAL nX AS DWORD
	CultureInfo.DefaultThreadCurrentCulture := CultureInfo{"EN-us"}
	CultureInfo.DefaultThreadCurrentUICulture := CultureInfo{"EN-us"}

	? Version()
	? "Size of IntPtr", IntPtr.Size
	? "Size of USUAL", SizeOf(USUAL)
	? "Size of FLOAT", SizeOf(FLOAT)
	? "Size of DATE", SizeOf(DATE)
	? "Size of SYMBOL", SizeOf(SYMBOL)
	? 
	? 1.234:ToString()
	? SetNatDLL("Dutch.DLL")
	? GetNatDLL()
	? GetAppLocaleID()
	? SetAppLocaleID(1043)
	? GetAppLocaleID()
	? 1.234:ToString()
	? __CavoStr(VOErrors.TMSG_PRESSANYKEY)
	? VO_Sprintf(VOErrors.__WCSLOADLIBRARYERROR, "CaTo3Cnt.DLL")
	? DosErrString(2)
	? ErrString(EG_ARG)
	Console.WriteLine("")
	LOCAL mem,mem2 AS INT64
	mem := GC.GetTotalMemory(TRUE)

	LOCAL a AS ARRAY 
	a := ArrayCreate(1000000)

	mem2 := GC.GetTotalMemory(TRUE) - mem
	mem := mem2
	? "Memory for 1M element ARRAY", mem:ToString("###,### bytes")
	for nX := 1 to 1000000
		a[nX] := 1
	NEXT
	? "Memory for 1M element ARRAY after assigning 1M values", mem:ToString("###,### bytes")
	a := NULL_ARRAY
	GC.Collect()
	a := ArrayCreate(0)
	mem := GC.GetTotalMemory(TRUE)
	FOR nX := 1 TO 1000000
		aadd(a, 1)
	NEXT
	mem2 := GC.GetTotalMemory(TRUE) - mem
	mem := mem2
	? "Memory for 1M element ARRAY after assigning 1M values with AAdd", mem:ToString("###,### bytes")
	
	TestUsualFloat()
	TestDate()
	GC.KeepAlive(a)
	
    _wait()
    RETURN 
    
PROCEDURE TestUsualFloat()
	LOCAL u1,u2 AS USUAL 
	LOCAL f1,f2 AS FLOAT 
	
    ? "Testing USUAL & Float"
	? "25.000.000 iterations"
	
	LOCAL d AS DateTime
	d := DateTime.Now
	
	FOR LOCAL n := 1 AS INT UPTO 25000000
		u1 := 1.1d
		f1 := 1.3d
		u2 := u1 + f1
		f2 := u1 + u2 + f1
		f1 := f2 + u1
		f2 := f2 + u2
	NEXT

	? "Time elapsed:", (DateTime.Now - d):ToString()
	?
    RETURN



PROCEDURE testDate()
	local d1, d2 as date	
    ? "Testing DATE"
	? "25.000.000 iterations"
	
	LOCAL d AS DateTime
	d := DateTime.Now
	
	for local n := 1 as int upto 25000000
		d1 := 2018.04.15
		d2 := d1+1
		d1 := d2

	NEXT

	? "Time elapsed:", (DateTime.Now - d):ToString()
	?
    RETURN

