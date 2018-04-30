function Start() as void
	local nX as dword
	? Version()
	? "Size of IntPtr", IntPtr.Size
	? "Size of USUAL", SizeOf(USUAL)
	? "Size of FLOAT", SizeOf(FLOAT)
	? "Size of DATE", SizeOf(DATE)
	? "Size of SYMBOL", SizeOf(SYMBOL)
	? 
	? SetNatDLL("Italian.DLL")
	? GetNatDLL()
	? __CavoStr(VOErrors.TMSG_PRESSANYKEY)
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

