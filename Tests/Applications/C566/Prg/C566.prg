// 566. error XS0121: The call is ambiguous between the following methods or 
// properties: 'Array.Sort<T>(T[], IComparer<T>)' and 'Array.Sort<T>(T[], Comparison<T>)'
// error is reported only in VO/Vulcan dialect, there's no ambiguouity error when compiling in Core dialect.
FUNCTION Start() AS VOID
	LOCAL aRef AS INT[]
	aRef := <INT>{1,3,3,2,5}
	System.Array.Sort<INT>(aRef , {|a,b| iif(TRUE , 0 , 1) })
	System.Array.Sort<INT>(aRef , {|a,b| => iif(TRUE , 0 , 1) })
	System.Array.Sort<INT>(aRef , {a,b => iif(TRUE , 0 , 1) })


	System.Array.Sort<INT>(aRef , {|a,b| => iif(a==b , 0 , iif(a > b , 1 , -1) ) })
	TypeArray(aRef)
RETURN

FUNCTION TypeArray(a AS INT[]) AS VOID
	LOCAL nPrev := 0 AS INT
	FOREACH n AS INT IN a
		? n
		IF n < nPrev
			THROW Exception{"Incorrect sort"}
		END IF
		nPrev := n
	NEXT
RETURN 

