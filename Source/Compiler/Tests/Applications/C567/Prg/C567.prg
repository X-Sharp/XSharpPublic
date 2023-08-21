// 567. error XS7038: Failed to emit module 'C567'.
// With build 1.02 this does compile without errors (a warning about the pipes only)
#pragma warnings(9058, off) // lambda with pipes

FUNCTION Start() AS VOID
	LOCAL aRef AS INT[]
	aRef := <INT>{1,3,3,2,5,4}

	System.Array.Sort<INT>(aRef , {|a,b|  iif(a==b , 0 , iif(a > b , 1 , -1) ) })
	System.Array.Sort<INT>(aRef , {a,b => iif(a==b , 0 , iif(a > b , 1 , -1) ) })
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

