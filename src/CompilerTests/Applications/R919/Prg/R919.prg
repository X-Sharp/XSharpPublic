
FUNCTION Start( ) AS VOID
	local e as object
	e := Exception{}
	? "Exception"
	if e is Exception
	    ? "Exception"
	endif
	if e is not Exception
	    ? "Not Exception"
	endif
	if e is not null
	    ? "Not null"
	endif
	e := "123"
	? "String"
	if e is Exception
	    ? "Exception"
	endif
	if e is not Exception
	    ? "Not Exception"
	endif
	if e is not null
	    ? "Not null"
	endif
	e := null
	? "NULL_OBJECT"
	if e is Exception
	    ? "Exception"
	endif
	if e is not Exception
	    ? "Not Exception"
	endif
	if e is not null
	    ? "Not null"
	endif
    if e is null
	    ? "null"
	endif
	Console.ReadLine()

RETURN
