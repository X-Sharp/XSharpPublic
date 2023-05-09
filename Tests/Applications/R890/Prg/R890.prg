// test for https://www.xsharp.eu/forum/betatesters/3493-x-2-16-0-1-installer#26220
#define XsIsGreat "X# is great"
FUNCTION Start( ) AS VOID
	? Foo{}:Bar()
	xAssert(Foo{}:Bar() == XsIsGreat)
RETURN

Class Foo
    method Bar as String
        local c1 as String
        c1 := XsIsGreat
        teststring(c1)
        local procedure teststring ( c as string)
            ? c == XsIsGreat
            Xassert(c == XsIsGreat)
        end procedure
        return c1

end class

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN


