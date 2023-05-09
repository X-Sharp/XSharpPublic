// test for https://www.xsharp.eu/forum/betatesters/3493-x-2-16-0-1-installer#26220
FUNCTION Start( ) AS VOID
	? Foo{}:Bar()
RETURN

Class Foo
    method Bar as String
        local c1 as String
        c1 := "Robert"
        teststring(c1)
        local procedure teststring ( c as string)
            ? c == "Robert"
        end procedure
        return c1

end class


