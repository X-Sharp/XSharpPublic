GLOBAL ga := 42*42 ,gb := 42 AS INT, gc := "Robert" AS STRING
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
	? Test{}
RETURN


CLASS Test     
    EXPORT Foo, Bar AS STRING
    EXPORT Foo1, Bar1 := 42 AS INT, FooBar := 10 AS DWORD
END CLASS    

