CLASS Foo
METHOD Bar(nDisplayMode := FooBar.One AS FooBar, lSecond := FALSE AS LOGIC) AS VOID
	? nDisplayMode, lSecond
	RETURN 
END CLASS
	
ENUM FooBar 
	MEMBER None
	MEMBER One
	MEMBER Two
END ENUM      

FUNCTION start() AS VOID
	Foo{}:Bar()
	RETURN
	
