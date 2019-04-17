// 672. error XS9055: Codeblocks cannot be declared with the Lambda Expression Syntax.
// error happens only when using the vulcan rutnime dlls
FUNCTION Start( ) AS VOID
    LOCAL nArea AS DWORD
    LOCAL a AS ARRAY
    DBCreate("test" , {{"TEST","C",10,0}})
    DBUseArea(,,"test")
	nArea := Select()
	? nArea
	xAssertTrue(nArea == 1)
    ? (nArea)->(EOF())
	xAssertTrue( (nArea)->(EOF()) )
    a := {(nArea)->(EOF())}
    ? a[1]
	xAssertTrue( a[1] )
RETURN

PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

