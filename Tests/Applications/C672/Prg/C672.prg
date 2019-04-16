// 672. error XS9055: Codeblocks cannot be declared with the Lambda Expression Syntax.
// error happens only when using the vulcan rutnime dlls
FUNCTION Start( ) AS VOID
    LOCAL nArea AS DWORD
    LOCAL a AS ARRAY
    DBCreate("test" , {{"TEST","C",10,0}})
    DBUseArea(,,"test")
	nArea := Select()
	? nArea
    ? (nArea)->(EOF())
    a := {(nArea)->(EOF())}
    ? a[1]
RETURN
