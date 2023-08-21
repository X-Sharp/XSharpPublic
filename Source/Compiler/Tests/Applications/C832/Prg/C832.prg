// 832. Incompatibilities with Type() function
#pragma warnings(9025, off) // return statement
#pragma warnings(219, off) // assigned but not used
#pragma warnings(9098, off) // late binding

FUNCTION Start( ) AS VOID
	LOCAL mo AS custom
	DoTest( "test 1 " + iif( Type("mo")="L", "OK", "Fail:"+Type("mo")+", all declared and not initialized object has value false") )
	LOCAL lu
	DoTest(  "test 2 " + iif(Type("lu")="L", "OK", "Fail:" +Type("lu") +", default value must be .f.") )
	LOCAL lc := collection{}
	DoTest( "test 3 " + iif( Type("lc") ="O" , "OK", "Fail"+Type("lc:prop")+", it isan Object ") )
	DoTest( "test 4 " + iif( Type("lc:prop") ="U" , "OK", "Fail"+Type("lc:prop")) )
	IF AddProperty(lc,"prop",123)
		DoTest( "Test 5 lc: OK prop exist and is correct " + AsString( lc.prop ) )  // the typedef is Long
		xAssert(lc.prop == 123)
		DoTest( "test 6" + iif( Type("lc:prop")<>"U", "OK", "Fail :"+Type("lc.prop")+ ",But lc:prop exist!!!" ) )
	ENDIF
	LOCAL vec [3,4]
	DoTest( "test 7 " + iif( Type("vec")="L", "OK", "Fail:"+ Type("vec") +" must by L not initialized value") )
	DoTest( "test 8 " + iif(Type("vec[1,1]")="L", "OK", "Fail:"+Type("vec[1,1]")+" must by L not initialized value") )
	DoTest( "test 9 " + iif(Type("vec[3]")="L", "OK", "Fail:"+Type("vec[3]")+" must by L not initialized value") )
RETURN

PROC DoTest(c AS STRING)
? c
xAssert(c:Contains("OK") .and. .not. c:Contains("Fail"))

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
