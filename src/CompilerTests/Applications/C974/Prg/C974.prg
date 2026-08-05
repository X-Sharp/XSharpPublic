// 974. Problems with the /fox3 option - codeblocks and other tests
// // https://github.com/X-Sharp/XSharpPublic/issues/2025

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS StaticClass
	STATIC EXPORT StaticMember := 123 AS INT
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL cb AS USUAL
	
	LOCAL u AS USUAL
	u := "abc"
	
	cb := { || u.Length }
	? Eval( cb )
	xAssert( Eval( cb ) == 3 )

	cb := { | uu | uu.Length }
	? Eval( cb, "test" )
	xAssert( Eval( cb, "test" ) == 4 )

	cb := { | uu | uu.ToUpperInvariant() }
	? Eval( cb, "test" )
	xAssert( Eval( cb, "test" ) == "TEST" )

	cb := { | uu | uu.ToUpperInvariant().Length }
	? Eval( cb, "test" )
	xAssert( Eval( cb, "test" ) == 4 )

	cb := { |  | StaticClass.StaticMember }
	? Eval( cb )
	xAssert( Eval( cb ) == 123 )

	PUBLIC mmm
	mmm := "testing"

	cb := { | | mmm.Length }
	? Eval( cb )
	xAssert( Eval( cb ) == 7 )

	cb := { | | mmm.ToUpperInvariant() }
	? Eval( cb )
	xAssert( Eval( cb ) == "TESTING" )

	LOCAL cDbf AS STRING
	cDbf := "fox3test"
	
	? DbCreate( cDbf, {{"FLD1","C",4,0}},"DBFVFP")
	
	? DbUseArea(TRUE,"DBFVFP",cDbf, "fox3test")
	fox3test -> DbAppend()
	fox3test->fld1 := "test"
	fox3test.fld1 := "test"
	
	? Eval( { || fox3test->fld1 } )
	xAssert( Eval( { || fox3test->fld1 } ) == "test" )
	xAssert( Eval( { || fox3test.fld1 } ) == "test" )

	? Eval( { || fox3test->fld1.Length } )
	xAssert( Eval( { || fox3test->fld1.Length } ) == 4 )
	xAssert( Eval( { || fox3test.fld1.Length } ) == 4 )
	
	? &("fox3test->fld1")
	xAssert( &("fox3test->fld1") == "test" )
	xAssert( &("fox3test.fld1") == "test" )
	
	DbCloseArea()

RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN nil
