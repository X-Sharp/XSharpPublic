// 869. IVarGetInfo() returns incorrect results
// https://github.com/X-Sharp/XSharpPublic/issues/1116

FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? IVarGetInfo(o,#iProtect)  // 0 in VO, 1 in X#
? IVarGetInfo(o,#iPrivate)  // 0, OK
? IVarGetInfo(o,#iInstance) // 1 in VO, 2 in X#
? IVarGetInfo(o,#iExport)   // 2 OK
? IVarGetInfo(o,#iAccess)   // 3 OK

xAssert( IVarGetInfo(o,#iProtect)  == 0 )
xAssert( IVarGetInfo(o,#iPrivate)  == 0 )
xAssert( IVarGetInfo(o,#iInstance) == 1 )
xAssert( IVarGetInfo(o,#iExport)   == 2 )
xAssert( IVarGetInfo(o,#iAccess)   == 3 )

CLASS TestClass
EXPORT iExport AS INT
PROTECT iProtect AS INT
INSTANCE iInstance := 123 AS INT
HIDDEN iPrivate AS INT
ACCESS iAccess
RETURN 0
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
