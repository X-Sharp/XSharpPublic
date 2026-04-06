// 960. Incompatibility with Object.ToString() [#1286]
// https://github.com/X-Sharp/XSharpPublic/issues/1286

#pragma options (vo5 , on) // CLIPPER calling convention
PROCEDURE Main()
    LOCAL o

    o := Example():new(5)
    ? o:ToString()

    xAssert( o:ToString() == "5" )

    RETURN

CLASS Example
EXPORTED:
    INLINE METHOD Init(b)
        ::b := b
    RETURN SELF

    INLINE NEW METHOD ToString()
    RETURN AsString(::b)

    VAR b
ENDCLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
