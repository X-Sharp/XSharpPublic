// 962. Readonly fields in XBase++ dialect [#1490,#1491]
// https://github.com/X-Sharp/XSharpPublic/issues/1490
// https://github.com/X-Sharp/XSharpPublic/issues/1491

PROCEDURE main()
//	LOCAL o AS Example
	LOCAL o
	o := Example():new(4)

    TRY
	    o:nNumber := 10 // should cause a runtime error
	    xAssert(FALSE)
    CATCH
	    xAssert(TRUE)
    END TRY

    ? o:nNumber
    xAssert( o:nNumber == 4 )

    o:SetNumber(15)
    ? o:nNumber
    xAssert( o:nNumber == 15 )
RETURN


CLASS Example
exported:
    VAR nNumber assignment HIDDEN

    inline METHOD init(nNumber)
        ::nNumber := nNumber
    RETURN

    inline METHOD SetNumber(nNumber)
        ::nNumber := nNumber
    RETURN SELF
endclass

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
