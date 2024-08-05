
// https://github.com/X-Sharp/XSharpPublic/issues/1483
class Days
exported:
    var date

    inline method init
        ::date := stod("20220101")
    return self

    inline access assign method date(dDate)
        if Valtype(dDate) == "D"
            ::date := dDate
        endif
    return ::date
endclass

procedure main()
    LOCAL oD := Days():new()
    ? oD:date
    xAssert(oD:date ==2022.01.01)
    ? oD:date := stod("20220101")+3
    xAssert(oD:date ==2022.01.04)
return


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
