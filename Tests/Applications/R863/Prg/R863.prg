// https://github.com/X-Sharp/XSharpPublic/issues/1073
// https://github.com/X-Sharp/XSharpPublic/issues/1077

#xtranslate ASTR(<x>) => ALLTRIM(STR(<x>))
#translate PN(<(pole)>) => GF(<(pole)>)

FUNCTION Start( ) AS VOID
    local n, fldleft
    fldleft := "123"
    xAssert(astr(n := val(fldleft)) == fldleft)
    if astr(n := val(fldleft)) == fldleft
        ? n
    endif
    xAssert(PN(NAME)=="NAME")
    ? PN(NAME)

RETURN

FUNCTION GF(cValue) as String
    return cValue

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
