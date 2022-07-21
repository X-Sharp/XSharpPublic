#xtranslate ASTR(<x>) => ALLTRIM(STR(<x>))
#translate PN(<(pole)>) => GF(<(pole)>)

FUNCTION Start( ) AS VOID
//    local n, fldleft
//    fldleft := "123"
//    if astr(n := val(fldleft)) == fldleft
//        ? n
//    endif
    ? PN(NAME)

RETURN

FUNCTION GF(cValue) as String
    return cValue
