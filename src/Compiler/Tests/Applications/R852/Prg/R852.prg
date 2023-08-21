//  https://github.com/X-Sharp/XSharpPublic/issues/1054
FUNCTION Start( ) AS VOID
    ? TestMe(String2Psz("SomeString"))
    WAIT
RETURN



FUNCTION TestMe(dw as DWORD) AS DWORD
    return dw
