// R777 IS Pattern Check with USUAL values
// Also Switch Pattern Check with USUAL values
// #https://github.com/X-Sharp/XSharpPublic/issues/636

FUNCTION Start( ) AS VOID

    FOR VAR i := 1 TO 3
        ? i
    LOCAL uValue := GetObject(i)
    IF uValue IS Error VAR oError
        ? "Error", oError:Message
        xAssert(i == 1 .or. i == 3)
    ELSEIF uValue IS STRING VAR strValue
        ? "String", strValue
        xAssert(i == 2)
    ENDIF
    SWITCH uValue
    CASE oError AS Error WHEN oError:Message == "Error"
        ? "Error 1", oError:Message
        xAssert(i == 3)
    CASE oError AS Error WHEN oError:Message == "test"
        ? "Error 2", oError:Message
        xAssert(i == 1)
    CASE strValue AS STRING
        ? "String", strValue
        xAssert(i == 2)
    END SWITCH
    NEXT


RETURN




FUNCTION GetObject(nType AS INT) AS USUAL
    SWITCH nType
    CASE 1
        RETURN Error{"test"}
    CASE 3
        RETURN Error{"Error"}
    CASE 2
        RETURN "abc"
    END SWITCH
    RETURN OBJECT{}

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
