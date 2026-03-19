USING XSharp.VFP

ENUM XSharp.VFP.AssertResult
    MEMBER None
    MEMBER Debug
    MEMBER Cancel
    MEMBER Ignore
    MEMBER IgnoreAll
END ENUM


STATIC GLOBAL glIgnoreAll := FALSE as LOGIC

FUNCTION __FoxAssert(lExpression as LOGIC, cExpression as STRING, uMessage := NIL as USUAL) AS VOID
    IF XSharp.RT.Functions.Set(Set.Asserts) .and. ! glIgnoreAll
        LOCAL sProc := ProcName(1) AS STRING
        LOCAL nLine := ProcLine(1) AS DWORD
        local strMessage as STRING

        if IsString(uMessage)
            strMessage := (STRING) uMessage
        else
            strMessage := "Assertion failed in "+sProc+" line "+nLine:ToString()
        endif
        strMessage  := "Expression: "+cExpression+Environment.NewLine+strMessage

        LOCAL eResult AS AssertResult
        eResult := VfpUIService.Provider:ShowAssertDialog(cExpression, strMessage)

        SWITCH eResult
        CASE AssertResult.None
            NOP
        CASE AssertResult.Debug
            System.Diagnostics.Debugger.Break()
        CASE AssertResult.Cancel
            _Quit()
        CASE AssertResult.Ignore
            NOP
        CASE AssertResult.IgnoreAll
            glIgnoreAll := TRUE
        END SWITCH
    ENDIF
END FUNCTION
