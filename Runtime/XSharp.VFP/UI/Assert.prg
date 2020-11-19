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
    IF Set(Set.Asserts) .and. ! glIgnoreAll
        LOCAL sProc := ProcName(1) AS STRING
        LOCAL nLine := ProcLine(1) AS DWORD
        local strMessage as STRING
        if IsString(uMessage)
            strMessage := (STRING) uMessage
        else
            strMessage := "Assertion failed in "+sProc+" line "+nLine:ToString()
        endif
        strMessage  := "Expression: "+cExpression+Environment.NewLine+strMessage
        LOCAL oDlg as AssertDialog
        oDlg := AssertDialog{}
        oDlg:Text := "Assertion failed"
        oDlg:Message := strMessage
        oDlg:ShowDialog()
        SWITCH oDlg:Result
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
    RETURN
