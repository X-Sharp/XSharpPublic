// RddExtensions.prg
// Created by    : robert
// Creation Date : 1/30/2024 1:01:58 PM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text

INTERNAL STATIC CLASS RddExtensions
    STATIC METHOD _dbfError(self area as Workarea, ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(ex, iSubCode, iGenCode, String.Empty, ex?:Message, XSharp.Severity.ES_ERROR, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, iSubCode AS DWORD, iGenCode AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR, lThrow )

    STATIC METHOD _dbfError(self area as Workarea, ex AS Exception,iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(ex, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(NULL, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(ex, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, lThrow := TRUE AS LOGIC) AS VOID
        area:_dbfError(NULL, iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR, lThrow)

    STATIC METHOD _dbfError(self area as Workarea, ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
        LOCAL oError AS RddError
        //
        IF ex is RddError VAR oRddError
            oError := oRddError
        ELSE
            IF ex != NULL
                oError := RddError{ex,iGenCode, iSubCode}
            ELSE
                oError := RddError{iGenCode, iSubCode}
            ENDIF
            oError:CanDefault := TRUE
            oError:SubSystem := area:Driver
            oError:Severity := iSeverity
            oError:FuncSym  := IIF(strFunction == NULL, "", strFunction) // code in the SDK expects all string properties to be non-NULL
            oError:FileName := area:FileName
            IF String.IsNullOrEmpty(strMessage)  .AND. ex != NULL
                strMessage := ex:Message
            ENDIF
            IF String.IsNullOrEmpty(strMessage)
                IF oError:SubCode != 0
                    oError:Description := oError:GenCodeText + " (" + oError:SubCodeText+")"
                ELSE
                    oError:Description := oError:GenCodeText
                ENDIF
            ELSE
                oError:Description := strMessage
            ENDIF
        ENDIF
        RuntimeState.LastRddError := oError
        //
        IF lThrow
            THROW oError
        ENDIF
        RETURN


END CLASS
