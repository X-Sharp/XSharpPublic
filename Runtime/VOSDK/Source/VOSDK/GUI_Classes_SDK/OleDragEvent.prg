CLASS OleDragEvent INHERIT VObject
//RvdH 030825 This code has been moved from the Ole Classes
        PROTECT hWnd AS PTR
        PROTECT dwDragEvent AS DWORD
        PROTECT pDataObject AS PTR
        PROTECT dwEffect AS DWORD
        PROTECT cObjectName AS STRING
        PROTECT cServerName AS STRING
        PROTECT oPoint AS point

ACCESS DataObject 
        RETURN pDataObject

ACCESS Effect 
        RETURN LONGINT(_CAST, dwEffect)

ASSIGN Effect(dwNewValue) 
        dwEffect := dwNewValue
        RETURN 

CONSTRUCTOR(DragInfo) 
        LOCAL DI AS OleDragEventInfo

        DI := DragInfo

        hWnd := DI:hDocWnd
        pDataObject := DI:pDataObject
        cObjectName := Psz2String(DI:pszObjectName)
        cServerName := Psz2String(DI:pszServerName)
        dwEffect := DI:dwEffect
        oPoint := Point{DI:dwMouseX, DI:dwMouseY}
        RETURN 

ACCESS ObjectName 
        RETURN cObjectName

ACCESS Position 
        LOCAL oWnd AS OBJECT

        oWnd := __WCGetWindowByHandle(hWnd)

        IF IsInstanceOf(oWnd, #Window)
             RETURN __WCConvertPoint(oWnd, oPoint)
        ENDIF
         RETURN NULL_OBJECT

ACCESS ServerName 
        RETURN cServerName

END CLASS

VOSTRUCT OleDragEventInfo
        MEMBER hDocWnd AS PTR
        MEMBER dwDragEvent AS DWORD
        MEMBER pDataObject AS PTR
        MEMBER dwEffect AS DWORD
        MEMBER pszObjectName AS PSZ
        MEMBER pszServerName AS PSZ
        MEMBER dwMouseX AS LONGINT
        MEMBER dwMouseY AS LONGINT
