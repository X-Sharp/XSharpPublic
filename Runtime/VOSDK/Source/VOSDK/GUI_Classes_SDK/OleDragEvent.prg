/// <include file="Gui.xml" path="doc/OleDragEvent/*" />
CLASS OleDragEvent INHERIT VObject
//RvdH 030825 This code has been moved from the Ole Classes
        PROTECT hWnd AS PTR
        PROTECT dwDragEvent AS DWORD
        PROTECT pDataObject AS PTR
        PROTECT dwEffect AS DWORD
        PROTECT cObjectName AS STRING
        PROTECT cServerName AS STRING
        PROTECT oPoint AS point


/// <include file="Gui.xml" path="doc/OleDragEvent.DataObject/*" />
ACCESS DataObject 
        RETURN pDataObject


/// <include file="Gui.xml" path="doc/OleDragEvent.Effect/*" />
ACCESS Effect 
        RETURN LONGINT(_CAST, dwEffect)


/// <include file="Gui.xml" path="doc/OleDragEvent.Effect/*" />
ASSIGN Effect(dwNewValue) 
        dwEffect := dwNewValue
        RETURN 


/// <include file="Gui.xml" path="doc/OleDragEvent.ctor/*" />
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


/// <include file="Gui.xml" path="doc/OleDragEvent.ObjectName/*" />
ACCESS ObjectName 
        RETURN cObjectName


/// <include file="Gui.xml" path="doc/OleDragEvent.Position/*" />
ACCESS Position 
        LOCAL oWnd AS OBJECT


        oWnd := __WCGetWindowByHandle(hWnd)


        IF IsInstanceOf(oWnd, #Window)
             RETURN __WCConvertPoint(oWnd, oPoint)
        ENDIF
         RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/OleDragEvent.ServerName/*" />
ACCESS ServerName 
        RETURN cServerName


END CLASS


/// <exclude/>
VOSTRUCT OleDragEventInfo
        MEMBER hDocWnd AS PTR
        MEMBER dwDragEvent AS DWORD
        MEMBER pDataObject AS PTR
        MEMBER dwEffect AS DWORD
        MEMBER pszObjectName AS PSZ
        MEMBER pszServerName AS PSZ
        MEMBER dwMouseX AS LONGINT
        MEMBER dwMouseY AS LONGINT
