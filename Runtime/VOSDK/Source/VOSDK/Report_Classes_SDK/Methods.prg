CLASS DDEMC
	EXPORT isModal AS LOGIC
	EXPORT cCmd AS STRING
	

CONSTRUCTOR(lModal, cCommandString) 
	isModal := lModal
	cCmd := cCommandString
	RETURN 
END CLASS

CLASS HiddenWndClass INHERIT ChildAppWindow
	HIDDEN oParentWnd AS OBJECT //ReportQueue
	

METHOD Dispatch(oEvent) 
	// dispatch events to the DataUpdate method of the ReportQueue class
	LOCAL oWin AS ReportQueue
	
	oWin := SELF:ParentWnd
	
	IF (oWin != NULL_OBJECT)
		oWin:DataUpdate(oEvent)
	ENDIF
	
	RETURN SUPER:Dispatch(oEvent)
	

CONSTRUCTOR(oOwner, lManaged, lImpl) 
    
    SUPER(oOwner, lManaged, lImpl)


RETURN 

ACCESS ParentWnd() 
	RETURN(oParentWnd)
	

ASSIGN ParentWnd(oWindow) 
	RETURN(oParentWnd := oWindow)
	
END CLASS

CLASS RCError INHERIT Error
	

CONSTRUCTOR(VAR, varnum) 
    SUPER() // dcaton 070307
	subsystem := "Report Class"
	
	FuncSym := String2Symbol(ProcName(1))
	
	description := __CavoStr(__CAVOSTR_REPORTCLASS_PARAMERROR)
	
	IF !IsNil(VAR) .OR. !IsNil(varnum)
		arg := AsString(VAR)
		argtype := UsualType(VAR)
	ENDIF
	
	IF !IsNil(varnum)
		Argnum:=varnum
	ENDIF
	
	args          := {}
	tries         := 1
	RETURN 
END CLASS

CLASS ReportQueue //INHERIT IpcClient
	HIDDEN RETname AS STRING    // The *.RET file for commands being Queued
	HIDDEN oOwner                // object to be told about ReportClosed
	
	HIDDEN hDbConn AS DWORD     // current database handle
	HIDDEN hWrmHandle AS DWORD  // handle for created report
	HIDDEN dwAdviseId AS DWORD  // Advise ID for this object
	
	HIDDEN oHiddenRptWnd AS HiddenWndClass // hidden window object
	
	HIDDEN servername AS STRING     // "Report Editor"
	HIDDEN aCommandQ AS ARRAY       // of DDE commands pending EXECUTES
	HIDDEN aOpenNames AS ARRAY      // Reports now Open
	HIDDEN nOpenIndex AS INT        // The file index now used by CA-RET
	HIDDEN sTopic, sItem AS STRING  // DDE Item attached to Ipc Topic
	HIDDEN MonitorIsOn AS LOGIC     // Init sets FALSE
	// QorExecute sets TRUE and asks CA-RET to advise on events before it opens first file
	HIDDEN ReportQueueIsBusy AS LOGIC  // Init sets false
	// QorExecute sets TRUE before modal (alias asynch) DDE commands
	// DataUpdate sets FALSE after notification of asynch DDE command completion
	HIDDEN CloseMethodCalled AS LOGIC // set in __ExecuteNow method
	// DataUpdate should ignore 'View.Close' if it was user-initiated via CA-RET menu
	HIDDEN _event // set in DataUpdate, accessible as EventType in oWindow:ReportNotification
//	HIDDEN err // set in ClientError, accessible as ErrorType in oWindow:ReportException
	HIDDEN bTermWrappers AS LOGIC // WrmTerminate() will send a WRM_NTFY_CLOSE so don't process that message.
	HIDDEN sPreviewTitle AS STRING // Preview title.
	HIDDEN bMDIChild AS LOGIC // make the preview window an MDI child.
	
ACCESS OpenNames AS ARRAY
RETURN aOpenNames

ACCESS OpenIndex AS INT
RETURN nOpenIndex

METHOD __CreateDefinerWindow(cReportName, iWindowType, cSQLStatement, iReportStyle) 
	LOCAL rWrmDefInit IS CAWRMCREATEDEFINIT
	LOCAL char1 AS STRING
	LOCAL iCharLoc AS INT
	LOCAL hActiveWnd AS PTR
	
	IF !IsString(cReportName)
		// handle error
		Eval(ErrorBlock(), RCError{cReportName,1})
	ENDIF
	
	IF (!IsString(cSQLStatement) .AND. (iWindowType == FILE_NEW))
		// handle error
		Eval(ErrorBlock(), RCError{cSQLStatement,3})
	ENDIF
	
	IF (iReportStyle == NIL)
		iReportStyle := RPTSTYLE_NOTSPECIFIED // default: prompt for style
	ENDIF
	
	rWrmDefInit:hWndNotify := oHiddenRptWnd:Handle()
	rWrmDefInit:wMessageBase := WRM_NTFY_MSG_BASE
	rWrmDefInit:hCqmHandle := hCqmHandle    // handle returned from CqmInitialize
	rWrmDefInit:hRccHandle := hRccHandle    // handle returned from RccInitialize
	
	rWrmDefInit:hDlgParent := oHiddenRptWnd:Handle()
	
	IF (iWindowType == FILE_OPEN)
		// open the .ret file in a HIDDEN definer window
		rWrmDefInit:BD:bddwFlags := _OR(rWrmDefInit:BD:bddwFlags, 2)
	ENDIF
	// otherwise it's a definer window; i.e. New()
	
	rWrmDefInit:hDbHandle := hDbConn
	
	IF (iWindowType = FILE_OPEN)
		// mark that we are passing in a .RET file name.
		rWrmDefInit:eReportType := RET_REPORT_OPEN
		rWrmDefInit:eFileType := RET_FILE_NAME
		rWrmDefInit:lpsFileName := String2Psz(cReportName)
	ELSE
		// mark that we are creating a new file
		rWrmDefInit:eReportType := RET_REPORT_CREATE
		IF ((iReportStyle < RPTSTYLE_NOTSPECIFIED) .OR. (iReportStyle > RPTSTYLE_CROSSTAB))
			iReportStyle := RPTSTYLE_NOTSPECIFIED // default: prompt for style
		ENDIF
		rWrmDefInit:eDefReportStyle := iReportStyle
		rWrmDefInit:eQueryType := RET_QUERY_CQMSQLSTRING
		rWrmDefInit:lpstrSQL := cSQLStatement //String2Psz("SELECT CDS.* FROM D:\DBFS\CDS")
	ENDIF
	
	rWrmDefInit:BD:bddwFlags := _OR(rWrmDefInit:BD:bddwFlags, 128)
	
	rWrmDefInit:BD:bddwFlags := _OR(rWrmDefInit:BD:bddwFlags, 256)
	
	// rWrmDefInit.BD.bdNtfySave := TRUE // TRUE to get notification about Save
	rWrmDefInit:BD:bddwFlags := _OR(rWrmDefInit:BD:bddwFlags, 4096)
	
	CloseMethodCalled := FALSE
	
	IF  ((iCharLoc := INT(RAt("\", cReportName))) != 0)   // strip off path + device
		char1 := SubStr(cReportName, iCharLoc+1)
	ELSEIF  ((iCharLoc := INT(RAt(":", cReportName))) != 0)   // strip off device
		char1 := SubStr(cReportName, iCharLoc+1)
	ELSE
		char1 := cReportName // MALRO02 10/10/96 no path or device
	ENDIF
	
	
	// Since we are creaating a definer window, but it is hidden,
	// focus will go to that hidden window.
	// Force it back to the last active window.
	hActiveWnd := GetActiveWindow()
	
	// when creating a preview window, a WRM_NTFY_PREVIEW message
	// will be sent to the notify window, and there the preview
	// window will be created.
	WrmCreateDefinerWindow( hWpHandle, @rWrmDefInit )
	
	SetActiveWindow(hActiveWnd)
	
	hWrmHandle := rWrmDefInit:hWrmHandle
	
	AAdd(aOpenNames, {char1, hWrmHandle, .F. }) // last element is if a previewer is open
	nOpenIndex := INT(ALen(aOpenNames))   // Don't just increment. Index can be reset by SetFocus
    oOwner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_COMMANDERROR2),servername, "FILE.OPEN", "" ))
	
	IF  !MonitorIsOn
		// set an advise to be notified of events
		dwAdviseId := WrmSetAdvise( hWpHandle, String2Psz("SYSTEM"), String2Psz("REPORTEVENTS"), oHiddenRptWnd:Handle(), NTFY_MSG_BASE, 0 )
		MonitorIsOn := TRUE
	ENDIF
	RETURN SELF

#ifndef __VULCAN__ 	 // this seems to have no purpose, exemode is never used and raises warning VN3005
METHOD __DumpQ()   // for debugging
	LOCAL exemode AS STRING
	LOCAL i AS WORD
	LOCAL cmd AS DDEMC
	LOCAL len := ALen(aCommandQ)
	FOR i:=1 UPTO len
		cmd := aCommandQ[i]
		IF (cmd :isModal == TRUE)
			exemode := __CavoStr(__CAVOSTR_REPORTCLASS_SYNCHRONOUS)
		ELSE
			exemode := __CavoStr(__CAVOSTR_REPORTCLASS_ASYNCHRONOUS)
		ENDIF
	NEXT
	RETURN SELF
#endif 	

METHOD __ExecuteNow (oDDEMC) 
	LOCAL eb AS ErrorBox
	LOCAL char1
	LOCAL endOfMsg AS STRING
	
	endOfMsg := VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_THEREPORT), SELF:ActiveFile) //419@0001 MS 12/07/94
	
	// most command types are handled by the last lines of this method !
	
	// Special handling for File.Close commands
	IF (At("FILE.CLOSE", oDDEMC:cCmd) != 0)
        oOwner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_TOCLOSEFILE),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
		CloseMethodCalled := TRUE  // record that the VO program is initiator, rather than the user.
		// Stops DataUpdate from generating second FILE.CLOSE command to CARET if user closes View
		// If CARETRUN user closes report, this flag lets DataUpdate delete it from the OpenNames array
		
		// MALRO02 4/8/97 added, see below same date.
		WrmCloseReportWindow(hWpHandle, aOpenNames[nOpenIndex, 2])
		
		IF ((servername == "Report Editor") .AND. (nOpenIndex>0))
			ReportQueueIsBusy := TRUE  // wait for 'View.Close' and subsequent 'Report.Close'.
		ELSE // Don't wait for 'Report.Close' from CARETif no files open, and CARETRUN never sends this
			ReportQueueIsBusy := FALSE
			SELF:__OneReportLess()  // harmless if nOpenIndex is already zero
		ENDIF
		
		
		RETURN SELF
	ENDIF
	
	
	
	// Special handling for WINDOW.CLOSE.ALL command
	IF (At("WINDOW.CLOSE.ALL", oDDEMC:cCmd) != 0)
		endOfMsg := ""
		aOpenNames := {}
		nOpenIndex := 0
	ENDIF
	
	
	// Normal handling for everything except File.Close commands
	IF ((char1 :=  Upper(SubStr(oDDEMC:cCmd, 1, 1))) == "A")   // DDE [APP...] command
		endOfMsg := ""
	ENDIF
	
	IF ((nOpenIndex>0) .OR. (char1=="A") .OR. (char1=="W"))
		// even with no file open, CA-RET should accept [APP...] commands or WINDOW.CLOSE.ALL
		// ReportQueueIsBusy is now FALSE, set it TRUE if this cmd is Modal (alias asynchronous)
		ReportQueueIsBusy := oDDEMC:isModal
        oOwner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_COMMANDERROR2),servername, oDDEMC:cCmd, endOfMsg )) //419@0001 MS 12/07/94
		IF (SELF:sPreviewTitle == "")
			IF (aOpenNames[nOpenIndex, 3] = .T. )
				sItem := "CA - Report Viewer : [" + SELF:ActiveFile + "]"
				WrmExecute(hWpHandle, String2Psz(sItem), String2Psz(oDDEMC:cCmd))
			ELSE
				sItem := "CA - Report Writer : [" + SELF:ActiveFile + "]"
				WrmExecute(hWpHandle, String2Psz(sItem), String2Psz(oDDEMC:cCmd))
			ENDIF
		ELSE
			// MALRO02 4/10/97 added
			IF (aOpenNames[nOpenIndex, 3] == .F. )
				sItem := "CA - Report Writer : [" + SELF:ActiveFile + "]"
				WrmExecute(hWpHandle, String2Psz(sItem), String2Psz(oDDEMC:cCmd))
			ELSE
				WrmExecute(hWpHandle, String2Psz(SELF:sPreviewTitle), String2Psz(oDDEMC:cCmd))
			ENDIF
			
			// MALRO02 4/10/97 added
			char1 := Upper(oDDEMC:cCmd)
			IF (At("SET.TITLE", char1) != 0)
				char1 := SubStr(oDDEMC:cCmd, 12)
				char1 := SubStr(char1, 1, (Len(char1)-1))
				aOpenNames[nOpenIndex, 1] := char1
				SELF:sPreviewTitle := char1
			ENDIF
		ENDIF
	ELSE
        oOwner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_CLOSEDREPORT),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
		eb := ErrorBox{NIL, VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_COMMANDERROR1), oDDEMC:cCmd , servername)} //419@0001 MS 12/07/94
		eb := ErrorBox {NIL,__CavoStr(__CAVOSTR_REPORTCLASS_COMMANDERROR1)+oDDEMC :cCmd + __CavoStr(__CAVOSTR_REPORTCLASS_COMMANDERROR2)+servername}
		eb :Show()
	ENDIF
	RETURN SELF

METHOD __InitCAQRT() 
	LOCAL rCqmInit IS CACQMINIT
	LOCAL rRccInit IS CARCCINIT
	LOCAL rDbaInit IS CADBAINIT
	LOCAL rWpInit IS CAWPINIT
	LOCAL rWqmInit IS CAWQMINIT
	LOCAL rWbmInit IS CAWBMINIT
	LOCAL rWrmInit IS CAWRMINIT
	LOCAL cString AS STRING
	
	cString := "CA"
	
	// alloc only these substructures; any more will cause a GPF in caret
	rCqmInit:lpQryProps := MemAlloc(_SIZEOF(CACQMQRYPROPS))
	rRccInit:lpCtrlProps := MemAlloc(_SIZEOF(CARCCCTRLPROPS))
	
	// initialize the DB connection to zero
	hDbConn := 0
	
	
	// set the CQM init structure info
	rCqmInit:wCommandBase := QQ_MENU_BASE
	rCqmInit:wMessageBase := NTFY_MSG_BASE
	rCqmInit:lpstrAppTitle := String2Psz(cString)
	rCqmInit:lpstrDebugFlags := String2Psz("")
	rCqmInit:BD:bddwFlags := _OR(rCqmInit:BD:bddwFlags, 1)
	hCqmHandle := CqmInitialize( @rCqmInit )
	IF(hCqmHandle == 0)
		RETURN (0)
	ENDIF
	
	// set the RCC init structure info and then init RCC
	rRccInit:hWndApp := oHiddenRptWnd:Handle()
	rRccInit:hCqmHandle := hCqmHandle
	hRccHandle := RccInitialize( @rRccInit )
	IF(hRccHandle == 0)
        RETURN (FALSE)
	ENDIF
	
	// set the DBA init structure info and then init DBA
	rDbaInit:hDbaInitAppWnd := oHiddenRptWnd:Handle()
	rDbaInit:hCaCqmHndl := hCqmHandle
	rDbaInit:wCommandBase := DBA_MENU_BASE
	hDbaHandle := DbaInitialize( @rDbaInit )
	IF(hDbaHandle == 0)
        RETURN (FALSE)
	ENDIF
	
	rWpInit:hCqmHandle := hCqmHandle
	rWpInit:lpsQrySection := String2Psz(cString)
	rWpInit:lpsResSection := String2Psz(cString)
	rWpInit:lpsRetSection := String2Psz(cString)
	
	// create the wrapper program
	hWpHandle := WpInitialize( @rWpInit )
	IF(hWpHandle == 0)
        RETURN (FALSE)
	ENDIF
	
	// create the query wrapper
	IF( WqmInitialize( hWpHandle, @rWqmInit ) != 1 )
        RETURN (FALSE)
	ENDIF
	
	// create the result browser wrapper
	IF( WbmInitialize( hWpHandle, @rWbmInit ) != 1 )
        RETURN (FALSE)
	ENDIF
	
	// create the report wrapper
	rWrmInit:hCqmHandle := hCqmHandle
	rWrmInit:hRccHandle := hRccHandle
	// TRUE (1) if CAQRT should act like a runtime system.
	// Necessary so that WrmFileOpen() opens to a preview window,
	// and not a definer window.
	IF( WrmInitialize( hWpHandle, @rWrmInit ) != 1 )
        RETURN (FALSE)
	ENDIF
	
	bTermWrappers := TRUE
	
	// de-alloc substructures
	MemFree(rCqmInit:lpQryProps)
	MemFree(rRccInit:lpCtrlProps)
	
	RETURN TRUE
	
METHOD __OneReportLess() 
	LOCAL ln
	IF nOpenIndex > 0
		ln := ALen(aOpenNames) - 1  // new length after this deletion
		ADel(aOpenNames, DWORD(nOpenIndex))
		ASize(aOpenNames, ln)
		nOpenIndex := ln  // most recently opened becomes current
	ENDIF
	RETURN SELF

METHOD __QorExecute(oDDEMC) 
	IF  !MonitorIsOn
		// set an advise to be notified of events
		dwAdviseId := WrmSetAdvise( hWpHandle, String2Psz("SYSTEM"), String2Psz("REPORTEVENTS"), oHiddenRptWnd:Handle(), NTFY_MSG_BASE, 0 )
		MonitorIsOn := TRUE
	ENDIF
	
	IF  ReportQueueIsBusy
		AAdd(aCommandQ, oDDEMC)
	ELSE
		SELF :__ExecuteNow(oDDEMC)
	ENDIF
	RETURN SELF

METHOD __ReportParameters(aParams) 
	// send any Report Params before Print, Preview or Export commands
	LOCAL lParamNum AS LONGINT
	LOCAL iLen AS INT
	iLen := INT(Len(aParams))
	FOR lParamNum := 1 UPTO iLen 
		IF (aParams[lParamNum] <> NIL)
			IF (!IsString(aParams[lParamNum]))
				// handle error
				Eval(ErrorBlock(), RCError{aParams,1})
			ENDIF
			
			WrmSetParameter(aOpenNames[nOpenIndex, 2], lParamNum, String2Psz(aParams[lParamNum]))
		ENDIF
	NEXT
	RETURN SELF

METHOD __TerminateCAQRT() 
	// if there is a WP handle...
	IF( hWpHandle != 0 )
		// WrmTerminate() will send a WRM_NTFY_CLOSE
		// so don't process that one.
		bTermWrappers := FALSE
		// free the report wrapper
		TRY // TODO: Temp workaround for Access Violation thrown by WrmTerminate()
		WrmTerminate( hWpHandle )
		END TRY
		bTermWrappers := TRUE
		
		// free the result browser wrapper
		WbmTerminate( hWpHandle )
		
		// free the query wrapper
		WqmTerminate( hWpHandle )
		
		// free the wrapper program
		WpTerminate( hWpHandle )
		
		hWpHandle := 0
	ENDIF
	
	// if there is an RCC handle...
	IF( hRccHandle != 0 )
		RccTerminate( hRccHandle )
		hRccHandle := 0
	ENDIF
	
	// if there is a DBA handle...
	IF( hDbaHandle != 0 )
		DbaTerminate( hDbaHandle )
		hDbaHandle := 0
	ENDIF
	
	// if there is a CQM handle...
	IF( hCqmHandle != 0 )
		CqmTerminate( hCqmHandle )
		hCqmHandle := 0
		// CqmTerminate() disconnects from any databases that were
		// connected with CqmConnect() and are still connected.
		hDbConn := 0
	ENDIF
	
	// if the hidden window exists, destroy it
	IF(IsObject(oHiddenRptWnd))
		oHiddenRptWnd:Destroy()
		oHiddenRptWnd := NULL_OBJECT
	ENDIF
	
	RETURN(TRUE)
	
ACCESS ActiveFile() 
	IF nOpenIndex > 0
		RETURN aOpenNames[nOpenIndex, 1]
	ENDIF
	RETURN ""

DESTRUCTOR() 
	// Decrease the object counter.
	// If the last object is being destroyed, unload the .DLLs.
	
	IF (gdwObjectIndex > 1)
		gdwObjectIndex--
	ELSE
		gdwObjectIndex := 0
		gbCARETInitialized := .F. 
		SELF:__TerminateCAQRT() // unload the .DLLs
	ENDIF
	RETURN 

METHOD Close() 
	SELF :__QorExecute(DDEMC{TRUE,"FILE.CLOSE()"})
	RETURN SELF

METHOD CloseAll() 
	SELF :__QorExecute(DDEMC{FALSE,"WINDOW.CLOSE.ALL()"})
	RETURN SELF

METHOD ConnectToDB(cDbName, cUserName, cPassword) 
	LOCAL db_init IS CACQMDBINIT
	
	
	IF ((cDbName != NIL) .AND. IsString(cDbName))
		db_init:lpsDbName := String2Psz(cDbName) // "dBASE Files"
		// MALRO02 2/12/97 added the ability to pass the username and password
		IF ((cUserName != NIL) .AND. IsString(cUserName))
			db_init:lpsUserName := String2Psz(cUserName)
		ELSE
			db_init:lpsUserName := String2Psz("")
		ENDIF
		
		IF ((cPassword != NIL) .AND. IsString(cPassword))
			db_init:lpsPassword := String2Psz(cPassword)
		ELSE
			db_init:lpsPassword := String2Psz("")
		ENDIF
		// end MALRO02 2/12/97
		
		//db_init.DbProps.BD.bdAutoConnect := TRUE // TRUE to autoconnect to DB
		db_init:BD2:bddwFlags := _OR(db_init:BD2:bddwFlags, 1)
	ENDIF
	
	// connect to DB
	IF (CqmConnect( hCqmHandle, @db_init ) != 0)
		hDbConn := db_init:hDbHandle
		IF( hDbConn != 0 )
			CqmSetDefaultDatabase( hCqmHandle, hDbConn )
			RETURN TRUE
		ENDIF
	ENDIF
	
	
	RETURN FALSE
	
METHOD DataUpdate(oEvent) 
	// receives notifications of REPORTEVENTS
	LOCAL oDDEMC AS DDEMC
	LOCAL len
	LOCAL r //return value
	
	LOCAL newdata AS STRING
	LOCAL lpAdvNtfy AS CAWRMADVISENTFY
	LOCAL rWrmVwInit IS CAWRMCREATEVWINIT
	LOCAL lpPreviewNtfy AS CAWRMPREVIEWNTFY
	
	IF (oEvent:Message == (NTFY_MSG_BASE + CAWRM_NTFY_ADVISE))
		lpAdvNtfy := PTR(_CAST, oEvent:lParam)
		newdata := Psz2String(lpAdvNtfy:lpstrValue)
		
		
		IF newdata == "Report.Close"  // sent when the definer window is closed
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_CLOSEDREPORT),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
            SELF:__OneReportLess()
            _event := REPORTCLOSEEVENT
            CloseMethodCalled := FALSE

        ELSEIF newdata == "Report.Complete"
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_COMPLETEDREPORT),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
            _event := REPORTCOMPLETEEVENT

        ELSEIF newdata == "View.Close" // sent when a preview window is closed
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_CLOSEDREPORT),servername,"")) //419@0001 MS 12/07/94
            _event := REPORTVIEWCLOSEEVENT

            // Indicate that the preview window is closed.
            IF nOpenIndex > 0
               aOpenNames[nOpenIndex, 3] := .F.
            ENDIF

        ELSEIF newdata == "Report.Opened"
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_OPENEDREPORT),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
            _event := REPORTOPENEVENT

        ELSEIF  (At("File.Save",newdata) != 0) // this is a "File.Save;FILENAME" advice
            IF nOpenIndex > 0
                aOpenNames[nOpenIndex, 1] := SubStr(newdata,11,Len(newdata)-10)
            ENDIF
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_SAVEDREPORT),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
            _event := REPORTFILESAVEEVENT

        ELSEIF newdata == "Report.Complete.Error"   // introduced in CA-RET v2.0
            owner:StatusMessage(VO_Sprintf(__CavoStr(__CAVOSTR_REPORTCLASS_DETECTEDERRORS),servername, SELF:ActiveFile)) //419@0001 MS 12/07/94
			_event := REPORTCOMPLETEERROREVENT
		ENDIF
		
	ENDIF
	
	IF (oEvent:Message == WRM_NTFY_PREVIEW)
		// LPARAM is a structure describing the preview
		lpPreviewNtfy := PTR(_CAST, oEvent:lParam)
		
		// We set bdNtfyPreview when we created the
		// definer so here is the notification.
		// lParam has the definer WRMHANDLE.  Create a
		// viewer for it.
		
		// setup notification
		rWrmVwInit:hWndNotify := oHiddenRptWnd:Handle()
		rWrmVwInit:wMessageBase := WRM_NTFY_MSG_BASE
		
		// we need to be notified of close
		rWrmVwInit:BD:bddwFlags := _OR(rWrmVwInit:BD:bddwFlags, 8)
		
		rWrmVwInit:hCqmHandle := hCqmHandle     // handle returned from CqmInitialize
		rWrmVwInit:hWrmReport := lpPreviewNtfy:hWrmHandle
		
		// MALRO02 4/10/97 added
		// If we were passed in a title for the previewer,
		// set the title, and change the
		IF (SELF:sPreviewTitle != "")
			rWrmVwInit:lpsTitle := String2Psz(SELF:sPreviewTitle)
			aOpenNames[nOpenIndex, 1] := SELF:sPreviewTitle
		ENDIF
		
		IF (bMDIChild)
			rWrmVwInit:BD:bddwFlags := _OR(rWrmVwInit:BD:bddwFlags, 2) // ismdi
			rWrmVwInit:hWndMdiClient := SELF:oOwner:Handle(4) // MDIClient handle
		ENDIF
		// end MALRO02 4/10/97
		
		// create the viewer
		WrmCreateViewerWindow( hWpHandle, @rWrmVwInit )
		
		// indicate that a preview window is open
		aOpenNames[nOpenIndex, 3] := .T. 
		
		RETURN SELF
	ENDIF
	
	IF (oEvent:Message == WRM_NTFY_ABOUT)
		MessageBox(GetActiveWindow(), String2Psz(__CavoStr(__CAVOSTR_REPORTCLASS_ABOUTBODY)),;
			String2Psz(__CavoStr(__CAVOSTR_REPORTCLASS_ABOUTTITLE)), _OR(MB_OK, MB_ICONINFORMATION))
		RETURN SELF
	ENDIF
	
	r := oOwner:ReportNotification(SELF)    //notify the owner window
	IF ((!IsNil(r)) .AND. (UsualType(r)==LOGIC) .AND. (r== FALSE))
		RETURN SELF // and give up if owner says don't dequeue pending commands (unlikely)
	ENDIF
	
	// an asyncronous command has completed execution on CA-RET server
	ReportQueueIsBusy := FALSE
	
	// dequeue any pending EXECUTE commands, until send an asynch one
	// The array resizing keeps the queue available for adding more
	DO WHILE (((len := ALen(aCommandQ))>0) .AND. (ReportQueueIsBusy==FALSE))
		oDDEMC := aCommandQ[1]
		ADel (aCommandQ, 1)
		ASize (aCommandQ, len-1)
		SELF :__ExecuteNow(oDDEMC)
	ENDDO //even if a command can't execute, still move on to the next
	RETURN SELF
	

METHOD Edit() 
	
	// since the report was originally opened in a hidden
	// definer window, just show that window.
	SELF:Show(SHOWNORMAL)
	RETURN SELF

ACCESS EventType() 
	RETURN _event
	

ACCESS Filename() 
	RETURN RETname
	

METHOD Hide() 
	SELF :__QorExecute(DDEMC{FALSE,"APP.HIDE"})
	RETURN SELF

CONSTRUCTOR(oAppWindow, cServerName) 
	//  LOCAL ib AS InfoBox
	oOwner := oAppWindow
	
	IF ((cServerName == NIL) .OR. !IsString(cServerName))
		cServerName := "Report Editor"
	ENDIF
	
	IF(cServerName <> "Report Editor")
		cServerName := "Report Editor"  // The Default
	ENDIF
	
	servername := cServerName // keep for status messages and errors
	
	RETname := "" // empty until Open method called
	aOpenNames := {} // empty until [File.Open]
	nOpenIndex := 0
	MonitorIsOn := FALSE
	ReportQueueIsBusy := FALSE
	sItem := "" // DDE doesn't use the Item parameter in Execute commands
	sTopic := "SYSTEM" // set "DDE" Topic for Executes
	aCommandQ := {} // no "DDE" commands queued yet
	
	// create the hidden window that receives events from caqrt.
	oHiddenRptWnd := HiddenWndClass{oAppWindow, FALSE}
	oHiddenRptWnd:ParentWnd := SELF
	oHiddenRptWnd:Origin := Point{10, 10} // who cares, never see it
	oHiddenRptWnd:Size := Dimension{10, 10} // who cares, never see it
	oHiddenRptWnd:Caption := "" // who cares, never see it
	oHiddenRptWnd:Hide() // make sure it's hidden
	oHiddenRptWnd:OVERRIDE()
	
	IF (!gbCARETInitialized)
		// initialize the wrappers
		IF (SELF:__InitCAQRT())
			gbCARETInitialized := .T. 
		ENDIF
	ENDIF
	
	// add new object to global counter
	gdwObjectIndex++
	
	// create a connection to a DB
	//  SELF:ConnectToDB()
	
	// remove the object from the global counter on destruction of object
	
	
	RETURN 

METHOD NEW(cSQLStatement, iReportStyle) 
	IF (!IsString(cSQLStatement))
		// handle error
		Eval(ErrorBlock(), RCError{cSQLStatement,1})
	ENDIF
	
	SELF:__CreateDefinerWindow("", FILE_NEW, cSQLStatement, iReportStyle)
	RETURN SELF
	

METHOD Open(cReportName) 
	IF (!IsString(cReportName))
		// handle error
		Eval(ErrorBlock(), RCError{cReportName,1})
	ENDIF
	
	IF(At(".",cReportName) !=0) //has a filetype extension
		RETname := cReportName
	ELSE   //attach default ".RET" extension, if missing
		RETname := cReportName + ".RET"
	ENDIF
	
	// Value 1 parameter to File.Open tells CA-RET to run the query on opening
	//  SELF:__QorExecute (DDEMC{TRUE,"FILE.OPEN("+RETname+",0)"})
	//  SELF:FilePreview(RETname, aParams)
	SELF:__CreateDefinerWindow(cReportName, FILE_OPEN)
	RETURN SELF

ASSIGN Origin(oPoint) 
	// Everywhere else in CommonView (and VO) origin is BOTTOM left, but CA-RET uses TOP left
	// Can't simply invert by subtracting Y co-ord from num of pixels in screen, because CA-RET draws
	// ... DOWN and can't tell how many pixels high it will draw.
	LOCAL h,w,x,y
	
	IF (!IsInstanceOf(oPoint, #POINT))
		// handle error
		Eval(ErrorBlock(), RCError{oPoint,1})
	ENDIF
	
	w := GetSystemMetrics(SM_CXSCREEN)
	IF w > oPoint:X
		x := Trim(Str(oPoint:X))
	ELSE  // x co-ord is off-screen to the right
		x := Trim(Str(w-5))   // default it to right-hand edge
	ENDIF
	
	h := GetSystemMetrics(SM_CYSCREEN)
	IF h > oPoint:Y
		y := Trim(Str(oPoint:Y))
	ELSE  // y co-ord is above screen
		y := Trim(Str(h-5))   // default it to bottom edge
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"APP.MOVE("+x+","+y+")"})
	RETURN 

ACCESS owner() 
	RETURN oOwner

METHOD Preview(aParams, cPreviewTitle, iMC) 
	// send any Report Params before PREVIEW command
	IF (aParams <> NIL)
		SELF:__ReportParameters(aParams)
	ENDIF
	
	IF (IsString(cPreviewTitle) .AND. (cPreviewTitle != NIL))
		SELF:sPreviewTitle := cPreviewTitle
	ELSE
		SELF:sPreviewTitle := ""
	ENDIF
	
	IF (IsLogic(iMC) .AND. (iMC == .T. ))
		bMDIChild := .T. 
	ELSE
		bMDIChild := .F. 
	ENDIF
	
	SELF :__QorExecute(DDEMC{TRUE,"FILE.PREVIEW()"})
	RETURN SELF

METHOD Print(aParams, oRange, nCopies) 
	LOCAL i AS INT
	
	DEFAULT(@nCopies, 1)
	
	FOR i:= 1 TO nCopies
		// send any Report Params across DDE before PRINT command
		IF (aParams <> NIL)
			SELF:__ReportParameters(aParams)
		ENDIF
		
		IF (oRange == NIL) // then print all
			SELF:__QorExecute(DDEMC{TRUE, "FILE.PRINT(0,0)"})
		ELSE
			IF (!IsInstanceOf(oRange, #RANGE))
				// handle error
				Eval(ErrorBlock(), RCError{oRange,2})
			ENDIF
			SELF:__QorExecute(DDEMC{TRUE, "FILE.PRINT("+Str(oRange:Min)+ "," +Str(oRange:Max)+ ")"})
		ENDIF
		GetAppObject():Exec(EXECWHILEEVENT)
	NEXT
	RETURN SELF
	

/*	
TEXTBLOCK Execute(sDDECmd, bIsModal) CLASS ReportQueue     //MALRO02 4/10/97 added
	
	IF (!IsString(sDDECmd))
		// handle error
		Eval(ErrorBlock(), RCError{sDDECmd,1})
	ENDIF
	
	IF (bIsModal == NIL)
		bIsModal := TRUE
	ENDIF
	
	SELF :__QorExecute(DDEMC{bIsModal, sDDECmd})
	

ENDTEXT
*/
METHOD ReportExit()                            //451@AJP001
	SELF :__QorExecute(DDEMC{TRUE,"FILE.EXIT()"})
	RETURN SELF

ACCESS ReportServer() 
	RETURN servername

METHOD SaveToFile(cOutFile,cFormat,aParams) 
	LOCAL ib AS InfoBox
	
	IF (!IsString(cOutFile))
		// handle error
		Eval(ErrorBlock(), RCError{cOutFile,1})
	ENDIF
	
	// format can be RTF or TEXT
	IF ((cFormat == NIL) .OR. !IsString(cFormat))
		cFormat := "RTF" //The default
	ENDIF
	
	IF((cFormat <> "RTF") .AND. (cFormat <> "TEXT"))
		ib:= InfoBox{,__CavoStr(__CAVOSTR_REPORTCLASS_SAVETOFILEINFO1),;
			__CavoStr(__CAVOSTR_REPORTCLASS_SAVETOFILEINFO2)}
		ib:Show()
		cFormat := "RTF"  //The Default
	ENDIF
	
	IF (aParams <> NIL) // send any Report Params across DDE before EXPORT command
		SELF:__ReportParameters(aParams)
	ENDIF
	
	SELF :__QorExecute(DDEMC{TRUE,"EXPORT("+cOutFile+","+cFormat+")"})
	RETURN SELF
	
METHOD SetPapersize(iSize) 
	
	IF (iSize == NIL)
		iSize := 1
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"Set.Printer.Papersize(" + Str(iSize) + ")"})
	RETURN SELF

METHOD SetPrinterOrientation(bOrientation) 
	
	IF (bOrientation == NIL)
		bOrientation := 0
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"Set.Printer.Orientation(" + Str(bOrientation) + ")"})
	RETURN SELF

METHOD SetReportQuery(sQueryName) 
	
	IF (!IsString(sQueryName))
		// handle error
		Eval(ErrorBlock(), RCError{sQueryName,1})
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"Database.Set.Report.Query(" + sQueryName + ")"})
	RETURN SELF

METHOD SetSQLFile(sQuery) 
	
	IF (!IsString(sQuery))
		// handle error
		Eval(ErrorBlock(), RCError{sQuery,1})
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"Database.Set.SQL(2, " + sQuery + ")"})
	RETURN SELF

METHOD SetSQLText(sQuery) 
	
	IF (!IsString(sQuery))
		// handle error
		Eval(ErrorBlock(), RCError{sQuery,1})
	ENDIF
	
	SELF :__QorExecute(DDEMC{FALSE,"Database.Set.SQL(5, " + sQuery + ")"})
	RETURN SELF

METHOD Show(nShowState) 
	IF ( nShowState == SHOWNORMAL )
		SELF :__QorExecute(DDEMC{FALSE,"APP.RESTORE()"})
	ELSEIF nShowState == SHOWICONIZED
		SELF :__QorExecute(DDEMC{FALSE,"APP.MINIMIZE()"})
	ELSEIF nShowState == SHOWZOOMED
		SELF :__QorExecute(DDEMC{FALSE,"APP.MAXIMIZE()"})
	ENDIF
	RETURN SELF

ASSIGN Size(oDimension) 
	LOCAL w, h
	
	IF (!IsInstanceOf(oDimension, #DIMENSION))
		// handle error
		Eval(ErrorBlock(), RCError{oDimension,1})
	ENDIF
	
	w := Str(oDimension :Width)
	h := Str(oDimension :Height)
	
	SELF :__QorExecute(DDEMC{FALSE,"APP.SIZE("+w+","+h+")"})
	RETURN 
	
END CLASS

STATIC GLOBAL gbCARETInitialized AS LOGIC
	
STATIC GLOBAL gdwObjectIndex := 0 AS DWORD
	
STATIC GLOBAL hWpHandle := 0 AS DWORD   // handle returned from WpInitialize
	
STATIC GLOBAL hRccHandle := 0 AS DWORD  // handle returned from RccInitialize
	
STATIC GLOBAL hDbaHandle := 0 AS DWORD  // handle returned from DbaInitialize
	
STATIC GLOBAL hCqmHandle := 0 AS DWORD  // handle returned from CqmInitialize
	
FUNCTION RetCqmHandle() AS DWORD
	RETURN hCqmHandle
	
FUNCTION RetWpHandle() AS DWORD
	RETURN hWpHandle
	
FUNCTION IsCaretLoaded() AS LOGIC
	RETURN gbCARETInitialized
	
_DLL FUNC WrmCloseReportWindow(x AS DWORD, y AS DWORD) AS VOID PASCAL:CAQR3WRM.WrmCloseReportWindow
	
