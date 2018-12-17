GLOBAL aMailInfo AS ARRAY



GLOBAL oEmailServer AS EmailStore	

GLOBAL ogINetDial AS CINetDial

GLOBAL ogOpenWindows AS Container

GLOBAL ogStorage AS MyStorage


[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XApp
	TRY
		oXApp := XApp{}
		oXApp:Start()
	CATCH e AS Exception
		LOCAL cMessage AS STRING
		cMessage := e:Message
		DO WHILE e:InnerException != NULL_OBJECT
			e := e:InnerException
			cMessage += CRLF+e:Message
		ENDDO
		ErrorBox{NIL, cMessage}:Show()
	END TRY
RETURN 0

CLASS XApp INHERIT App
METHOD InitialiseApplication() 

	// Set system wide defaults
	SetDeleted (TRUE)               // hide deleted records
	SetAMExt(" AM")
	SetPMExt(" PM")
	SetAmPm (FALSE)        	    	// 24 hr clock
	SetCollation(#CLIPPER)
	SetExclusive(FALSE)            // start off in SHARED mode for databases
	SetDateCountry(BRITISH)        // Date format is dd/mm/yy
	SetDateFormat("DD/MM/YYYY")
	SetEpoch(1990)                 // Y2K compliance measure
	SetDecimal(2)
	RDDSETDEFAULT("DBFCDX")			// Set default index parameters accordingly
	RDDINFO(_SET_AUTOOPEN, TRUE)
	
	RETURN NIL

METHOD Notify(kNotifyCode, uValue) 	
   LOCAL cCaption, cText AS STRING
   LOCAL nType AS DWORD 
   LOCAL oDial AS CINetDial

   DO CASE
      CASE kNotifyCode = NOTIFY_CINetDial_ERROR
           oDial := uValue
           cCaption := "Connection error"
           cText    := oDial:ErrorDescription 
           IF Empty(cText)
              cText := "No internet connection available!"
           ENDIF   
           nType    := BOXICONHAND
      
      CASE kNotifyCode = NOTIFY_CINetDial_QueryHangUp 
           cCaption := "Hang up"
           cText    := "Shall the modem internet connection be hanged up now?"
           nType    := BUTTONYESNO+BOXICONQUESTIONMARK
           
      CASE kNotifyCode = NOTIFY_CINetDial_QueryHangUpAll
           cCaption := "Hang up all"
           cText    := "Shall all modem internet connections be hanged up now?"
           nType    := BUTTONYESNO+BOXICONQUESTIONMARK
                
      OTHERWISE
           RETURN TRUE
   ENDCASE

   IF TextBox{NIL, cCaption, cText, nType}:Show() == BOXREPLYYES	
		RETURN TRUE
	ENDIF
   RETURN FALSE                                         

METHOD Start() 

	LOCAL oMainWindow AS  EmailWindowMain
	LOCAL oConfigWin AS EmailConfig 
	
	SELF:InitialiseApplication()
	SetUserInfo()	// call up registry settings
	
	ogOpenWindows := Container{}
	ogStorage     := MyStorage{} 
	ogINetDial    := CINetDial{,SELF}
	
	// set up global server objects
	oEmailServer := EmailStore{}
	oEmailServer:SetOrder("BOXDATE")

	oMainWindow := EmailWindowMain{SELF}
	oMainWindow:QuitOnClose := TRUE
	oMainWindow:Show(SHOWCENTERED)

	// check for first-time use and/or query user for account setup features
	IF Empty(aMailInfo[DEF_PASSWORD])
		oConfigWin := EmailConfig{oMainWindow}
		oConfigWin:Show()
	ENDIF	

	SELF:exec() 
	
	oEmailServer:Close()
	
	ogINetDial:HangUp(TRUE)

	RETURN SELF

END CLASS
