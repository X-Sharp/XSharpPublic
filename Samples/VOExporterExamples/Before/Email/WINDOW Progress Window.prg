#region DEFINES
STATIC DEFINE PROGRESSWINDOW_VO_ANIMATION := 105 
STATIC DEFINE PROGRESSWINDOW_VO_BARFEEDBACK := 103 
STATIC DEFINE PROGRESSWINDOW_VO_PSHCANCEL := 104 
STATIC DEFINE PROGRESSWINDOW_VO_TXTFEEDBACK := 101 
STATIC DEFINE PROGRESSWINDOW_VO_TXTMESSAGE := 100 
STATIC DEFINE PROGRESSWINDOW_VO_TXTPERCENT := 102 
#endregion

CLASS ProgressWindow INHERIT ProgressWindow_vo
	PROTECT _nCount       AS DWORD
	PROTECT _nPos         AS DWORD
	PROTECT _nStart       AS DWORD
	PROTECT _nCurrent     AS DWORD
	PROTECT _nLastSeconds AS DWORD
	PROTECT _lCancel      AS LOGIC
	PROTECT _lPercent     AS LOGIC
	PROTECT _lCanCancel   AS LOGIC
	PROTECT _lTime        AS LOGIC
	PROTECT _lAppExec     AS LOGIC
	
	~"ONLYEARLY+"
	~"ONLYEARLY-"
	
	

ASSIGN AppExec(lValue) 
	RETURN _lAppExec := lValue

ASSIGN AVIResource(cValue) 
   oDCAnimation:OpenResource(cValue)
   RETURN cValue

ACCESS Cancel 
	RETURN _lCancel


ASSIGN Cancel (lCancel) 
	
	_lCanCancel := lCancel
	
	IF .NOT. _lCanCancel
		oCCpshCancel:disable()
		oCCpshCancel:Hide()
	ENDIF
   RETURN SELF

ASSIGN Count (nCount) 
   IF IsNumeric(nCount)
   	oDCbarFeedback:position := _nCurrent:= _nPos:=0
   	_nStart := DWORD(Seconds())
   	_nCount := nCount
   ENDIF
   RETURN nCount

METHOD Dispatch (oEvent) 
   LOCAL oEvt AS @@Event

   oEvt := oEvent
	IF _lCanCancel .and. oEvt:message == WM_Command .AND. oEvt:wParam  == IDCancel
		// Esc-Taste
		_lCancel := TRUE
	ENDIF

	RETURN SUPER:dispatch (oEvt)


Method EndDialog () 
	IF self:result != 99
		super:endDialog (99)
	ENDIF

   RETURN SELF

METHOD ExecModal() 
   ApplicationExec (ExecWhileEvent)
   RETURN SELF

CONSTRUCTOR (oOwner, cCaption, cMessage, nCount) 
	
	SUPER:INIT (oOwner)

	SELF:Percent := TRUE
	_lCancel     := FALSE
	_lCanCancel  := TRUE
	_lAppExec    := FALSE

	// Wertebereich des Laufbalkens
 	oDCbarFeedback:Range := Range { 1, 100 }

 	// Prozentangabe Rechtsbündig ausgeben
   oDCtxtPercent:SetStandardStyle(FT_RIGHTALIGN)

   SELF:Time := TRUE

   IF IsString(cCaption)
   	SELF:Caption := cCaption
   ENDIF
	
	SELF:Message(cMessage)
	SELF:Count := nCount
	
   RETURN SELF

METHOD Message (cText) 
	IF IsString(cText)
		oDCtxtMessage:Caption := cText
      IF _lAppExec
   		ApplicationExec (ExecWhileEvent)
   	ENDIF	
   ENDIF
   RETURN SELF

ASSIGN Percent (lPercent) 
	oDCtxtPercent:caption  := IF(lPercent,NTrim(_nCurrent) + " %","")
	RETURN _lPercent := lPercent

ACCESS Position 
   RETURN _nPos

ASSIGN Position(nValue) 

   IF nValue >= 0 .and. nValue <=_nCount
      _nPos := nValue
      SELF:StepIt(0)
   ENDIF
	RETURN nValue


Method pshCancel () 
	_lCancel := TRUE

   RETURN SELF

METHOD StepIt (dwSteps := 1 AS DWORD) AS LOGIC PASCAL 
	LOCAL dwSeconds AS DWORD
	LOCAL dwNew     AS DWORD
	
	IF _lAppExec
      ApplicationExec (ExecWhileEvent)
   ENDIF

	IF _nPos<_nCount
		
		_nPos += dwSteps
		dwNew := (_nPos * 100U) / _nCount
		IF dwNew > 100
         dwNew := 100
      ENDIF
		
		IF dwNew != _nCurrent .or. _nPos = 1
			_nCurrent := dwNew
			oDCbarFeedback:Position := _nCurrent
			
		   IF _lAppExec
      	   ApplicationExec (ExecWhileEvent)
      	ENDIF

			IF _lPercent
				oDCtxtPercent:caption  := NTrim(_nCurrent) + " %"
			ENDIF
		
		   IF _lTime
		   	dwSeconds := DWORD(Seconds())-_nStart
		   	IF _nCurrent = 0
		   		dwSeconds := 100 * dwSeconds
		   	ELSE	
               dwSeconds := 100*dwSeconds/_nCurrent-dwSeconds
            ENDIF
            IF dwSeconds != _nLastSeconds
   		   	oDCtxtFeedback:caption:="Time: " +StrZero(dwSeconds/3600,2)+":";
   		   	                                 +StrZero((dwSeconds%3600)/60,2)+":";
   		                                       +StrZero(dwSeconds%60,2)
   		      _nLastSeconds := dwSeconds
            ENDIF
			ENDIF
		ENDIF
   ENDIF	
	RETURN .NOT. _lCancel	

ASSIGN Time (lTime) 
	_nStart:=DWORD(Seconds())
	oDCtxtFeedback:caption:="Time: 00:00:00"
	RETURN _lTime := lTime


METHOD UpdateText (cCaption) 
	
	IF _lAppExec
		ApplicationExec (ExecWhileEvent)
	ENDIF	
	
	oDCtxtFeedback:caption := cCaption
		
	RETURN .NOT. _lCancel


END CLASS
CLASS ProgressWindow_Vo INHERIT DIALOGWINDOW 

	PROTECT oDCtxtMessage AS FIXEDTEXT
	PROTECT oDCtxtFeedback AS FIXEDTEXT
	PROTECT oDCtxtPercent AS FIXEDTEXT
	PROTECT oDCbarFeedback AS PROGRESSBAR
	PROTECT oCCpshCancel AS PUSHBUTTON
	PROTECT oDCAnimation AS ANIMATIONCONTROL

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oParent,uExtra)  

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"ProgressWindow_Vo",_GetInst()},TRUE)

oDCtxtMessage := FixedText{SELF,ResourceID{PROGRESSWINDOW_VO_TXTMESSAGE,_GetInst()}}
oDCtxtMessage:HyperLabel := HyperLabel{#txtMessage,NULL_STRING,NULL_STRING,NULL_STRING}

oDCtxtFeedback := FixedText{SELF,ResourceID{PROGRESSWINDOW_VO_TXTFEEDBACK,_GetInst()}}
oDCtxtFeedback:HyperLabel := HyperLabel{#txtFeedback,NULL_STRING,NULL_STRING,NULL_STRING}

oDCtxtPercent := FixedText{SELF,ResourceID{PROGRESSWINDOW_VO_TXTPERCENT,_GetInst()}}
oDCtxtPercent:HyperLabel := HyperLabel{#txtPercent,NULL_STRING,NULL_STRING,NULL_STRING}

oDCbarFeedback := ProgressBar{SELF,ResourceID{PROGRESSWINDOW_VO_BARFEEDBACK,_GetInst()}}
oDCbarFeedback:TooltipText := "Laufbalken 1-100%"
oDCbarFeedback:HyperLabel := HyperLabel{#barFeedback,NULL_STRING,NULL_STRING,NULL_STRING}

oCCpshCancel := PushButton{SELF,ResourceID{PROGRESSWINDOW_VO_PSHCANCEL,_GetInst()}}
oCCpshCancel:HyperLabel := HyperLabel{#pshCancel,_chr(38)+"Cancel",NULL_STRING,NULL_STRING}
oCCpshCancel:UseHLforToolTip := True

oDCAnimation := AnimationControl{SELF,ResourceID{PROGRESSWINDOW_VO_ANIMATION,_GetInst()}}
oDCAnimation:HyperLabel := HyperLabel{#Animation,NULL_STRING,NULL_STRING,NULL_STRING}

SELF:Caption := ""
SELF:HyperLabel := HyperLabel{#ProgressWindow_Vo,NULL_STRING,NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


END CLASS
