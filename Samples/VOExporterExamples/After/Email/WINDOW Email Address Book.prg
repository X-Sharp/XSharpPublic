#region DEFINES
STATIC DEFINE EMAILADDRESSBOOK_TCAPTION := 100
STATIC DEFINE EMAILADDRESSBOOK_TCONTACT := 101
STATIC DEFINE EMAILADDRESSBOOK_TCOMPANY := 102
STATIC DEFINE EMAILADDRESSBOOK_TSTREET := 103
STATIC DEFINE EMAILADDRESSBOOK_TCITY := 104
STATIC DEFINE EMAILADDRESSBOOK_TSTATE := 105
STATIC DEFINE EMAILADDRESSBOOK_TPHONE := 106
STATIC DEFINE EMAILADDRESSBOOK_TFAX := 107
STATIC DEFINE EMAILADDRESSBOOK_TMOBILE := 108
STATIC DEFINE EMAILADDRESSBOOK_TEMAIL := 109
STATIC DEFINE EMAILADDRESSBOOK_CONTACT := 110
STATIC DEFINE EMAILADDRESSBOOK_COMPANY := 111
STATIC DEFINE EMAILADDRESSBOOK_STREET := 112
STATIC DEFINE EMAILADDRESSBOOK_CITY := 113
STATIC DEFINE EMAILADDRESSBOOK_STATE := 114
STATIC DEFINE EMAILADDRESSBOOK_PHONE := 115
STATIC DEFINE EMAILADDRESSBOOK_MOBILE := 116
STATIC DEFINE EMAILADDRESSBOOK_FAX := 117
STATIC DEFINE EMAILADDRESSBOOK_EMAIL := 118
STATIC DEFINE EMAILADDRESSBOOK_PBNEW := 119
STATIC DEFINE EMAILADDRESSBOOK_PBEXIT := 120
STATIC DEFINE EMAILADDRESSBOOK_PBSAVE := 121
#endregion

CLASS EmailAddressBook INHERIT DATADIALOG
	PROTECT oDCtCaption AS FIXEDTEXT
	PROTECT oDCtContact AS FIXEDTEXT
	PROTECT oDCtCompany AS FIXEDTEXT
	PROTECT oDCtStreet AS FIXEDTEXT
	PROTECT oDCtCITY AS FIXEDTEXT
	PROTECT oDCtSTATE AS FIXEDTEXT
	PROTECT oDCtPHONE AS FIXEDTEXT
	PROTECT oDCtFax AS FIXEDTEXT
	PROTECT oDCtMobile AS FIXEDTEXT
	PROTECT oDCtEMAIL AS FIXEDTEXT
	PROTECT oDCCONTACT AS SINGLELINEEDIT
	PROTECT oDCCOMPANY AS SINGLELINEEDIT
	PROTECT oDCSTREET AS SINGLELINEEDIT
	PROTECT oDCCITY AS SINGLELINEEDIT
	PROTECT oDCSTATE AS SINGLELINEEDIT
	PROTECT oDCPHONE AS SINGLELINEEDIT
	PROTECT oDCMobile AS SINGLELINEEDIT
	PROTECT oDCFax AS SINGLELINEEDIT
	PROTECT oDCEMAIL AS SINGLELINEEDIT
	PROTECT oCCPBNew AS PUSHBUTTON
	PROTECT oCCPBExit AS PUSHBUTTON
	PROTECT oCCPBSave AS PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  
  //USER CODE STARTS HERE (do NOT remove this line)

  PROTECT lAppend AS LOGIC
  PROTECT InitCompleted AS LOGIC
  PROTECT _oDBServer AS DBServer
  PROTECT _aFields AS ARRAY


METHOD PBNew( ) 

	SELF:oCCPBNew:Disable()
	SELF:oCCPBSave:Disable()
	SELF:FillControls(TRUE)		 	// append blank entry
	
	RETURN SELF

METHOD PBSave( ) 
   LOCAL dwI AS DWORD
   LOCAL symField AS SYMBOL

	IF SELF:ControlVerify()
		IF SELF:lAppend
			_oDBServer:Append()
		ENDIF
   
      FOR dwI := 1 UPTO 9
         symField := _aFields[dwI]
         _oDBServer:FIELDPUT(symField, Trim(SELF:FIELDGET(symField)))
      NEXT dwI
   
		_oDBServer:Commit()
		SELF:oCCPBNew:Enable()
		SELF:oCCPBSave:Disable()
	ENDIF	

	RETURN SELF	


METHOD PBExit( ) 

	SELF:EndWindow()
	
	RETURN SELF

METHOD ControlVerify() 

	LOCAL aCtrls AS ARRAY
	LOCAL i AS DWORD
	LOCAL lRet AS LOGIC

	lRet := TRUE
	aCtrls := SELF:acontrols

	FOR i := 1 UPTO ALen(aCtrls)
		IF aCtrls[i]:NameSym == #COMPANY .and. ( IsNil(aCtrls[i]:Value) .or. Empty(aCtrls[i]:Value))
			TextBox{SELF,"Error", "This control cannot be empty"}:Show()
			aCtrls:SetFocus()
			lRet := FALSE
			EXIT
		ENDIF
		IF aCtrls[i]:NameSym == #EMAIL
			IF IsNil(aCtrls[i]:Value) .or. Empty(aCtrls[i]:Value)
				TextBox{SELF,"Error", "This control cannot be empty"}:Show()
				aCtrls:SetFocus()			
				lRet := FALSE
				EXIT
			ELSEIF At( "@", aCtrls[i]:Value )==0 .or. At(".",aCtrls[i]:Value) == 0
				TextBox{SELF,"Error", "The Email address is in an invalid format"}:Show()
				aCtrls:SetFocus()			
				lRet := FALSE
				EXIT			
			ENDIF
		ENDIF
	NEXT

	RETURN lRet			


CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oWindow,iCtlID,oServer,uExtra)

	SUPER(oWindow , ResourceID{"EmailAddressBook" , _GetInst()},iCtlID)

	SELF:oDCtCaption := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TCAPTION  , _GetInst() } }
	oFont := Font{  , 24 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtCaption:Font( oFont )
	SELF:oDCtCaption:HyperLabel := HyperLabel{#tCaption , "Address Book Entry" , NULL_STRING , NULL_STRING}

	SELF:oDCtContact := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TCONTACT  , _GetInst() } }
	SELF:oDCtContact:TextColor := Color{ COLORBLACK }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtContact:Font( oFont )
	SELF:oDCtContact:HyperLabel := HyperLabel{#tContact , "Contact" , NULL_STRING , NULL_STRING}

	SELF:oDCtCompany := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TCOMPANY  , _GetInst() } }
	SELF:oDCtCompany:TextColor := Color{ COLORBLACK }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtCompany:Font( oFont )
	SELF:oDCtCompany:HyperLabel := HyperLabel{#tCompany , "Company" , NULL_STRING , NULL_STRING}

	SELF:oDCtStreet := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TSTREET  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtStreet:Font( oFont )
	SELF:oDCtStreet:HyperLabel := HyperLabel{#tStreet , "Street Address" , NULL_STRING , NULL_STRING}

	SELF:oDCtCITY := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TCITY  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtCITY:Font( oFont )
	SELF:oDCtCITY:HyperLabel := HyperLabel{#tCITY , "City" , NULL_STRING , NULL_STRING}

	SELF:oDCtSTATE := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TSTATE  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtSTATE:Font( oFont )
	SELF:oDCtSTATE:HyperLabel := HyperLabel{#tSTATE , "State" , NULL_STRING , NULL_STRING}

	SELF:oDCtPHONE := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TPHONE  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtPHONE:Font( oFont )
	SELF:oDCtPHONE:HyperLabel := HyperLabel{#tPHONE , "Phone" , NULL_STRING , NULL_STRING}

	SELF:oDCtFax := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TFAX  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtFax:Font( oFont )
	SELF:oDCtFax:HyperLabel := HyperLabel{#tFax , "Fax" , NULL_STRING , NULL_STRING}

	SELF:oDCtMobile := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TMOBILE  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtMobile:Font( oFont )
	SELF:oDCtMobile:HyperLabel := HyperLabel{#tMobile , "Mobile" , NULL_STRING , NULL_STRING}

	SELF:oDCtEMAIL := FIXEDTEXT{SELF , ResourceID{ EMAILADDRESSBOOK_TEMAIL  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCtEMAIL:Font( oFont )
	SELF:oDCtEMAIL:HyperLabel := HyperLabel{#tEMAIL , "Email Address" , NULL_STRING , NULL_STRING}

	SELF:oDCCONTACT := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_CONTACT  , _GetInst() } }
	SELF:oDCCONTACT:UseHLforToolTip := True
	SELF:oDCCONTACT:TooltipText := "Enter the full name of a friend"
	SELF:oDCCONTACT:HyperLabel := HyperLabel{#CONTACT , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCOMPANY := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_COMPANY  , _GetInst() } }
	SELF:oDCCOMPANY:UseHLforToolTip := True
	SELF:oDCCOMPANY:TooltipText := "Enter the full name of a friend"
	SELF:oDCCOMPANY:HyperLabel := HyperLabel{#COMPANY , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSTREET := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_STREET  , _GetInst() } }
	SELF:oDCSTREET:UseHLforToolTip := True
	SELF:oDCSTREET:TooltipText := "Enter the street address of the person"
	SELF:oDCSTREET:HyperLabel := HyperLabel{#STREET , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCITY := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_CITY  , _GetInst() } }
	SELF:oDCCITY:TooltipText := "Enter the city of the person"
	SELF:oDCCITY:HyperLabel := HyperLabel{#CITY , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSTATE := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_STATE  , _GetInst() } }
	SELF:oDCSTATE:UseHLforToolTip := True
	SELF:oDCSTATE:TooltipText := "Enter the state "
	SELF:oDCSTATE:HyperLabel := HyperLabel{#STATE , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCPHONE := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_PHONE  , _GetInst() } }
	SELF:oDCPHONE:UseHLforToolTip := True
	SELF:oDCPHONE:TooltipText := "Enter the phone number of the person"
	SELF:oDCPHONE:HyperLabel := HyperLabel{#PHONE , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCMobile := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_MOBILE  , _GetInst() } }
	SELF:oDCMobile:UseHLforToolTip := True
	SELF:oDCMobile:TooltipText := "Enter the phone number of the person"
	SELF:oDCMobile:HyperLabel := HyperLabel{#Mobile , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFax := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_FAX  , _GetInst() } }
	SELF:oDCFax:UseHLforToolTip := True
	SELF:oDCFax:TooltipText := "Enter the phone number of the person"
	SELF:oDCFax:HyperLabel := HyperLabel{#Fax , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCEMAIL := SINGLELINEEDIT{SELF , ResourceID{ EMAILADDRESSBOOK_EMAIL  , _GetInst() } }
	SELF:oDCEMAIL:UseHLforToolTip := True
	SELF:oDCEMAIL:TooltipText := "Enter the Email address of the person"
	SELF:oDCEMAIL:HyperLabel := HyperLabel{#EMAIL , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCPBNew := PUSHBUTTON{SELF , ResourceID{ EMAILADDRESSBOOK_PBNEW  , _GetInst() } }
	SELF:oCCPBNew:TooltipText := "To add a new friend"
	SELF:oCCPBNew:HyperLabel := HyperLabel{#PBNew , "New" , NULL_STRING , NULL_STRING}

	SELF:oCCPBExit := PUSHBUTTON{SELF , ResourceID{ EMAILADDRESSBOOK_PBEXIT  , _GetInst() } }
	SELF:oCCPBExit:UseHLforToolTip := True
	SELF:oCCPBExit:TooltipText := "Exit"
	SELF:oCCPBExit:HyperLabel := HyperLabel{#PBExit , "Exit" , NULL_STRING , NULL_STRING}

	SELF:oCCPBSave := PUSHBUTTON{SELF , ResourceID{ EMAILADDRESSBOOK_PBSAVE  , _GetInst() } }
	SELF:oCCPBSave:TooltipText := "To save entry"
	SELF:oCCPBSave:HyperLabel := HyperLabel{#PBSave , "Save" , NULL_STRING , NULL_STRING}

	SELF:Caption := "VO 2.7 Email Client Address Book"
	SELF:AllowServerClose := False
	SELF:HyperLabel := HyperLabel{#EmailAddressBook , "VO 2.7 Email Client Address Book" , NULL_STRING , NULL_STRING}
	IF !IsNil(oServer)
		SELF:Use(oServer)
	ENDIF


	SELF:PostInit(oWindow,iCtlID,oServer,uExtra)

RETURN


METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 
	
	Default(@uExtra, FALSE)		// establishes a NEW record if TRUE 
	
	_oDBServer := SELF:Server
	
	_aFields := {#Contact, #Company, #Street, #City, #State, #PHONE, #Fax, #Mobile, #EMAIL}
	
	SELF:USE(Null_Object)
	
	SELF:oCCPBNew:Disable()
	SELF:oCCPBSave:Disable()
	SELF:FillControls(uExtra)
	
	SELF:InitCompleted := TRUE	// finished setting up controls - allow EditChange to work

	RETURN NIL


METHOD FillControls(lNew AS LOGIC) AS VOID PASCAL 
   LOCAL dwI AS DWORD
   LOCAL symField AS SYMBOL
  
   SELF:lAppend := lNew
   
   FOR dwI := 1 UPTO 9
      symField := _aFields[dwI]
      SELF:FIELDPUT(symField, IF(lNew, "", _oDBServer:FIELDGET(symField)))
   NEXT dwI
   
	RETURN

METHOD EditChange(oControlEvent) 

	LOCAL oControl AS Control

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
	SUPER:EditChange(oControlEvent)

	IF SELF:InitCompleted
		IF !Empty(SELF:oDCCompany:TextValue) .AND. !Empty( SELF:oDCEMAIL:TextValue)
			SELF:oCCPBSave:Enable()
		ELSE
			SELF:oCCPBSave:Disable()
		ENDIF
	ENDIF			

	RETURN NIL



END CLASS
