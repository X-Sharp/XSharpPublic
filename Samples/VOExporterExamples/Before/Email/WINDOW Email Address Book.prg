#region DEFINES
STATIC DEFINE EMAILADDRESSBOOK_CITY := 113 
STATIC DEFINE EMAILADDRESSBOOK_COMPANY := 111 
STATIC DEFINE EMAILADDRESSBOOK_CONTACT := 110 
STATIC DEFINE EMAILADDRESSBOOK_EMAIL := 118 
STATIC DEFINE EMAILADDRESSBOOK_FAX := 117 
STATIC DEFINE EMAILADDRESSBOOK_MOBILE := 116 
STATIC DEFINE EMAILADDRESSBOOK_PBEXIT := 120 
STATIC DEFINE EMAILADDRESSBOOK_PBNEW := 119 
STATIC DEFINE EMAILADDRESSBOOK_PBSAVE := 121 
STATIC DEFINE EMAILADDRESSBOOK_PHONE := 115 
STATIC DEFINE EMAILADDRESSBOOK_STATE := 114 
STATIC DEFINE EMAILADDRESSBOOK_STREET := 112 
STATIC DEFINE EMAILADDRESSBOOK_TCAPTION := 100 
STATIC DEFINE EMAILADDRESSBOOK_TCITY := 104 
STATIC DEFINE EMAILADDRESSBOOK_TCOMPANY := 102 
STATIC DEFINE EMAILADDRESSBOOK_TCONTACT := 101 
STATIC DEFINE EMAILADDRESSBOOK_TEMAIL := 109 
STATIC DEFINE EMAILADDRESSBOOK_TFAX := 107 
STATIC DEFINE EMAILADDRESSBOOK_TMOBILE := 108 
STATIC DEFINE EMAILADDRESSBOOK_TPHONE := 106 
STATIC DEFINE EMAILADDRESSBOOK_TSTATE := 105 
STATIC DEFINE EMAILADDRESSBOOK_TSTREET := 103 
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

  //{{%UC%}}
  //USER CODE STARTS HERE (do NOT remove this line)

  PROTECT lAppend AS LOGIC
  PROTECT InitCompleted AS LOGIC
  PROTECT _oDBServer AS DBServer
  PROTECT _aFields AS ARRAY


METHOD ControlVerify() 

	LOCAL aCtrls AS ARRAY
	LOCAL i AS DWORD
	LOCAL lRet AS LOGIC

	lRet := TRUE
	aCtrls := SELF:acontrols

	FOR i := 1 UPTO ALen(aCtrls)
		IF aCtrls[i]:NameSym == #COMPANY .and. ( IsNil(aCtrls[i]:@@Value) .or. Empty(aCtrls[i]:@@Value))
			TextBox{SELF,"Error", "This control cannot be empty"}:Show()
			aCtrls:SetFocus()
			lRet := FALSE
			EXIT
		ENDIF
		IF aCtrls[i]:NameSym == #EMAIL
			IF IsNil(aCtrls[i]:@@Value) .or. Empty(aCtrls[i]:@@Value)
				TextBox{SELF,"Error", "This control cannot be empty"}:Show()
				aCtrls:SetFocus()			
				lRet := FALSE
				EXIT
			ELSEIF At( "@", aCtrls[i]:@@Value )==0 .or. At(".",aCtrls[i]:@@Value) == 0
				TextBox{SELF,"Error", "The Email address is in an invalid format"}:Show()
				aCtrls:SetFocus()			
				lRet := FALSE
				EXIT			
			ENDIF
		ENDIF
	NEXT

	RETURN lRet			


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



METHOD FillControls(lNew AS LOGIC) AS VOID PASCAL 
   LOCAL dwI AS DWORD
   LOCAL symField AS SYMBOL
  
   SELF:lAppend := lNew
   
   FOR dwI := 1 UPTO 9
      symField := _aFields[dwI]
      SELF:FIELDPUT(symField, IF(lNew, "", _oDBServer:FIELDGET(symField)))
   NEXT dwI
   
	RETURN

CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  
LOCAL DIM aFonts[2] AS OBJECT

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"EmailAddressBook",_GetInst()},iCtlID)

aFonts[1] := Font{,24,"MS Sans Serif"}
aFonts[1]:Bold := TRUE
aFonts[2] := Font{,8,"MS Sans Serif"}
aFonts[2]:Bold := TRUE

oDCtCaption := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TCAPTION,_GetInst()}}
oDCtCaption:HyperLabel := HyperLabel{#tCaption,"Address Book Entry",NULL_STRING,NULL_STRING}
oDCtCaption:Font(aFonts[1], FALSE)

oDCtContact := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TCONTACT,_GetInst()}}
oDCtContact:HyperLabel := HyperLabel{#tContact,"Contact",NULL_STRING,NULL_STRING}
oDCtContact:TextColor := Color{COLORBLACK}
oDCtContact:Font(aFonts[2], FALSE)

oDCtCompany := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TCOMPANY,_GetInst()}}
oDCtCompany:HyperLabel := HyperLabel{#tCompany,"Company",NULL_STRING,NULL_STRING}
oDCtCompany:TextColor := Color{COLORBLACK}
oDCtCompany:Font(aFonts[2], FALSE)

oDCtStreet := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TSTREET,_GetInst()}}
oDCtStreet:HyperLabel := HyperLabel{#tStreet,"Street Address",NULL_STRING,NULL_STRING}
oDCtStreet:Font(aFonts[2], FALSE)

oDCtCITY := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TCITY,_GetInst()}}
oDCtCITY:HyperLabel := HyperLabel{#tCITY,"City",NULL_STRING,NULL_STRING}
oDCtCITY:Font(aFonts[2], FALSE)

oDCtSTATE := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TSTATE,_GetInst()}}
oDCtSTATE:HyperLabel := HyperLabel{#tSTATE,"State",NULL_STRING,NULL_STRING}
oDCtSTATE:Font(aFonts[2], FALSE)

oDCtPHONE := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TPHONE,_GetInst()}}
oDCtPHONE:HyperLabel := HyperLabel{#tPHONE,"Phone",NULL_STRING,NULL_STRING}
oDCtPHONE:Font(aFonts[2], FALSE)

oDCtFax := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TFAX,_GetInst()}}
oDCtFax:HyperLabel := HyperLabel{#tFax,"Fax",NULL_STRING,NULL_STRING}
oDCtFax:Font(aFonts[2], FALSE)

oDCtMobile := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TMOBILE,_GetInst()}}
oDCtMobile:HyperLabel := HyperLabel{#tMobile,"Mobile",NULL_STRING,NULL_STRING}
oDCtMobile:Font(aFonts[2], FALSE)

oDCtEMAIL := FixedText{SELF,ResourceID{EMAILADDRESSBOOK_TEMAIL,_GetInst()}}
oDCtEMAIL:HyperLabel := HyperLabel{#tEMAIL,"Email Address",NULL_STRING,NULL_STRING}
oDCtEMAIL:Font(aFonts[2], FALSE)

oDCCONTACT := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_CONTACT,_GetInst()}}
oDCCONTACT:HyperLabel := HyperLabel{#CONTACT,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCONTACT:UseHLforToolTip := True
oDCCONTACT:TooltipText := "Enter the full name of a friend"

oDCCOMPANY := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_COMPANY,_GetInst()}}
oDCCOMPANY:HyperLabel := HyperLabel{#COMPANY,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCOMPANY:UseHLforToolTip := True
oDCCOMPANY:TooltipText := "Enter the full name of a friend"

oDCSTREET := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_STREET,_GetInst()}}
oDCSTREET:HyperLabel := HyperLabel{#STREET,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSTREET:UseHLforToolTip := True
oDCSTREET:TooltipText := "Enter the street address of the person"

oDCCITY := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_CITY,_GetInst()}}
oDCCITY:HyperLabel := HyperLabel{#CITY,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCITY:TooltipText := "Enter the city of the person"

oDCSTATE := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_STATE,_GetInst()}}
oDCSTATE:HyperLabel := HyperLabel{#STATE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSTATE:UseHLforToolTip := True
oDCSTATE:TooltipText := "Enter the state "

oDCPHONE := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_PHONE,_GetInst()}}
oDCPHONE:HyperLabel := HyperLabel{#PHONE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCPHONE:UseHLforToolTip := True
oDCPHONE:TooltipText := "Enter the phone number of the person"

oDCMobile := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_MOBILE,_GetInst()}}
oDCMobile:HyperLabel := HyperLabel{#Mobile,NULL_STRING,NULL_STRING,NULL_STRING}
oDCMobile:UseHLforToolTip := True
oDCMobile:TooltipText := "Enter the phone number of the person"

oDCFax := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_FAX,_GetInst()}}
oDCFax:HyperLabel := HyperLabel{#Fax,NULL_STRING,NULL_STRING,NULL_STRING}
oDCFax:UseHLforToolTip := True
oDCFax:TooltipText := "Enter the phone number of the person"

oDCEMAIL := SingleLineEdit{SELF,ResourceID{EMAILADDRESSBOOK_EMAIL,_GetInst()}}
oDCEMAIL:HyperLabel := HyperLabel{#EMAIL,NULL_STRING,NULL_STRING,NULL_STRING}
oDCEMAIL:UseHLforToolTip := True
oDCEMAIL:TooltipText := "Enter the Email address of the person"

oCCPBNew := PushButton{SELF,ResourceID{EMAILADDRESSBOOK_PBNEW,_GetInst()}}
oCCPBNew:HyperLabel := HyperLabel{#PBNew,"New",NULL_STRING,NULL_STRING}
oCCPBNew:TooltipText := "To add a new friend"

oCCPBExit := PushButton{SELF,ResourceID{EMAILADDRESSBOOK_PBEXIT,_GetInst()}}
oCCPBExit:HyperLabel := HyperLabel{#PBExit,"Exit",NULL_STRING,NULL_STRING}
oCCPBExit:UseHLforToolTip := True
oCCPBExit:TooltipText := "Exit"

oCCPBSave := PushButton{SELF,ResourceID{EMAILADDRESSBOOK_PBSAVE,_GetInst()}}
oCCPBSave:HyperLabel := HyperLabel{#PBSave,"Save",NULL_STRING,NULL_STRING}
oCCPBSave:TooltipText := "To save entry"

SELF:Caption := "VO 2.7 Email Client Address Book"
SELF:HyperLabel := HyperLabel{#EmailAddressBook,"VO 2.7 Email Client Address Book",NULL_STRING,NULL_STRING}
SELF:AllowServerClose := False

if !IsNil(oServer)
	SELF:Use(oServer)
ENDIF

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD PBExit( ) 

	SELF:EndWindow()
	
	RETURN SELF

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


END CLASS
