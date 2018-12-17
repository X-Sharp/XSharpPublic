#region DEFINES
STATIC DEFINE ADDRESSFROMBOOK_FIXEDTEXT1 := 107 
STATIC DEFINE ADDRESSFROMBOOK_LOOKUP := 100 
STATIC DEFINE ADDRESSFROMBOOK_MULTILINEEDIT1 := 102 
STATIC DEFINE ADDRESSFROMBOOK_PBEDIT := 105 
STATIC DEFINE ADDRESSFROMBOOK_PBEXIT := 106 
STATIC DEFINE ADDRESSFROMBOOK_PBMOVE := 108 
STATIC DEFINE ADDRESSFROMBOOK_PBNEW := 104 
STATIC DEFINE ADDRESSFROMBOOK_PBOK := 103 
STATIC DEFINE ADDRESSFROMBOOK_SLELOOKUP := 101 
STATIC DEFINE LOOKUP_CONTACT := 100 
STATIC DEFINE LOOKUP_EMAIL := 101 
#endregion

CLASS AddressFrombook INHERIT DATADIALOG 

	EXPORT oDCSleLookUp AS SINGLELINEEDIT
	EXPORT oDCMultiLineEdit1 AS MULTILINEEDIT
	EXPORT oCCPBOk AS PUSHBUTTON
	EXPORT oCCPBNew AS PUSHBUTTON
	EXPORT oCCPBEdit AS PUSHBUTTON
	EXPORT oCCPBExit AS PUSHBUTTON
	EXPORT oDCFixedText1 AS FIXEDTEXT
	EXPORT oCCPBMove AS PUSHBUTTON
	EXPORT oSFLookUp AS LookUp

  //{{%UC%}}
  //USER CODE STARTS HERE (do NOT remove this line)
	EXPORT oControl AS USUAL

METHOD EditChange(oControlEvent) 

	LOCAL oControl AS Control
	LOCAL cContact AS STRING

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)

	SUPER:EditChange(oControlEvent)

	IF oControl != Null_Object .and. oControl:NameSym == #SLELookUp
		cContact := Trim(oControl:TextValue)
		IF !SELF:Server:Seek(cContact)
			MessageBox(NULL_PTR, PSZ("The case sensitive search failed to find " + cContact), PSZ("Lookup Failed"), MB_ICONSTOP+MB_OK)
		ENDIF
	ENDIF		

	RETURN NIL


CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  
LOCAL DIM aFonts[2] AS OBJECT

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"AddressFrombook",_GetInst()},iCtlID)

aFonts[1] := Font{,12,"MS Sans Serif"}
aFonts[1]:Bold := TRUE
aFonts[2] := Font{,8,"Microsoft Sans Serif"}
aFonts[2]:Bold := TRUE

oDCSleLookUp := SingleLineEdit{SELF,ResourceID{ADDRESSFROMBOOK_SLELOOKUP,_GetInst()}}
oDCSleLookUp:HyperLabel := HyperLabel{#SleLookUp,NULL_STRING,NULL_STRING,NULL_STRING}

oDCMultiLineEdit1 := MultiLineEdit{SELF,ResourceID{ADDRESSFROMBOOK_MULTILINEEDIT1,_GetInst()}}
oDCMultiLineEdit1:HyperLabel := HyperLabel{#MultiLineEdit1,NULL_STRING,NULL_STRING,NULL_STRING}

oCCPBOk := PushButton{SELF,ResourceID{ADDRESSFROMBOOK_PBOK,_GetInst()}}
oCCPBOk:HyperLabel := HyperLabel{#PBOk,"OK",NULL_STRING,NULL_STRING}

oCCPBNew := PushButton{SELF,ResourceID{ADDRESSFROMBOOK_PBNEW,_GetInst()}}
oCCPBNew:HyperLabel := HyperLabel{#PBNew,"New",NULL_STRING,NULL_STRING}

oCCPBEdit := PushButton{SELF,ResourceID{ADDRESSFROMBOOK_PBEDIT,_GetInst()}}
oCCPBEdit:HyperLabel := HyperLabel{#PBEdit,"Edit",NULL_STRING,NULL_STRING}

oCCPBExit := PushButton{SELF,ResourceID{ADDRESSFROMBOOK_PBEXIT,_GetInst()}}
oCCPBExit:HyperLabel := HyperLabel{#PBExit,"Exit",NULL_STRING,NULL_STRING}

oDCFixedText1 := FixedText{SELF,ResourceID{ADDRESSFROMBOOK_FIXEDTEXT1,_GetInst()}}
oDCFixedText1:HyperLabel := HyperLabel{#FixedText1,"Look Up",NULL_STRING,NULL_STRING}
oDCFixedText1:Font(aFonts[1], FALSE)

oCCPBMove := PushButton{SELF,ResourceID{ADDRESSFROMBOOK_PBMOVE,_GetInst()}}
oCCPBMove:HyperLabel := HyperLabel{#PBMove,"ADD TO LIST",NULL_STRING,NULL_STRING}
oCCPBMove:Font(aFonts[2], FALSE)

SELF:Caption := "VO 2.7 Email Client Address Book Look-Up"
SELF:HyperLabel := HyperLabel{#AddressFrombook,"VO 2.7 Email Client Address Book Look-Up",NULL_STRING,NULL_STRING}
SELF:AllowServerClose := True

if !IsNil(oServer)
	SELF:Use(oServer)
ENDIF

oSFLookUp := LookUp{SELF,ADDRESSFROMBOOK_LOOKUP}
oSFLookUp:show()

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD PBEdit( ) 
	
	LOCAL oWin AS EmailAddressBook

	oWin := EmailAddressBook{SELF:Owner,,SELF:Server,FALSE}
	oWin:Show()
	
	SELF:Server:Skip(0)
	
	RETURN SELF


METHOD PBExit( ) 

	SELF:EndWindow()
	
	RETURN SELF

METHOD PBMove( ) 

	LOCAL cName, cEmail, cEntry, cTemp AS STRING

	cTemp := AllTrim(SELF:oDCMultiLineEdit1:TextValue)
	cEmail := Trim(SELF:Server:FIELDGET(#EMAIL))
	cName := Trim(SELF:Server:FIELDGET(#CONTACT))
	IF !Empty(cName)
		cEntry := e"\"" + cName + e"\" <" + cEmail + e">"
	ELSE
		cEntry := cEmail
	ENDIF

	IF Empty(cTemp)
		cTemp := cEntry
	ELSE
		cTemp +=  +"; "+ cEntry
	ENDIF

	SELF:oDCMultiLineEdit1:Value := cTemp

	RETURN SELF

METHOD PBNew( ) 

	LOCAL oWin AS EmailAddressBook

	oWin := EmailAddressBook{SELF:Owner,,SELF:Server,TRUE}
	oWin:Show()
	
	SELF:Server:Skip(0)
	
	RETURN SELF

METHOD PBOk( ) 

	IF !IsNil(oControl)
		oControl:Value := oDCMULtiLineEdit1:Value
	ENDIF		

	SELF:EndWindow()	
	
	RETURN SELF

METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 

	IF IsNil(uExtra)
		// Use this window just as a lookup so disable the MLE and button
		SELF:oCCPBMove:Disable()
		SELF:oDCMultiLineEdit1:Disable()
		SELF:oCCPBOk:Disable()
	ELSE
		// Using this window to select email addresses
		SELF:oControl := uExtra
		SELF:oDCMultiLineEdit1:Value := uExtra:Value
	ENDIF

	RETURN NIL


END CLASS
CLASS Lookup INHERIT DATAWINDOW 

	PROTECT oDBCONTACT as DataColumn
	PROTECT oDBEMAIL as DataColumn

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"Lookup",_GetInst()},iCtlID)

SELF:Caption := "DataWindow Caption"
SELF:HyperLabel := HyperLabel{#Lookup,"DataWindow Caption",NULL_STRING,NULL_STRING}

if !IsNil(oServer)
	SELF:Use(oServer)
ELSE
	SELF:Use(SELF:Owner:Server)
ENDIF
self:Browser := LookUpDataBrowser{self}

oDBCONTACT := DataColumn{33}
oDBCONTACT:Width := 33
oDBCONTACT:HyperLabel := HyperLabel{#CONTACT,"Contact:",NULL_STRING,"Contacts_CONTACT"} 
oDBCONTACT:Caption := "Contact:"
self:Browser:AddColumn(oDBCONTACT)

oDBEMAIL := DataColumn{35}
oDBEMAIL:Width := 35
oDBEMAIL:HyperLabel := HyperLabel{#EMAIL,"Email:",NULL_STRING,"Contacts_EMAIL"} 
oDBEMAIL:Caption := "Email:"
self:Browser:AddColumn(oDBEMAIL)


SELF:ViewAs(#BrowseView)

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 

	SELF:Browser:SetStandardStyle( GBSREADONLY )

	RETURN NIL


END CLASS
CLASS LookUpDataBrowser INHERIT DataBrowser

METHOD CellDoubleClick() 
	
	SELF:Owner:Owner:PBMove()
	
	RETURN SELF



END CLASS
