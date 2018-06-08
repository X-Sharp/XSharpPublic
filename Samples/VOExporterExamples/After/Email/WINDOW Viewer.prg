#region DEFINES
STATIC DEFINE VIEWERDLG_INFO := 100
STATIC DEFINE VIEWERDLG_BTNOK := 101
STATIC DEFINE VIEWERDLG_SIZER := 102
#endregion

CLASS ViewerDlg INHERIT DIALOGWINDOW
	PROTECT oDCInfo AS MULTILINEEDIT
	PROTECT oCCBtnOK AS PUSHBUTTON
	PROTECT oDCSizer AS VERTICALSCROLLBAR

	// {{%UC%}} User code starts here (DO NOT remove this line)  

METHOD BtnOK( ) 
   SELF:EndDialog(IDOK)
   RETURN SELF

CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"ViewerDlg" , _GetInst()} , TRUE)

	SELF:oDCInfo := MULTILINEEDIT{SELF , ResourceID{ VIEWERDLG_INFO  , _GetInst() } }
	oFont := Font{  , 8 , "Courier New" }
	SELF:oDCInfo:Font( oFont )
	SELF:oDCInfo:OwnerAlignment := OA_WIDTH_HEIGHT
	SELF:oDCInfo:HyperLabel := HyperLabel{#Info , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCBtnOK := PUSHBUTTON{SELF , ResourceID{ VIEWERDLG_BTNOK  , _GetInst() } }
	SELF:oCCBtnOK:OwnerAlignment := OA_X_Y
	SELF:oCCBtnOK:HyperLabel := HyperLabel{#BtnOK , "OK" , NULL_STRING , NULL_STRING}

	SELF:oDCSizer := VERTICALSCROLLBAR{SELF , ResourceID{ VIEWERDLG_SIZER  , _GetInst() } }
	SELF:oDCSizer:OwnerAlignment := OA_X_Y
	SELF:oDCSizer:HyperLabel := HyperLabel{#Sizer , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:Caption := ""
	SELF:HyperLabel := HyperLabel{#ViewerDlg , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD PostInit(oParent,uExtra) 
	//Put your PostInit additions here
	
	SELF:Icon := Aap_Email_Icon{}
	
	oDCSizer:SetStyle(SBS_SIZEGRIP, TRUE)
	
	RETURN NIL

ASSIGN TextValue(cValue) 
   RETURN oDCInfo:TextValue := cValue

ASSIGN TextLimit(nBytes) 
   LOCAL liCount as DWORD

   IF IsNumeric(nBytes) .and. nBytes > 0
      liCount := nBytes
      SendMessage(oDCInfo:Handle(), EM_LIMITTEXT, liCount, 0L)
   ENDIF

   RETURN nBytes


END CLASS
CLASS MessageSourceDialog INHERIT ViewerDlg

METHOD PostInit(oParent,uExtra) 

	SUPER:PostInit()
	
	SELF:Size := Dimension{500, 300}
	
	SELF:Caption   := "Message Source"
	SELF:TextLimit := 1000000L
	SELF:TextValue := uExtra
   RETURN SELF

END CLASS
CLASS MailIDsDialog INHERIT ViewerDlg

METHOD PostInit(oParent,uExtra) 

	SUPER:PostInit()
	
	SELF:Size := Dimension{250,300}
	
	SELF:Caption   := "Mail IDs"
	SELF:TextValue := uExtra
   RETURN SELF


END CLASS
