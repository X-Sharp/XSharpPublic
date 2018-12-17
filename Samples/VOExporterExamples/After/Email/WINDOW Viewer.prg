#region DEFINES
STATIC DEFINE VIEWERDLG_BTNOK := 101 
STATIC DEFINE VIEWERDLG_INFO := 100 
STATIC DEFINE VIEWERDLG_SIZER := 102 
#endregion

CLASS MailIDsDialog INHERIT ViewerDlg

METHOD PostInit(oParent,uExtra) 

	SUPER:PostInit()
	
	SELF:Size := Dimension{250,300}
	
	SELF:Caption   := "Mail IDs"
	SELF:TextValue := uExtra
   RETURN SELF

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
class ViewerDlg inherit DIALOGWINDOW 

	protect oDCInfo as MULTILINEEDIT
	protect oCCBtnOK as PUSHBUTTON
	protect oDCSizer as VERTICALSCROLLBAR

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

METHOD BtnOK( ) 
   SELF:EndDialog(IDOK)
   RETURN SELF

CONSTRUCTOR(oParent,uExtra)  
local dim aFonts[1] AS OBJECT

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"ViewerDlg",_GetInst()},TRUE)

aFonts[1] := Font{,8,"Courier New"}

oDCInfo := MultiLineEdit{self,ResourceID{VIEWERDLG_INFO,_GetInst()}}
oDCInfo:HyperLabel := HyperLabel{#Info,NULL_STRING,NULL_STRING,NULL_STRING}
oDCInfo:OwnerAlignment := OA_WIDTH_HEIGHT
oDCInfo:Font(aFonts[1], FALSE)

oCCBtnOK := PushButton{self,ResourceID{VIEWERDLG_BTNOK,_GetInst()}}
oCCBtnOK:HyperLabel := HyperLabel{#BtnOK,"OK",NULL_STRING,NULL_STRING}
oCCBtnOK:OwnerAlignment := OA_X_Y

oDCSizer := VerticalScrollBar{self,ResourceID{VIEWERDLG_SIZER,_GetInst()}}
oDCSizer:HyperLabel := HyperLabel{#Sizer,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSizer:OwnerAlignment := OA_X_Y

self:Caption := ""
self:HyperLabel := HyperLabel{#ViewerDlg,NULL_STRING,NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


METHOD PostInit(oParent,uExtra) 
	//Put your PostInit additions here
	
	SELF:Icon := Aap_Email_Icon{}
	
	oDCSizer:SetStyle(SBS_SIZEGRIP, TRUE)
	
	RETURN NIL

ASSIGN TextLimit(nBytes) 
   LOCAL liCount as DWORD

   IF IsNumeric(nBytes) .and. nBytes > 0
      liCount := nBytes
      SendMessage(oDCInfo:Handle(), EM_LIMITTEXT, liCount, 0L)
   ENDIF

   RETURN nBytes


ASSIGN TextValue(cValue) 
   RETURN oDCInfo:TextValue := cValue

END CLASS
