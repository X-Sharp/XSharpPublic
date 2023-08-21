#region DEFINES
STATIC DEFINE HELPABOUTDIALOG_ENDDIALOG := 102 
STATIC DEFINE HELPABOUTDIALOG_LABEL1 := 100 
STATIC DEFINE HELPABOUTDIALOG_LABEL2 := 101 
#endregion

class HelpAboutDialog inherit DIALOGWINDOW 

	protect oDCLabel1 as FIXEDTEXT
	protect oDCLabel2 as FIXEDTEXT
	protect oCCEndDialog as PUSHBUTTON

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oParent,uExtra)  

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"HelpAboutDialog",_GetInst()},TRUE)

oDCLabel1 := FixedText{self,ResourceID{HELPABOUTDIALOG_LABEL1,_GetInst()}}
oDCLabel1:HyperLabel := HyperLabel{#Label1,"CA-Visual Objects 2.5a",NULL_STRING,NULL_STRING}

oDCLabel2 := FixedText{self,ResourceID{HELPABOUTDIALOG_LABEL2,_GetInst()}}
oDCLabel2:HyperLabel := HyperLabel{#Label2,"Explorer Sample",NULL_STRING,NULL_STRING}

oCCEndDialog := PushButton{self,ResourceID{HELPABOUTDIALOG_ENDDIALOG,_GetInst()}}
oCCEndDialog:HyperLabel := HyperLabel{#EndDialog,"OK",NULL_STRING,NULL_STRING}

self:Caption := "About Explorer Sample..."
self:HyperLabel := HyperLabel{#HelpAboutDialog,"About Explorer Sample...",NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


END CLASS
