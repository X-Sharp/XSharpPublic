#region DEFINES
STATIC DEFINE HELPABOUT_ABOUTTEXT := 100 
STATIC DEFINE HELPABOUT_FIXEDBITMAP1 := 102 
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 101 
#endregion

class HelpAbout inherit DIALOGWINDOW 

	protect oDCAboutText as FIXEDTEXT
	protect oCCPushButton1 as PUSHBUTTON
	protect oDCFixedBitmap1 as FIXEDBITMAP

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oParent,uExtra)  

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"HelpAbout",_GetInst()},TRUE)

oDCAboutText := FixedText{self,ResourceID{HELPABOUT_ABOUTTEXT,_GetInst()}}
oDCAboutText:HyperLabel := HyperLabel{#AboutText,"VOPad",NULL_STRING,NULL_STRING}

oCCPushButton1 := PushButton{self,ResourceID{HELPABOUT_PUSHBUTTON1,_GetInst()}}
oCCPushButton1:HyperLabel := HyperLabel{#PushButton1,"OK",NULL_STRING,NULL_STRING}
oCCPushButton1:Image := IMAGE{}

oDCFixedBitmap1 := FIXEDBITMAP{self,ResourceID{HELPABOUT_FIXEDBITMAP1,_GetInst()}}
oDCFixedBitmap1:HyperLabel := HyperLabel{#FixedBitmap1,"POWVOBMP",NULL_STRING,NULL_STRING}

self:Caption := "About Standard Application"
self:HyperLabel := HyperLabel{#HelpAbout,"About Standard Application",NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


METHOD PostInit(oParent,uExtra) 
	LOCAL sVer AS STRING
	LOCAL oSysLink AS SysLink
	LOCAL oFT1, oFT2 AS FixedText
	LOCAL oHL1, oHL2 AS HyperLink
	LOCAL oFont1, oFont2 AS Font
	LOCAL s AS STRING

	sVer := Version()
	sVer := SubStr(sVer, RAt2(" ", sVer)+1)
	oDCAboutText:CurrentText := _Chr(13)+" Visual Objects VOPad"+_Chr(13)+_Chr(13);
		+" Version "+sVer+_Chr(13)+_Chr(13);
		+" © 1994-"+Str2(Year(Today()),4)+" GrafX Software Dev. Inc.& CA INT"
		
	IF IsThemeEnabled()
		s := "Visit <A HREF="+_chr(34)+;
			"http://www.grafxsoft.com"+_chr(34)+">GrafX</A> and <A HREF="+_CHR(34)+;
			"http://www.cavo.com"+_Chr(34)+">Visual Objects</A> on the web!"
		oSysLink := SysLink{SELF, -1, Point{10,5}, Dimension{300,20}, s}
		oSysLink:Show()
	ELSE
		
		s := "Visit GrafX on the web:"
		oFT1 := FixedText{SELF, -1, Point{10,25}, dimension{200,20}, s}
		oFT1:show()

		s := "Visit Visual Objects on the web:"
		oFT2 := FixedText{SELF, -1, Point{10,5}, dimension{200,20}, s}
		oFT2:show()
		
		oFont1 := Font{,8,"Microsoft Sans Serif"}
		oFont1:Underline := TRUE
		
		oHL1 := HyperLink{SELF,-1,point{190,25},dimension{0,0},"http://www.grafxsoft.com"}
		oHL1:font := oFont1
		oHL1:size := dimension{150,20}
		oHL1:textcolor := color{COLORBLUE}
		oHL1:font:underline := TRUE
		oHL1:show()

		oFont2 := Font{,8,"Microsoft Sans Serif"}
		oFont2:Underline := TRUE

		oHL2 := HyperLink{SELF,-1,point{190,5},dimension{0,0},"http://www.cavo.com"}
		oHL2:font := oFont2
		oHL2:size := dimension{150,20}
		oHL2:textcolor := color{COLORBLUE}
		oHL2:font:underline := TRUE
		oHL2:show()
		
	ENDIF


	RETURN NIL


METHOD PushButton1() 

	SELF:EndDialog()
	

END CLASS
