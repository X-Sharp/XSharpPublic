#region DEFINES
STATIC DEFINE HELPABOUT_ABOUTTEXT := 100
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 101
STATIC DEFINE HELPABOUT_FIXEDBITMAP1 := 102
#endregion

PARTIAL CLASS HelpAbout INHERIT DIALOGWINDOW
	PROTECT oDCAboutText AS FIXEDTEXT
	PROTECT oCCPushButton1 AS PUSHBUTTON
	PROTECT oDCFixedBitmap1 AS FIXEDBITMAP

	// {{%UC%}} User code starts here (DO NOT remove this line)

CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"HelpAbout" , _GetInst()} , TRUE)

	SELF:oDCAboutText := FIXEDTEXT{SELF , ResourceID{ HELPABOUT_ABOUTTEXT  , _GetInst() } }
	SELF:oDCAboutText:HyperLabel := HyperLabel{#AboutText , "VO MDI Application" , NULL_STRING , NULL_STRING}

	SELF:oCCPushButton1 := PUSHBUTTON{SELF , ResourceID{ HELPABOUT_PUSHBUTTON1  , _GetInst() } }
	SELF:oCCPushButton1:HyperLabel := HyperLabel{#PushButton1 , "OK" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedBitmap1 := FIXEDBITMAP{SELF , ResourceID{ HELPABOUT_FIXEDBITMAP1  , _GetInst() } }
	SELF:oDCFixedBitmap1:HyperLabel := HyperLabel{#FixedBitmap1 , "POWXSHARPBMP" , NULL_STRING , NULL_STRING}

	SELF:Caption := "About Standard Application"
	SELF:HyperLabel := HyperLabel{#HelpAbout , "About Standard Application" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD PostInit(oParent,uExtra)
	LOCAL sVer AS STRING
	LOCAL oSysLink AS SysLink
	LOCAL oFT1 AS FixedText
	LOCAL oHL1 AS HyperLink
	LOCAL oFont1 AS Font
	LOCAL s AS STRING

	sVer := Version()
	sVer := SubStr(sVer, RAt2(" ", sVer)+1)
	oDCAboutText:CurrentText := _CHR(13)+" $safeprojectname$"+_CHR(13)+_CHR(13);
		+" X# Version "+sVer+_CHR(13)+_CHR(13);
		+" Copyright (c) XSharp BV 2015-2022"

	var point := oCCPushButton1:Origin
    point:x := self:oDCFixedBitmap1:Origin:x
	var size := Dimension{300,20}


	IF IsThemeEnabled()
		s := "Visit <A HREF="+_CHR(34)+;
			"https://www.xsharp.eu"+_CHR(34)+">X#</A> on the web!"
		oSysLink := SysLink{self, -1, point, size, s}
		oSysLink:Show()
	ELSE

		s := "Visit X# on the web:"
		oFT1 := FixedText{self, -1, point, dimension{200,20}, s}
		oFT1:show()

		oFont1 := Font{,8,"Microsoft Sans Serif"}
		oFont1:Underline := TRUE

		oHL1 := HyperLink{self,-1,point,size,"https://www.xsharp.eu"}
		oHL1:font(oFont1)
		oHL1:size := size
		oHL1:textcolor := color{COLORBLUE}
		oHL1:font():underline := TRUE
		oHL1:show()

	ENDIF


	RETURN NIL


METHOD PushButton1()

	SELF:EndDialog()

RETURN SELF

END CLASS
