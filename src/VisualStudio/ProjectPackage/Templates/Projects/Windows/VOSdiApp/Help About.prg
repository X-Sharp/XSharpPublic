#region DEFINES
static define HELPABOUT_ABOUTTEXT := 100
static define HELPABOUT_PUSHBUTTON1 := 101
static define HELPABOUT_FIXEDBITMAP1 := 102
#endregion

partial class HelpAbout inherit DIALOGWINDOW
	protect oDCAboutText as FIXEDTEXT
	protect oCCPushButton1 as PUSHBUTTON
	protect oDCFixedBitmap1 as FIXEDBITMAP

	// {{%UC%}} User code starts here (DO NOT remove this line)

constructor(oParent,uExtra)

	self:PreInit(oParent,uExtra)

	super(oParent , ResourceID{"HelpAbout" , _GetInst()} , true)

	self:oDCAboutText := FIXEDTEXT{self , ResourceID{ HELPABOUT_ABOUTTEXT  , _GetInst() } }
	SELF:oDCAboutText:HyperLabel := HyperLabel{#AboutText , "VO SDI Application" , NULL_STRING , NULL_STRING}

	self:oCCPushButton1 := PUSHBUTTON{self , ResourceID{ HELPABOUT_PUSHBUTTON1  , _GetInst() } }
	self:oCCPushButton1:HyperLabel := HyperLabel{#PushButton1 , "OK" , null_string , null_string}

	self:oDCFixedBitmap1 := FIXEDBITMAP{self , ResourceID{ HELPABOUT_FIXEDBITMAP1  , _GetInst() } }
	self:oDCFixedBitmap1:HyperLabel := HyperLabel{#FixedBitmap1 , "POWXSHARPBMP" , null_string , null_string}

	self:Caption := "About Standard Application"
	self:HyperLabel := HyperLabel{#HelpAbout , "About Standard Application" , null_string , null_string}

	self:PostInit(oParent,uExtra)

return


method PostInit(oParent,uExtra)
	local sVer as string
	local oSysLink as SysLink
	local oFT1 as FixedText
	local oHL1 as HyperLink
	local oFont1 as Font
	local s as string

	sVer := Version()
	sVer := SubStr(sVer, RAt2(" ", sVer)+1)
	oDCAboutText:CurrentText := _CHR(13)+" $safeprojectname$"+_CHR(13)+_CHR(13);
		+" X# Version "+sVer+_CHR(13)+_CHR(13);
		+" Copyright (c) XSharp BV 2015-2022"

	var point := oCCPushButton1:Origin
    point:x := self:oDCFixedBitmap1:Origin:x
	var size := Dimension{300,20}


	if IsThemeEnabled()
		s := "Visit <A HREF="+_CHR(34)+;
			"https://www.xsharp.eu"+_CHR(34)+">X#</A> on the web!"
		oSysLink := SysLink{self, -1, point, size, s}
		oSysLink:Show()
	else

		s := "Visit X# on the web:"
		oFT1 := FixedText{self, -1, point, dimension{200,20}, s}
		oFT1:show()

		oFont1 := Font{,8,"Microsoft Sans Serif"}
		oFont1:Underline := true

		oHL1 := HyperLink{self,-1,point,size,"https://www.xsharp.eu"}
		oHL1:font(oFont1)
		oHL1:size := size
		oHL1:textcolor := color{COLORBLUE}
		oHL1:font():underline := true
		oHL1:show()

	endif


	return nil


method PushButton1()

	self:EndDialog()

return self

end class
