#region DEFINES
STATIC DEFINE OMARGINDIALOG_FTLM := 100
STATIC DEFINE OMARGINDIALOG_FTRM := 101
STATIC DEFINE OMARGINDIALOG_SLELM := 102
STATIC DEFINE OMARGINDIALOG_SLERM := 103
STATIC DEFINE OMARGINDIALOG_PBOK := 104
STATIC DEFINE OMARGINDIALOG_PBCANCEL := 105
STATIC DEFINE PAGEDLG_FTPAGEWIDTH := 101
STATIC DEFINE PAGEDLG_PBOK := 102
STATIC DEFINE PAGEDLG_PBCANCEL := 103
STATIC DEFINE PAGEDLG_VERTICALSPINNER1 := 104
STATIC DEFINE PAGEDLG_SLEWIDTH := 100
#endregion

CLASS PadWin INHERIT ChildAppWindow
	EXPORT oDCRichEdit AS RICHEDIT
	
	EXPORT cFName 			AS STRING
	EXPORT oCB 				AS SearchBox //ComboBox
	EXPORT nLeftMargin, nRightMargin AS LONGINT
	EXPORT nStartSearch AS LONGINT
	EXPORT oRange 			AS Range
	EXPORT nWidth 			AS LONGINT
	EXPORT lChanged 		AS LOGIC
	
	

CONSTRUCTOR(oOwner, lManaged, lImpl, cFileName,lReadOnly) 
	LOCAL kRTFStyle AS LONG
	
	kRTFStyle := _OR(ES_WANTRETURN,ES_AUTOVSCROLL)
	kRTFStyle := _OR(kRTFStyle, ES_MULTILINE)
	kRTFStyle := _OR(kRTFStyle, WS_CHILD)
	kRTFStyle := _OR(kRTFStyle, WS_BORDER)
	kRTFStyle := _OR(kRTFStyle, WS_VSCROLL)
	kRTFStyle := _OR(kRTFStyle, WS_EX_CLIENTEDGE )
	kRTFStyle := _OR(kRTFStyle, WS_HSCROLL )
	kRTFStyle := _OR(kRTFStyle, ES_AUTOHSCROLL )
	
	SELF:PreInit(cFileName, lReadOnly)
	
	LoadLibrary(String2Psz("RICHED32.DLL"))
	SUPER(oOwner, lManaged, lImpl)
	
	
	oDCRichEdit := RichEdit{SELF, 4711, Point{10, 10}, Dimension{2, 2}, kRTFStyle}
	oDCRichEdit:HyperLabel := HyperLabel{#RichEdit,NULL_STRING,NULL_STRING,NULL_STRING}
	
	oDCRichEdit:Show()
	
	SELF:Caption := "DataWindow Caption"
	SELF:HyperLabel := HyperLabel{#PadWin,"DataWindow Caption",NULL_STRING,NULL_STRING}
	
	SELF:PostInit(cFileName)
	
	RETURN SELF
	
	

METHOD RtfSave() 
	IF Left(cFName, 8) = "Untitled"
		SELF:RtfSaveAs()
	ELSE	
		IF Upper(Right(cFName, 3)) = "RTF"
			SELF:oDCRichEdit:SaveToFile(cFName)
		 ELSE
			MemoWrit(cFName, SELF:oDCRichedit:TextValue)
		ENDIF
	ENDIF	
	SELF:lChange := FALSE
	
	

method RtfSaveAs() 
	local oOD as SaveAsDialog
	local oTB as TextBox
	local lWrite := true as logic
	local cExt as string
	local aFilter, aFilterDesc as array
	local cFileName as string
	aFilter := {"*.RTF", "*.TXT"}
	aFilterDesc := {"Rich Edit Text", "ASCII"}
	oOD := SaveAsDialog{self, ".\" + cFName}
	oOD:SetFilter(aFilter, aFilterDesc, 1)
	oOD:Show()
	cExt := Right(aFilter[oOD:FilterIndex], 4)
	cFileName := oOD:FileName + cExt
	if Empty(cFileName)
		lWrite := false
	endif		
	if File(cFilename)
		oTB := TextBox{self, "File already exist!", "Overwrite ?" }
		oTB:Type := BUTTONYESNO + BOXICONHAND + MB_DEFBUTTON2	
		if oTB:Show() = BOXREPLYNO
			lWrite := false
		endif
	endif
	if lWrite
		if cExt = ".RTF"
			self:oDCRichEdit:SaveToFile(cFilename)
		 else	
		 	MemoWrit(cFileName, oDCRichEdit:TextValue)
		endif
		cFName := cFilename
		self:Caption := cFname
	endif




METHOD QueryClose( oEvent ) 
	LOCAL lAllowClose := TRUE AS LOGIC
	LOCAL oTB AS TextBox
	LOCAL cCaption := oEvent:window:caption AS STRING
	LOCAL nBoxReply AS INT

	IF SELF:lChange
		oTB := TextBox{SELF, "File has changed!", "Save " + cCaption + " ?" }
		oTB:Type := BUTTONYESNOCANCEL + BOXICONHAND + MB_DEFBUTTON2	
		nBoxReply := oTB:Show()
		DO CASE
			CASE nBoxReply = BOXREPLYYES
				SELF:RtfSave()
				SELF:lChange := FALSE
			CASE nBoxReply = BOXREPLYNO
			CASE nBoxReply = BOXREPLYCANCEL
				lAllowClose := FALSE
		ENDCASE
	ENDIF

	RETURN lAllowClose


METHOD MenuCommand(oMenuCommandEvent) 
	SUPER:MenuCommand(oMenuCommandEvent)
	//Put your changes here
	DO CASE
		CASE oMenuCommandEvent:NameSym = #Edit_Find
			SELF:Find(TRUE)
		CASE oMenuCommandEvent:NameSym = #Edit_Find_Next
			SELF:Find(FALSE)
		CASE oMenuCommandEvent:NameSym = #Edit_AlignMent_Left
			SELF:RTFAlign(REPARA_LEFT)			
		CASE oMenuCommandEvent:NameSym = #Edit_AlignMent_Right
			SELF:RTFAlign(REPARA_RIGHT)			
		CASE oMenuCommandEvent:NameSym = #Edit_AlignMent_Center
			SELF:RTFAlign(REPARA_CENTER)			
	ENDCASE			
	RETURN NIL


METHOD RTFAlign(nVal) 
	SELF:oDCRichEdit:Alignment := nVal



METHOD SetFont() 
	SELF:oDCRichEdit:RTFChangeFont()
	

METHOD SetMargins() 
	LOCAL oMarginDlg AS oMarginDialog
	LOCAL nLM, nRM AS INT
	oMarginDlg := oMarginDialog{SELF}
	oMarginDlg:show()	
    IF oMarginDlg:Result != 0
	  nLM := Val(oMarginDlg:slelm:TextValue)
		nRM := Val(oMarginDlg:slerm:TextValue)
		SELF:oDCRichEdit:Margin(nLM * 1440 , (7 - nRM) * 1440, 0)
	ENDIF	
	

METHOD PostInit(cFileName) 
	LOCAL pt			   IS _WINPOINT
	LOCAL hDC              AS PTR
	LOCAL liOldMapMode     AS LONG
	nLeftMargin := 0
	nRightMargin := 8 * 1440
	SELF:odcrichedit:Modified := FALSE
	
	SELF:InitAppearance()
	
	hDC := GetDC(SELF:handle())
	
	liOldMapMode := SetMapMode(hDC, MM_TWIPS)
	
	pt.x := nWidth
	pt.y := 0
	
	LPtoDP(hDC, @pt, 1)
	
	SetMapMode(hDC, liOldMapMode)
	DeleteDC(hDC)
	
	nWidth := INT(pt.x *  1.181019332162)
	
	RETURN NIL
	

METHOD Close() 
SELF:owner:RemoveChild(self)
SELF:EndWindow()

RETURN NIL



ACCESS lChange 
	RETURN lChanged


ASSIGN lChange(uValue) 
	RETURN lChanged := uValue



METHOD EditChange(oControlEvent) 
	LOCAL oControl AS Control
	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
	SUPER:EditChange(oControlEvent)
	//Put your changes here
	SELF:lChange := TRUE
	SELF:owner:menu:EnableItem(IDM_StandardPadMenu_File_Save_ID)
	RETURN NIL



METHOD SaveAsASCII(cFile) 
	
RETURN NIL


METHOD InitAppearance() 
	LOCAL hDC AS PTR
	LOCAL cf IS _WINCHARFORMAT
	nWidth := 7 * 1440         // 7 inches
	hDC := CreateCompatibleDC(NULL)
	
	IF SendMessage(oDCRichEdit:handle(), EM_SETTARGETDEVICE, DWORD(_CAST, hDC),  nWidth) != 0
		SetProp(oDCRichEdit:Handle(), PSZ("TargetDC"), hDC)
	ELSE
		DeleteDC(hDC)
	ENDIF
	
	cf.cbSize := _sizeof(_WINCHARFORMAT)
	cf.dwMask := DWORD(_CAST, _OR( CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT,;
		CFM_UNDERLINE, CFM_COLOR, CFM_OFFSET, CFM_PROTECTED))
	cf.dwEffects := CFE_AUTOCOLOR
	cf.yHeight := 200
	cf.yOffset := 0
	cf.crTextColor := 0
	cf.bPitchAndFamily :=  _OR(FF_SWISS, VARIABLE_PITCH)
	lstrcpy(PSZ(_CAST, @cf.szFaceName[1]), PSZ(_CAST, "Arial"))
	
	// Set the default character format.
	SendMessage(oDCRichEdit:Handle(), EM_SETCHARFORMAT, SCF_SELECTION, LONG(_CAST,@cf))
	

METHOD Find(lVal) 
	LOCAL cSearch AS STRING	
	LOCAL oSelect AS Selection
	LOCAL nLen AS LONG
	
	cSearch := SELF:owner:oCB:CurrentText
	IF lVal   			// search from the top
		IF AScan(aCB, cSearch) = 0
			AAdd(aCB, cSearch)
			SELF:owner:oCB:FillUsing(aCB)
		ENDIF
		nStartSearch := 1
	ENDIF
	nLen := oDCRichEdit:Length + 1
	SELF:owner:oCb:CurrentItem := cSearch
	SELF:oDCRichEdit:SetFocus()
	oRange := oDCRichEdit:Seek(cSearch, Range{nStartSearch, nLen}, , , TRUE)
	IF oRange = Null_Object
		SELF:owner:StatusBar:setmessage("Text not found", MESSAGEERROR)
		SELF:owner:StatusBar:DisplayMessage()
	ELSE					
		oSelect := Selection{}
		oSelect:Start := oRange:Min
		oSelect:Finish := oRange:Max
		oDCRichEdit:Selection := oSelect
		oDCRichEdit:ShowSelection()
		nStartSearch := oRange:Max + 1
	ENDIF	
	
	

METHOD Copy() 
	SELF:oDCRichEdit:Copy()
	RETURN NIL


METHOD Paste() 
	SELF:oDCRichEdit:Paste()
	RETURN NIL


METHOD CloseAll() 
SELF:Owner:CloseAllChildren()

RETURN NIL


METHOD FilePrint() 
	SELF:oDCRichedit:Print()
	RETURN NIL


METHOD Cut() 
	SELF:oDCRichEdit:Cut()
	RETURN NIL


METHOD RTFPageSetup() 
	LOCAL oTB AS TextBox

	oTB := TextBox{SELF, "Info", "Not yet implemented."}
	oTB:Show()
	
	

METHOD Expose(oExposeEvent) 
   //07/20/04 S.Ebert
	LOCAL oRect AS RectangleObject
	LOCAL oLine AS LineObject		
	LOCAL oText AS TextObject		
	LOCAL liTop AS LONG
	LOCAL dwPos AS DWORD
	LOCAL oSize AS Dimension

   oSize := SELF:CanvasArea:Size
	
   liTop := oSize:Height - 25

	// white Line at the bottom
	oLine := LineObject{Point{1, liTop + 1}, Point{oSize:width - 13, liTop + 1}, Pen{Color{COLORWHITE}}}
	SELF:Draw(oLine)
	
	// white rect, dark gray framed
	oRect := RectangleObject{Point{1, liTop+2}, Dimension{oSize:width, 20}, Pen{COLOR{127, 127, 127}}, Brush{COLOR{COLORWHITE}}}
	SELF:Draw(oRect)	
	
	// black line at the top of the white area
	oLine := LineObject{Point{2, liTop + 21}, Point{nwidth, liTop + 21}, Pen{Color{COLORBLACK}}}
	SELF:Draw(oLine)
	
    // gray the non writeable area
	oRect := RectangleObject{Point{nWidth + 1, liTop + 3}, Dimension{oSize:Width-nWidth-2, 18}, Pen{COLOR{191, 191, 191}}, Brush{COLOR{191, 191, 191}}}
	SELF:Draw(oRect)	

	// white line at the top and left side of the gray area
	oLine := LineObject{Point{nWidth + 1, liTop + 21}, Point{oSize:Width, liTop + 21}, Pen{Color{COLORWHITE}}}
	SELF:Draw(oLine)
	oLine := LineObject{Point{nWidth + 1, liTop + 20}, Point{nWidth + 1, liTop + 2}, Pen{Color{COLORWHITE}}}
	SELF:Draw(oLine)
	oLine := LineObject{Point{nWidth + 1, liTop + 20}, Point{nWidth + 1, liTop + 2}, Pen{Color{127, 127, 127}}}
	SELF:Draw(oLine)

   // draw the numbers and halfs
   dwPos := 96
   DO WHILE dwPos - 48 < oSize:Width
		oText := TextObject{Point{dwPos, liTop + 4}, AllTrim(AsString(dwPos/96))}
		SELF:Draw(oText)
		oText := TextObject{Point{dwPos - 48, liTop + 5}, "|"}
		SELF:Draw(oText)
		dwPos += 96
	ENDDO


	RETURN NIL


METHOD Resize(oResizeEvent) 
   //07/20/04 S.Ebert
   LOCAL sRect IS _WINRECT

   SUPER:ReSize(oResizeEvent)

   IF SELF:oDCRichedit != NULL_OBJECT
		GetClientRect(SELF:Handle(), @sRect)
		MoveWindow(oDCRichEdit:handle(), sRect.left, sRect.top + 25 , sRect.right, sRect.bottom - 25, TRUE)
	ENDIF

   RETURN NIL

END CLASS
CLASS SearchBox INHERIT ComboBox
	

METHOD Dispatch(e) 
	
	IF e:uMSg == wM_KEYDOWN .or.  e:uMSg == wM_KEYUP
		Tone(440,2)
	ELSEIF e:uMSg == wM_CHAR
		Tone(440,12)
	ENDIF	
	RETURN SUPER:Dispatch(e)

END CLASS
CLASS oMarginDialog INHERIT DIALOGWINDOW
	PROTECT oDCsleLM AS SINGLELINEEDIT
	PROTECT oDCsleRM AS SINGLELINEEDIT
	PROTECT oCCpbOK AS PUSHBUTTON
	PROTECT oCCpbCancel AS PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  

CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"oMarginDialog" , _GetInst()} , TRUE)

	SELF:oDCsleLM := SINGLELINEEDIT{SELF , ResourceID{ OMARGINDIALOG_SLELM  , _GetInst() } }
	SELF:oDCsleLM:HyperLabel := HyperLabel{#sleLM , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCsleRM := SINGLELINEEDIT{SELF , ResourceID{ OMARGINDIALOG_SLERM  , _GetInst() } }
	SELF:oDCsleRM:HyperLabel := HyperLabel{#sleRM , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCpbOK := PUSHBUTTON{SELF , ResourceID{ OMARGINDIALOG_PBOK  , _GetInst() } }
	SELF:oCCpbOK:HyperLabel := HyperLabel{#pbOK , "OK" , NULL_STRING , NULL_STRING}

	SELF:oCCpbCancel := PUSHBUTTON{SELF , ResourceID{ OMARGINDIALOG_PBCANCEL  , _GetInst() } }
	SELF:oCCpbCancel:HyperLabel := HyperLabel{#pbCancel , "Cancel" , NULL_STRING , NULL_STRING}

	SELF:Caption := "Margins"
	SELF:HyperLabel := HyperLabel{#oMarginDialog , "Margins" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD pbCancel( ) 
	SELF:EndDialog(0)
	

METHOD pbOK( ) 
	SELF:owner:nLeftMargin := Val(oDCSleLM:Value) * 1440
	SELF:owner:nRightMargin := Val(oDCSleRM:Value) * 1440
	SELF:EndDialog(1)
	

METHOD PostInit() 
	
	oDCsleLM:TextValue := "0"	
	oDCsleRM:TextValue := "7"	
	

ACCESS sleLM 
	RETURN(oDCsleLM)
ACCESS sleRM 
	RETURN(oDCsleRM)
END CLASS

STATIC CLASS RichEditExtensions 
    STATIC METHOD RTFChangeFont( SELF oEdit as RichEdit) AS VOID
        LOCAL oFontDlg AS StandardFontDialog
        oFontDlg := StandardFontDialog{oEdit:Owner}
        oFontDlg:FontColor := oEdit:TextColor
        oFontDlg:Font      := oEdit:Font
        oFontDlg:Show()
        oEdit:TextColor := oFontDlg:FontColor
        oEdit:Font      := oFontDlg:Font
  END CLASS

