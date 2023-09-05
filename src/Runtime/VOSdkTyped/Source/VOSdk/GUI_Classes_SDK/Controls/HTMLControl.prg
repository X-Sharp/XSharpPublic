//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

CLASS HTMLControl INHERIT TextControl

	PROTECT lNoNotify AS LOGIC
	PROTECT lForceModFlag2True AS LOGIC
	PROTECT _lModified AS LOGIC
	PROTECT _oWebBrowser AS VOHtmlEditorControl
	PROTECT _oVScrollBar AS System.Windows.Forms.VScrollBar


METHOD __SetText(cNewText AS STRING) AS STRING STRICT
		SELF:_oWebBrowser:DocumentText := cNewText
		RETURN cNewText

METHOD __GetText() AS STRING STRICT
	RETURN SELF:_oWebBrowser:DocumentText


PROPERTY ControlType AS ControlType GET ControlType.Panel

METHOD OnControlCreated(oC AS IVOControl) AS VOID
	SELF:_oWebBrowser := VOHtmlEditorControl{}
	SELF:_oWebBrowser:Parent := (System.Windows.Forms.Control) oC
	SELF:_oWebBrowser:Dock := System.Windows.Forms.DockStyle.Fill
	SELF:_oVScrollBar				:= System.Windows.Forms.VScrollBar{}
	SELF:_oVScrollBar:Parent		:= (System.Windows.Forms.Control) oC
	SELF:_oVScrollBar:Visible		:= TRUE
	SELF:_oVScrollBar:Dock		:= System.Windows.Forms.DockStyle.Right
	SELF:_oVScrollBar:BackColor	:= System.Drawing.Color.Yellow
	SELF:_oVScrollBar:SmallChange := 1
	SELF:_oVScrollBar:LargeChange := 5
	SELF:_oVScrollBar:Scroll      += OnVScrolled
	SELF:_oWebBrowser:DocumentCompleted += setDefaultFont
	SELF:__SetText(" ")
	RETURN


PROTECTED METHOD setDefaultFont(sender AS OBJECT, e AS System.Windows.Forms.WebBrowserDocumentCompletedEventArgs) AS VOID
	IF SELF:_oWebBrowser:Document != NULL .AND. SELF:_oWebBrowser:Document:Body != NULL
		SELF:_oWebBrowser:Document:Body:Style := "font-family:'Arial';font-size:8pt;padding:3px;margin:0px;"
	ENDIF

PROTECTED METHOD OnVScrolled(sender AS OBJECT, se AS System.Windows.Forms.ScrollEventArgs ) AS VOID
	SELF:_oWebBrowser:Document:body:ScrollTop := SELF:_oVScrollBar:Value * 10

ASSIGN __ForceModFlag2True(lNewValue AS LOGIC)  STRICT
	lForceModFlag2True := lNewValue

ACCESS __NoNotify AS LOGIC STRICT
	RETURN lNoNotify

METHOD CanUndo()
	RETURN TRUE

ACCESS Caption AS STRING
	RETURN cCaption

ASSIGN Caption(cNewCaption AS STRING)
	cCaption := cNewCaption

METHOD Clear()   AS VOID STRICT
	RETURN

METHOD Copy()   AS VOID STRICT
	RETURN

METHOD Cut()   AS VOID STRICT
	RETURN

METHOD Font(oNewFont, lRescal)
	LOCAL uRet AS USUAL
	LOCAL oMargins AS Dimension

	IF SELF:ValidateControl()
		oMargins := SELF:Margins
		uRet := SUPER:Font(oNewFont, lRescal)
		SELF:Margins := oMargins
	ENDIF

	RETURN uRet

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
	LOCAL dwStyle AS DWORD

	IF !(xID IS ResourceID)
		dwStyle:= _OR(WS_CHILD, WS_CLIPSIBLINGS)
		IF !IsNil(kStyle)
			dwStyle := _OR(DWORD(kStyle), dwStyle)
		ENDIF
		SUPER(oOwner, xID, oPoint, oDimension, "Edit", dwStyle, TRUE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, TRUE)
	ENDIF
	IF !(xID IS ResourceID)
		SELF:SetStyle(ES_MultiLine)
	ENDIF
	RETURN

METHOD IsPassword()
	RETURN FALSE

ACCESS Margins  AS Dimension
	IF SELF:ValidateControl()
        RETURN (Dimension) _oWebBrowser:Margin
	ENDIF
	RETURN Dimension{}

ASSIGN Margins(oNewMargins AS Dimension)
		IF SELF:ValidateControl()
			LOCAL oPadding AS System.Windows.Forms.Padding
			oPadding := _oWebBrowser:Margin
			IF oPadding:Left != oNewMargins:Width .or. oPadding:Right != oNewMargins:Height
				oPadding:Left := oNewMargins:Width
				oPadding:Right := oNewMargins:Height
				_oWebBrowser:Margin := oPadding
			ENDIF
		ENDIF

	RETURN

METHOD Paste(cNewString as string)   AS VOID STRICT
	RETURN


ACCESS ReadOnly  AS LOGIC
	RETURN TRUE

ASSIGN ReadOnly(lNewValue AS LOGIC)
	RETURN

ACCESS SelectedText AS STRING
	RETURN ""

ASSIGN SelectedText(cNewString AS STRING)
	RETURN

ACCESS Selection AS Selection
	RETURN Selection{0,0}

ASSIGN Selection(oSel AS Selection)
	RETURN

METHOD SelectAll()
	RETURN NIL

METHOD SelectNone()
	RETURN NIL

METHOD SetSelectionFocus()
	RETURN NIL

ACCESS TextLimit AS LONG
	RETURN 0

ASSIGN TextLimit(nChars AS LONG)
	RETURN

METHOD Undo() AS VOID
	RETURN

ACCESS TextValue AS STRING
	RETURN SELF:_oWebBrowser:DocumentText;


ASSIGN TextValue(cNewText AS STRING)
	LOCAL cOldValue AS STRING
	IF !SELF:_oWebBrowser:isDisposed
		cOldValue := AsString(uValue)
		IF SELF:FieldSpec != NULL
			uValue := SELF:FieldSpec:Val(cNewText)
		ELSE
			uValue := cNewText
		ENDIF
		SELF:_oWebBrowser:DocumentText := cNewText
		SELF:ValueChanged := !(cOldValue == AsString(uValue))
	ENDIF
	RETURN

ACCESS Modified AS LOGIC
	IF SELF:ValidateControl()
		IF lForceModFlag2True
			RETURN TRUE
		ELSE
			RETURN SELF:_lModified
		ENDIF
	ENDIF
	RETURN FALSE

ASSIGN Modified(lModified  AS LOGIC)
	IF SELF:ValidateControl()
		SELF:_lModified := lModified
	ENDIF
	RETURN

END CLASS

