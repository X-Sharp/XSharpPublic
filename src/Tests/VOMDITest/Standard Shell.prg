#region DEFINES
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 104
DEFINE IDI_STANDARDICON := 101
DEFINE IDS_ERROR := 65520
DEFINE IDS_EXCHANGE_NOT_INSTALLED := 65521
DEFINE IDS_SAVE := 65522
STATIC DEFINE MAILDLG_PBCANCEL := 101
STATIC DEFINE MAILDLG_PBSEND := 102
STATIC DEFINE MAILDLG_RICHEDIT1 := 100
#endregion

GLOBAL oTest AS ARRAY

CLASS StandardShellWindow INHERIT ShellWindow
	PROTECT oPrinter      AS PrintingDevice


METHOD DoOpenFile(cFileName, lReadOnly)
	LOCAL oTB AS TextBox
	LOCAL oNewChild AS StdDataWindow

	IF (Len(cFileName) > 3 )  .AND. (Upper(Right(cFileName, 4)) == ".DBF")
		oNewChild := StdDataWindow{SELF, cFileName, lReadOnly}
	  oNewChild:ViewAs(#FormView)
  	oNewChild:Show()
	ELSE
		oTB := TextBox{SELF, "File Open Error", "Cannot open " + cFileName + " - Not a DBF file"}
		oTB:Type := BUTTONOKAY
		oTB:Show()
	ENDIF

RETURN SELF



METHOD Drop(oDragEvent)
LOCAL nNumFiles := oDragEvent:FileCount
	LOCAL nFile AS INT

	FOR nFile := 1 TO nNumFiles
		IF File(oDragEvent:FileName(nFile))
			SELF:DoOpenFile(oDragEvent:FileName(nFile))
		ENDIF
	NEXT
    RETURN NIL

METHOD FileExit()

	SELF:EndWindow()

RETURN SELF

METHOD FileOpen()
	LOCAL oOD AS OpenDialog

	(oOD := OpenDialog{SELF, "*.dbf"}):Show()

	IF !Empty(oOD:FileName)
		SELF:DoOpenFile(oOD:FileName, oOD:ReadOnly)
	ENDIF

RETURN SELF

METHOD FilePrinterSetup()

	oPrinter:Setup()

RETURN SELF


METHOD ShowWorkareas() AS VOID
    DbgShowWorkareas()

METHOD ShowSettings() AS VOID
    DbgShowSettings()

METHOD ShowGlobals() AS VOID
    DbgShowGlobals()

METHOD ShowMemvars() AS VOID
    DbgShowMemvars()


CONSTRUCTOR( oOwnerApp )
	local oSB as StatusBar

	SUPER( oOwnerApp )

	SetDeleted(true)

	self:EnableDragDropClient()

	oSB := self:EnableStatusBar()
	oSB:DisplayTime()

	self:Menu := EmptyShellMenu{self}

	self:Icon 	:= Icon{ResourceID{IDI_STANDARDICON, _GetInst()}}
	self:IconSm := Icon{ResourceID{IDI_STANDARDICON, _GetInst()}}

	SELF:Caption := "VOMDITest"

	oPrinter := PrintingDevice{}

	return self


ACCESS Printer

	return oPrinter


METHOD WindowCascade()

	SELF:Arrange(ARRANGECASCADE)

RETURN SELF

METHOD WindowIcon()

	SELF:Arrange(ARRANGEASICONS)

RETURN SELF

METHOD WindowTile()

	SELF:Arrange(ARRANGETILE)

RETURN SELF

END CLASS
CLASS StdDataWindow INHERIT DataWindow


METHOD FileClose()

	SELF:Server:Close()

 	SELF:EndWindow()

RETURN SELF

METHOD FilePrint()

	SELF:Print(SELF:Owner:Printer)

RETURN SELF

CONSTRUCTOR(oParentWindow, sFileName, lReadOnly, oServer)
	LOCAL sCaption AS STRING

	SUPER(oParentWindow)

	SELF:Menu := StandardShellMenu{SELF}
	SELF:ToolBar:PressItem(IDM_StandardShellMenu_View_Form_ID)
	sCaption  := "Browse Database: "

	IF !IsNil(oServer)
		SELF:Use(oServer)
		SELF:Caption := sCaption + oServer:Name
	ELSE
		SELF:Use(CreateInstance(#DBServer, sFileName, , lReadOnly))
		SELF:Caption := sCaption + sFileName
	ENDIF

RETURN SELF

method ViewForm()
	self:ToolBar:UnPressItem(IDM_StandardShellMenu_View_Table_ID)
	self:ToolBar:PressItem(IDM_StandardShellMenu_View_Form_ID)

	return super:ViewForm()




method ViewTable()
	self:ToolBar:UnPressItem(IDM_StandardShellMenu_View_Form_ID)
	self:ToolBar:PressItem(IDM_StandardShellMenu_View_Table_ID)

	return super:ViewTable()



END CLASS
