//#include "VOGUIClasses.vh"
#include "Standard Menus.prg.vh"

CLASS StandardShellWindow INHERIT ShellWindow

	PROTECT oPrinter      AS PrintingDevice
                                         

CONSTRUCTOR( oOwnerApp )

	LOCAL oSB AS StatusBar                 
	                            
  	SUPER( oOwnerApp )
	
	SetDeleted(TRUE)

	oSB := SELF:EnableStatusBar()
	oSB:DisplayTime()

	SELF:Menu := EmptyShellMenu{SELF}

	SELF:Caption := "Standard MDI Application"
	
	oPrinter := PrintingDevice{}
	
	RETURN

METHOD FileOpen()

	LOCAL oOD AS OpenDialog

	(oOD := OpenDialog{SELF, "*.dbf"}):Show()
	
	IF !Empty(oOD:FileName)
		SELF:DoOpenFile(oOD:FileName, oOD:ReadOnly)
	ENDIF

RETURN SELF	

METHOD FileExit()

	SELF:EndWindow()
	
RETURN SELF	

METHOD FilePrinterSetup()

	oPrinter:Setup()
	
RETURN SELF	
	
METHOD DoOpenFile(cFileName, lReadOnly)

	LOCAL oTB AS TextBox
	LOCAL oNewChild AS StdDataWindow

	IF (Len(cFileName) > 3 ) .AND. (Upper(Right(cFileName, 4)) == ".DBF")
		oNewChild := StdDataWindow{SELF, cFileName, lReadOnly}
	  oNewChild:ViewAs(#FormView)
  	oNewChild:Show()
	ELSE
		oTB := TextBox{SELF, "File Open Error", "Cannot open " + cFileName + " - Not a DBF file"}
		oTB:Type := BUTTONOKAY
		oTB:Show()
	ENDIF

RETURN SELF	

METHOD WindowCascade()

	SELF:Arrange(ARRANGECASCADE)

RETURN SELF

METHOD WindowIcon()

	SELF:Arrange(ARRANGEASICONS)

RETURN SELF

METHOD WindowTile()

	SELF:Arrange(ARRANGETILE)
	
RETURN SELF

ACCESS Printer
	
	RETURN oPrinter

METHOD ShowDial() CLIPPER
	LOCAL o AS DIALOGWINDOW1
	o := DIALOGWINDOW1{SELF}
	o:Show()
RETURN NIL

END CLASS











CLASS StdDataWindow INHERIT DataWindow

	
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

RETURN

METHOD FileClose()

	SELF:Server:Close()

 	SELF:EndWindow()

RETURN SELF

METHOD FilePrint()

	SELF:Print(SELF:Owner:Printer)

RETURN SELF	

METHOD ViewForm()

	SELF:ToolBar:UnPressItem(IDM_StandardShellMenu_View_Table_ID)
	SELF:ToolBar:PressItem(IDM_StandardShellMenu_View_Form_ID)
	
RETURN SUPER:ViewForm()	

METHOD ViewTable()

	SELF:ToolBar:UnPressItem(IDM_StandardShellMenu_View_Form_ID)
	SELF:ToolBar:PressItem(IDM_StandardShellMenu_View_Table_ID)
	
RETURN SUPER:ViewTable()


END CLASS	
                                                                    
