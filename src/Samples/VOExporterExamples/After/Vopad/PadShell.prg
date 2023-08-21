#region DEFINES
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 104
DEFINE IDI_VOPADICON := 101
#endregion

CLASS PadShellWindow INHERIT ShellWindow
	EXPORT aChildWindows AS ARRAY
	EXPORT cFName AS STRING	
	EXPORT oCB AS SearchBox

METHOD AddChild(oNewChild) 
	IF aChildWindows = NULL_ARRAY
		aChildWindows := {oNewChild}
	  ELSE
		AAdd(aChildWindows, oNewChild)
	ENDIF
	

METHOD DoOpenFile(cFileName, lReadOnly) 
	LOCAL oTB AS TextBox

	IF (Len(cFileName) > 3 ) .AND. ((Upper(Right(cFileName, 4)) == ".RTF") .OR. (Upper(Right(cFileName, 4)) == ".TXT"))
		SELF:NewEditWindow(cFileName, lReadOnly)
	ELSE
		oTB := TextBox{	SELF, ;
						"File Open Error", ;
					   	"Cannot open " + cFileName + " - Not a RTF or TXT  file"}
		oTB:Type := BUTTONOKAY
		oTB:Show()
	ENDIF
	


METHOD Drop(oDragEvent) 
	LOCAL nNumFiles := oDragEvent:FileCount
	LOCAL nFile AS INT
	
	FOR nFile := 1 TO nNumFiles
		IF File(oDragEvent:FileName(nFile))
			SELF:DoOpenFile(oDragEvent:FileName(nFile))
		ENDIF
	NEXT


METHOD FileExit() 

	SELF:EndWindow()


METHOD FileNew() 
	STATIC nCount := 1 AS SHORT
	SELF:NewEditWindow("Untitled" + Trim(Str(nCount,2)))
	nCount++
	

method FileOpen() 
	local oOD as OpenDialog

	oOD := OpenDialog{self, ".\*.RTF"}
	
	oOD:SetFilter({"*.RTF", "*.TXT"}, {"Rich Edit Text", "TXT"}, 1)	
	oOD:Show()	
	if !Empty(oOD:FileName)
		self:DoOpenFile(oOD:FileName, oOD:ReadOnly)
	endif


METHOD FilePrinterSetup() 

	LOCAL oPrinter AS PrintingDevice
	
	oPrinter := PrintingDevice{}
	oPrinter:Setup()


CONSTRUCTOR( oOwnerApp ) 
	LOCAL oSB AS StatusBar
	LOCAL oTB AS ToolBar
	
	SUPER( oOwnerApp )
	
   SELF:EnableDragDropClient()

	oSB := SELF:EnableStatusBar()
	oSB:DisplayTime()

	SELF:Icon 	:= Icon{ResourceID{IDI_VOPADICON, _GetInst()}}
	SELF:IconSm := Icon{ResourceID{IDI_VOPADICON, _GetInst()}}
	
	SELF:Caption := "VOPad"


	SELF:Menu := StandardPadMenu{SELF}
	
	oTB := SELF:ToolBar	
	
 	oCB := SearchBox{SELF:ToolBar, 4711, Point{0, 0}, Dimension{50,152}, BOXDROPDOWN	}
 	oTB:AddBand(#FFIND, oCB, -1, 150, 25,"Search")
   oTB:AppendSubItem(#FFIND, IDT_FIND,IDM_StandardPadMenu_Edit_Find_ID)					
	
	oTB:AddSubToolBarBand(#Find, , 50)
	oTB:AppendSubItem(#Find,IDT_FIND,IDM_StandardPadMenu_Edit_Find_ID)
	oTB:AddTipText(IDT_FIND,IDM_StandardPadMenu_Edit_Find_ID,"Find")
	oTB:AppendSubItem(#Find,IDT_NEXTREC,IDM_StandardPadMenu_Edit_Find_Next_ID)
	oTB:AddTipText(IDT_NEXTREC,IDM_StandardPadMenu_Edit_Find_Next_ID,"Find Next")

	RETURN SELF


METHOD NewEditWindow(cFileName,lReadOnly) 
	LOCAL oNewChild AS PadWin
 	oNewChild := PadWin{SELF, TRUE, TRUE, cFileName, lReadOnly}
	oNewChild:Show(SHOWZOOMED)
	SELF:AddChild(oNewChild)
	oNewChild:cFName := cFileName
	oNewChild:Caption := cFileName	
	IF Left(cFileName, 8) != "Untitled"
		IF Upper(Right(cFileName, 3)) = "RTF"
			oNewChild:oDCRichEdit:LoadFromFile(cFileName)
		 ELSE
			oNewChild:oDCRichEdit:TextValue	:= MemoRead(cFileName)
		ENDIF	
	ENDIF	
	oNewChild:lChange := FALSE
	SELF:omenu:DisableItem(IDM_StandardPadMenu_File_Save_ID)


METHOD RemoveChild(oChild) 
	LOCAL nALen, n AS DWORD
	nAlen := ALen(SELF:aChildWindows)
	FOR n := 1 TO nAlen
		IF SELF:aChildWindows[n] == oChild
			ADel(SELF:aChildWindows, n)    
			ASize(Self:aChildWindows, ALen(Self:aChildWindows)-1)
		ENDIF
	NEXT	
		

METHOD WindowCascade() 

	SELF:Arrange(ARRANGECASCADE)
	

METHOD WindowIcon() 

	SELF:Arrange(ARRANGEASICONS)


METHOD WindowTile() 

	SELF:Arrange(ARRANGETILE)


	


END CLASS
