#include "Standard Menus.prg.vh"
//#include "VOGUIClasses.vh"

CLASS EmptyShellMenu INHERIT Menu

	// User code starts here (DO NOT remove this line)  ##USER##
	CONSTRUCTOR( oOwner )

		LOCAL oTB AS Toolbar

		SELF:PreInit()

		SUPER( ResourceID { "EmptyShellMenu" , _GetInst( ) } )

		SELF:RegisterItem(IDM_EmptyShellMenu_File_ID, ;
		HyperLabel{ #File , "&File" , NULL_STRING , "File" } , SELF:Handle() , 0)

		SELF:RegisterItem(IDM_EmptyShellMenu_File_Open_ID, ;
		HyperLabel{ #FileOpen , "&Open...	Ctrl+O" , "Open a file" , "File_Open" })

		SELF:RegisterItem(IDM_EmptyShellMenu_File_Print_Setup_ID, ;
		HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

		SELF:RegisterItem(IDM_EmptyShellMenu_File_Exit_ID, ;
		HyperLabel{ #FileExit , "E&xit	Alt+F4" , "End of application" , "File_Exit" })

		SELF:RegisterItem(IDM_EmptyShellMenu_Help_ID, ;
		HyperLabel{ #Help , "&Help" , NULL_STRING , NULL_STRING } , SELF:Handle() , 1)

		SELF:RegisterItem(IDM_EmptyShellMenu_Help_Index_ID, ;
		HyperLabel{ #HelpIndex , "&Index	F1" , "Index of help" , "Help_Index" })

		SELF:RegisterItem(IDM_EmptyShellMenu_Help_Using_Help_ID, ;
		HyperLabel{ #HelpUsingHelp , "&Using Help" , "How to use help" , "Help_UsingHelp" })

		SELF:RegisterItem(IDM_EmptyShellMenu_Help_About_ID, ;
		HyperLabel{ #HelpAbout , "&About..." , "About application" , NULL_STRING })

		SELF:RegisterItem(IDM_EmptyShellMenu_Help_dialogwindow1_ID, ;
		HyperLabel{ #showdial , "dialogwindow1" , NULL_STRING , NULL_STRING })

		oTB := Toolbar{}

		oTB:ButtonStyle := TB_ICONONLY
		oTB:Flat := TRUE
		oTB:EnableBands(FALSE)

		oTB:AppendItem(IDT_OPEN , IDM_EmptyShellMenu_File_Open_ID)
		oTB:AddTipText(IDT_OPEN , IDM_EmptyShellMenu_File_Open_ID , "Open File")

		oTB:AppendItem(IDT_HELP , IDM_EmptyShellMenu_Help_About_ID)
		oTB:AddTipText(IDT_HELP , IDM_EmptyShellMenu_Help_About_ID , "Help")


		SELF:ToolBar := oTB
		SELF:Accelerator := EmptyShellMenu_Accelerator{ }

		SELF:PostInit()

	RETURN

END CLASS

CLASS EmptyShellMenu_Accelerator INHERIT Accelerator

	// User code starts here (DO NOT remove this line)  ##USER##
	CONSTRUCTOR()
		SUPER( ResourceID { "EmptyShellMenu_Accelerator" , _GetInst( ) } )
	RETURN


END CLASS



CLASS StandardShellMenu INHERIT Menu

// User code starts here (DO NOT remove this line)  ##USER##
CONSTRUCTOR( oOwner )

	LOCAL oTB AS Toolbar

	SELF:PreInit()

	SUPER( ResourceID { "StandardShellMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_StandardShellMenu_File_ID, ;
		HyperLabel{ #File , "&File" , NULL_STRING , "File" } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_StandardShellMenu_File_Open_ID, ;
		HyperLabel{ #FileOpen , "&Open...	Ctrl+O" , "Open a file" , "File_Open" })

	SELF:RegisterItem(IDM_StandardShellMenu_File_Close_ID, ;
		HyperLabel{ #FileClose , "&Close" , "Close current child window" , "File_Close" })

	SELF:RegisterItem(IDM_StandardShellMenu_File_Print_ID, ;
		HyperLabel{ #FilePrint , "&Print" , "Print the active window" , NULL_STRING })

	SELF:RegisterItem(IDM_StandardShellMenu_File_Print_Setup_ID, ;
		HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

	SELF:RegisterItem(IDM_StandardShellMenu_File_Exit_ID, ;
		HyperLabel{ #FileExit , "E&xit	Alt+F4" , "End of application" , "File_Exit" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_ID, ;
		HyperLabel{ #Edit , "&Edit" , "Edit information" , "Edit" } , SELF:Handle() , 1)

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Cut_ID, ;
		HyperLabel{ #Cut , "Cu&t	Ctrl+X" , "Cut to clipboard" , "Cut" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Copy_ID, ;
		HyperLabel{ #Copy , "&Copy	Ctrl+C" , "Copy to clipboard" , "Copy" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Paste_ID, ;
		HyperLabel{ #Paste , "&Paste	Ctrl+V" , "Paste from clipboard" , "Paste" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Insert_Record_ID, ;
		HyperLabel{ #Append , "&Insert Record" , "Insert a new record" , "Append" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Delete_Record_ID, ;
		HyperLabel{ #Delete , "&Delete Record" , "Delete this record" , "Delete" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Go_To_Top_ID, ;
		HyperLabel{ #GoTop , "G&o To Top	Ctrl+Home" , "Go to first record" , "GoTop" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Previous_ID, ;
		HyperLabel{ #SkipPrevious , "Pre&vious" , "Go to previous record" , "SkipPrevious" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Next_ID, ;
		HyperLabel{ #SkipNext , "&Next" , "Go to next record" , "SkipNext" })

	SELF:RegisterItem(IDM_StandardShellMenu_Edit_Go_To_Bottom_ID, ;
		HyperLabel{ #GoBottom , "Go To &Bottom	Ctrl+End" , "Go to last record" , "GoBottom" })

	SELF:RegisterItem(IDM_StandardShellMenu_View_ID, ;
		HyperLabel{ #View , "&View" , NULL_STRING , NULL_STRING } , SELF:Handle() , 2)

	SELF:RegisterItem(IDM_StandardShellMenu_View_Form_ID, ;
		HyperLabel{ #ViewForm , "&Form	Shift+F2" , "View a single record as a form" , "ViewForm" })

	SELF:RegisterItem(IDM_StandardShellMenu_View_Table_ID, ;
		HyperLabel{ #ViewTable , "&Table	F2" , "View several records in a table" , "ViewTable" })

	SELF:RegisterItem(IDM_StandardShellMenu_Window_ID, ;
		HyperLabel{ #Window , "&Window" , "Arrange child windows" , NULL_STRING } , SELF:Handle() , 3)

	SELF:RegisterItem(IDM_StandardShellMenu_Window_Cascade_ID, ;
		HyperLabel{ #WindowCascade , "&Cascade" , "Arrange child windows in a cascade" , "WindowCascade" })

	SELF:RegisterItem(IDM_StandardShellMenu_Window_Tile_ID, ;
		HyperLabel{ #WindowTile , "&Tile" , "Arrange child windows tiled" , "WindowTile" })

	SELF:RegisterItem(IDM_StandardShellMenu_Window_Close_All_ID, ;
		HyperLabel{ #CloseAllChildren , "Close A&ll" , "Close all child windows" , "WindowCloseAll" })

	SELF:RegisterItem(IDM_StandardShellMenu_Help_ID, ;
		HyperLabel{ #Help , "&Help" , NULL_STRING , NULL_STRING } , SELF:Handle() , 4)

	SELF:RegisterItem(IDM_StandardShellMenu_Help_Index_ID, ;
		HyperLabel{ #HelpIndex , "&Index	F1" , "Index of help" , "Help_Index" })

	SELF:RegisterItem(IDM_StandardShellMenu_Help_Context_Help_ID, ;
		HyperLabel{ #HelpContext , "&Context Help	Ctrl+F1" , "Context sensitive help" , "Help_ContextHelp" })

	SELF:RegisterItem(IDM_StandardShellMenu_Help_Using_Help_ID, ;
		HyperLabel{ #HelpUsingHelp , "&Using Help" , "How to use help" , "Help_UsingHelp" })

	SELF:RegisterItem(IDM_StandardShellMenu_Help_About_ID, ;
		HyperLabel{ #HelpAbout , "&About..." , "About application" , NULL_STRING })

	SELF:SetAutoUpdate( 3 )

	oTB := Toolbar{}

	oTB:ButtonStyle := TB_ICONONLY
	oTB:Flat := TRUE
	oTB:EnableBands(FALSE)

	oTB:AppendItem(IDT_CLOSE , IDM_StandardShellMenu_File_Close_ID)
	oTB:AddTipText(IDT_CLOSE , IDM_StandardShellMenu_File_Close_ID , "Close File")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT , IDM_StandardShellMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT , IDM_StandardShellMenu_Edit_Cut_ID , "Cut")

	oTB:AppendItem(IDT_COPY , IDM_StandardShellMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY , IDM_StandardShellMenu_Edit_Copy_ID , "Copy")

	oTB:AppendItem(IDT_PASTE , IDM_StandardShellMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE , IDM_StandardShellMenu_Edit_Paste_ID , "Paste")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_STARTREC , IDM_StandardShellMenu_Edit_Go_To_Top_ID)
	oTB:AddTipText(IDT_STARTREC , IDM_StandardShellMenu_Edit_Go_To_Top_ID , "Go Top")

	oTB:AppendItem(IDT_PREVREC , IDM_StandardShellMenu_Edit_Previous_ID)
	oTB:AddTipText(IDT_PREVREC , IDM_StandardShellMenu_Edit_Previous_ID , "Previous Record")

	oTB:AppendItem(IDT_NEXTREC , IDM_StandardShellMenu_Edit_Next_ID)
	oTB:AddTipText(IDT_NEXTREC , IDM_StandardShellMenu_Edit_Next_ID , "Next Record")

	oTB:AppendItem(IDT_ENDREC , IDM_StandardShellMenu_Edit_Go_To_Bottom_ID)
	oTB:AddTipText(IDT_ENDREC , IDM_StandardShellMenu_Edit_Go_To_Bottom_ID , "Go Bottom")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_VFORM , IDM_StandardShellMenu_View_Form_ID)
	oTB:AddTipText(IDT_VFORM , IDM_StandardShellMenu_View_Form_ID , "Form View")

	oTB:AppendItem(IDT_VGBROWSE , IDM_StandardShellMenu_View_Table_ID)
	oTB:AddTipText(IDT_VGBROWSE , IDM_StandardShellMenu_View_Table_ID , "Browse View")


	SELF:ToolBar := oTB
	SELF:Accelerator := StandardShellMenu_Accelerator{ }

	SELF:PostInit()

	RETURN
END CLASS

CLASS StandardShellMenu_Accelerator INHERIT Accelerator

// User code starts here (DO NOT remove this line)  ##USER##
CONSTRUCTOR()
	SUPER( ResourceID { "StandardShellMenu_Accelerator" , _GetInst( ) } )
RETURN

END CLASS

