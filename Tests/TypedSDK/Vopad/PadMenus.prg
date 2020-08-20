#region DEFINES
DEFINE IDM_StandardPadMenu_File_ID := 12000
DEFINE IDM_StandardPadMenu_File_New_ID := 12001
DEFINE IDM_StandardPadMenu_File_Open_ID := 12002
DEFINE IDM_StandardPadMenu_File_Save_ID := 12003
DEFINE IDM_StandardPadMenu_File_Print_ID := 12005
DEFINE IDM_StandardPadMenu_File_Print_Setup_ID := 12006
DEFINE IDM_StandardPadMenu_File_Page_Setup_ID := 12007
DEFINE IDM_StandardPadMenu_File_Exit_ID := 12009
DEFINE IDM_StandardPadMenu_Edit_ID := 12010
DEFINE IDM_StandardPadMenu_Edit_Font_ID := 12011
DEFINE IDM_StandardPadMenu_Edit_Margins_ID := 12013
DEFINE IDM_StandardPadMenu_Edit_AlignMent_ID := 12014
DEFINE IDM_StandardPadMenu_Edit_AlignMent_Left_ID := 12015
DEFINE IDM_StandardPadMenu_Edit_AlignMent_Center_ID := 12016
DEFINE IDM_StandardPadMenu_Edit_AlignMent_Right_ID := 12017
DEFINE IDM_StandardPadMenu_Edit_Cut_ID := 12019
DEFINE IDM_StandardPadMenu_Edit_Copy_ID := 12020
DEFINE IDM_StandardPadMenu_Edit_Paste_ID := 12021
DEFINE IDM_StandardPadMenu_Edit_Find_ID := 12023
DEFINE IDM_StandardPadMenu_Edit_Find_Next_ID := 12024
DEFINE IDM_StandardPadMenu_Window_ID := 12025
DEFINE IDM_StandardPadMenu_Window_Cascade_ID := 12026
DEFINE IDM_StandardPadMenu_Window_Tile_ID := 12027
DEFINE IDM_StandardPadMenu_Window_Close_ID := 12029
DEFINE IDM_StandardPadMenu_Window_Close_All_ID := 12030
DEFINE IDM_StandardPadMenu_Help_ID := 12031
DEFINE IDM_StandardPadMenu_Help_Index_ID := 12032
DEFINE IDM_StandardPadMenu_Help_About_ID := 12034
DEFINE IDA_EmptyPadMenu := "EmptyPadMenu"
DEFINE IDA_StandardPadMenu := "StandardPadMenu"
DEFINE IDM_EmptyPadMenu := "EmptyPadMenu"
DEFINE IDM_EmptyPadMenu_File_Exit_ID := 21011
DEFINE IDM_EmptyPadMenu_File_ID := 21007
DEFINE IDM_EmptyPadMenu_File_New_ID := 21008
DEFINE IDM_EmptyPadMenu_File_Open_ID := 21009
DEFINE IDM_EmptyPadMenu_Help_About_ID := 21015
DEFINE IDM_EmptyPadMenu_Help_ID := 21012
DEFINE IDM_EmptyPadMenu_Help_Index_ID := 21013
DEFINE IDM_RtfMenu := "PadMenu"
DEFINE IDM_StandardPadMenu := "StandardPadMenu"
#endregion

PARTIAL CLASS StandardPadMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	LOCAL oTB AS Toolbar

	SELF:PreInit()

	SUPER( ResourceID { "StandardPadMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_StandardPadMenu_File_ID, ;
		HyperLabel{ #File , "&File" ,  , "File" } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_StandardPadMenu_File_New_ID, ;
		HyperLabel{ #FileNew , "&New" , "Create a new file" , "File_New" })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Open_ID, ;
		HyperLabel{ #FileOpen , "&Open..." , "Open a file" , "File_Open" })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Save_ID, ;
		HyperLabel{ #RtfSave , "&Save" , "Save a file" , "File_Save" })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Print_ID, ;
		HyperLabel{ #FilePrint , "&Print..." , "Print a file" , "File_Print" })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Print_Setup_ID, ;
		HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Page_Setup_ID, ;
		HyperLabel{ #RTFPageSetup , "Page Set&up" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_File_Exit_ID, ;
		HyperLabel{ #FileExit , "E&xit	Alt+F4" , "End of Application" , "File_Exit" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_ID, ;
		HyperLabel{ #Edit , "&Edit" , "Edit information" , "Edit" } , SELF:Handle() , 1)

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Font_ID, ;
		HyperLabel{ #SetFont , "F&ont" , "Open Font Dialog" , "Undo" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Margins_ID, ;
		HyperLabel{ #SetMargins , "&Margins" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_AlignMent , "&AlignMent" ,  ,  } , GetSubMenu( SELF:Handle() , 1 ) , 3)

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Left_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_AlignMent_Left , "Left" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Center_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_AlignMent_Center , "Center" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Right_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_AlignMent_Right , "Right" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Cut_ID, ;
		HyperLabel{ #Cut , "Cu&t	Ctrl+X" , "Cut to Clipboard" , "Cut" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Copy_ID, ;
		HyperLabel{ #Copy , "&Copy	Ctrl+C" , "Copy to Clipboard" , "Copy" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Paste_ID, ;
		HyperLabel{ #Paste , "&Paste	Ctrl+V" , "Paste from Clipboard" , "Paste" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Find_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_Find , "&Find	Alt+F3" , "Find a specific expression" , "Find" })

	SELF:RegisterItem(IDM_StandardPadMenu_Edit_Find_Next_ID, ;
		HyperLabel{ #StandardPadMenu_Edit_Find_Next , "Find Ne&xt	F3" , "Find next matching expression" , "FindNext" })

	SELF:RegisterItem(IDM_StandardPadMenu_Window_ID, ;
		HyperLabel{ #Window , "&Window" , "Arrange child windows" ,  } , SELF:Handle() , 2)

	SELF:RegisterItem(IDM_StandardPadMenu_Window_Cascade_ID, ;
		HyperLabel{ #WindowCascade , "&Cascade" , "Arrange child windows in a cascade" , "WindowCascade" })

	SELF:RegisterItem(IDM_StandardPadMenu_Window_Tile_ID, ;
		HyperLabel{ #WindowTile , "&Tile" , "Arrange child windows tiled" , "WindowTile" })

	SELF:RegisterItem(IDM_StandardPadMenu_Window_Close_ID, ;
		HyperLabel{ #Close , "C&lose" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Window_Close_All_ID, ;
		HyperLabel{ #CloseAll , "Close &All" ,  ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Help_ID, ;
		HyperLabel{ #Help , "&Help" ,  ,  } , SELF:Handle() , 3)

	SELF:RegisterItem(IDM_StandardPadMenu_Help_Index_ID, ;
		HyperLabel{ #HelpIndex , "&Index" , "Index of help" ,  })

	SELF:RegisterItem(IDM_StandardPadMenu_Help_About_ID, ;
		HyperLabel{ #HelpAbout , "&About..." , "About VOPad" ,  })

	SELF:SetAutoUpdate( 2 )

	oTB := Toolbar{}

	oTB:ButtonStyle := TB_ICONONLY
	oTB:Flat := TRUE
	oTB:EnableBands(FALSE)

	oTB:AppendItem(IDT_NEWSHEET , IDM_StandardPadMenu_File_New_ID)
	oTB:AddTipText(IDT_NEWSHEET , IDM_StandardPadMenu_File_New_ID , "Edit new file")

	oTB:AppendItem(IDT_OPEN , IDM_StandardPadMenu_File_Open_ID)
	oTB:AddTipText(IDT_OPEN , IDM_StandardPadMenu_File_Open_ID , "Open  a file")

	oTB:AppendItem(IDT_SAVE , IDM_StandardPadMenu_File_Save_ID)
	oTB:AddTipText(IDT_SAVE , IDM_StandardPadMenu_File_Save_ID , "Save this file")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_PRINT , IDM_StandardPadMenu_File_Print_ID)
	oTB:AddTipText(IDT_PRINT , IDM_StandardPadMenu_File_Print_ID , "Print")

	oTB:AppendItem(IDT_SIZEPLUS , IDM_StandardPadMenu_Edit_Font_ID)
	oTB:AddTipText(IDT_SIZEPLUS , IDM_StandardPadMenu_Edit_Font_ID , "Font")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT , IDM_StandardPadMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT , IDM_StandardPadMenu_Edit_Cut_ID , "Cut")

	oTB:AppendItem(IDT_COPY , IDM_StandardPadMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY , IDM_StandardPadMenu_Edit_Copy_ID , "Copy")

	oTB:AppendItem(IDT_PASTE , IDM_StandardPadMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE , IDM_StandardPadMenu_Edit_Paste_ID , "Paste")

	oTB:AppendItem(IDT_LEFT , IDM_StandardPadMenu_Edit_AlignMent_Left_ID)
	oTB:AddTipText(IDT_LEFT , IDM_StandardPadMenu_Edit_AlignMent_Left_ID , "Left")

	oTB:AppendItem(IDT_CENTER , IDM_StandardPadMenu_Edit_AlignMent_Center_ID)
	oTB:AddTipText(IDT_CENTER , IDM_StandardPadMenu_Edit_AlignMent_Center_ID , "Center")

	oTB:AppendItem(IDT_JUSTIFY , IDM_StandardPadMenu_Edit_AlignMent_Right_ID)
	oTB:AddTipText(IDT_JUSTIFY , IDM_StandardPadMenu_Edit_AlignMent_Right_ID , "Right")

	oTB:AppendItem(IDT_HELP , IDM_StandardPadMenu_Help_About_ID)
	oTB:AddTipText(IDT_HELP , IDM_StandardPadMenu_Help_About_ID , "About")


	SELF:ToolBar := oTB
	SELF:Accelerator := StandardPadMenu_Accelerator{ }

	SELF:DisableItem(IDM_StandardPadMenu_Edit_AlignMent_ID)
	SELF:PostInit()

	RETURN

END CLASS
PARTIAL CLASS StandardPadMenu_Accelerator INHERIT Accelerator

CONSTRUCTOR()
	SUPER( ResourceID { "StandardPadMenu_Accelerator" , _GetInst( ) } )
RETURN


END CLASS
