#region DEFINES
Define IDA_EmptyPadMenu := "EmptyPadMenu"
Define IDA_StandardPadMenu := "StandardPadMenu"
Define IDM_EmptyPadMenu := "EmptyPadMenu"
Define IDM_EmptyPadMenu_File_Exit_ID := 21011
Define IDM_EmptyPadMenu_File_ID := 21007
Define IDM_EmptyPadMenu_File_New_ID := 21008
Define IDM_EmptyPadMenu_File_Open_ID := 21009
Define IDM_EmptyPadMenu_Help_About_ID := 21015
Define IDM_EmptyPadMenu_Help_ID := 21012
Define IDM_EmptyPadMenu_Help_Index_ID := 21013
DEFINE IDM_RtfMenu := "PadMenu"
Define IDM_StandardPadMenu := "StandardPadMenu"
Define IDM_StandardPadMenu_Edit_AlignMent_Center_ID := 22659
Define IDM_StandardPadMenu_Edit_AlignMent_ID := 22657
Define IDM_StandardPadMenu_Edit_AlignMent_Left_ID := 22658
Define IDM_StandardPadMenu_Edit_AlignMent_Right_ID := 22660
Define IDM_StandardPadMenu_Edit_Copy_ID := 22663
Define IDM_StandardPadMenu_Edit_Cut_ID := 22662
Define IDM_StandardPadMenu_Edit_Find_ID := 22666
Define IDM_StandardPadMenu_Edit_Find_Next_ID := 22667
Define IDM_StandardPadMenu_Edit_Font_ID := 22654
Define IDM_StandardPadMenu_Edit_ID := 22653
Define IDM_StandardPadMenu_Edit_Margins_ID := 22656
Define IDM_StandardPadMenu_Edit_Paste_ID := 22664
Define IDM_StandardPadMenu_File_Exit_ID := 22652
Define IDM_StandardPadMenu_File_ID := 22643
Define IDM_StandardPadMenu_File_New_ID := 22644
Define IDM_StandardPadMenu_File_Open_ID := 22645
Define IDM_StandardPadMenu_File_Page_Setup_ID := 22650
Define IDM_StandardPadMenu_File_Print_ID := 22648
Define IDM_StandardPadMenu_File_Print_Setup_ID := 22649
Define IDM_StandardPadMenu_File_Save_ID := 22646
Define IDM_StandardPadMenu_Help_About_ID := 22677
Define IDM_StandardPadMenu_Help_ID := 22674
Define IDM_StandardPadMenu_Help_Index_ID := 22675
Define IDM_StandardPadMenu_Window_Cascade_ID := 22669
Define IDM_StandardPadMenu_Window_Close_All_ID := 22673
Define IDM_StandardPadMenu_Window_Close_ID := 22672
Define IDM_StandardPadMenu_Window_ID := 22668
Define IDM_StandardPadMenu_Window_Tile_ID := 22670
#endregion

CLASS StandardPadMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_StandardPadMenu, _GetInst( )})

	self:RegisterItem(IDM_StandardPadMenu_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_StandardPadMenu_File_New_ID,	;
		HyperLabel{#FileNew,	;
			"&New",	;
			"Create a new file",	;
			"File_New"})
	self:RegisterItem(IDM_StandardPadMenu_File_Open_ID,	;
		HyperLabel{#FileOpen,	;
			"&Open...",	;
			"Open a file",	;
			"File_Open"})
	self:RegisterItem(IDM_StandardPadMenu_File_Save_ID,	;
		HyperLabel{#RtfSave,	;
			"&Save",	;
			"Save a file",	;
			"File_Save"})
	self:RegisterItem(IDM_StandardPadMenu_File_Print_ID,	;
		HyperLabel{#FilePrint,	;
			"&Print...",	;
			"Print a file",	;
			"File_Print"})
	self:RegisterItem(IDM_StandardPadMenu_File_Print_Setup_ID,	;
		HyperLabel{#FilePrinterSetup,	;
			"P&rint Setup...",	;
			"Setup printer options",	;
			"File_Printer_Setup"})
	self:RegisterItem(IDM_StandardPadMenu_File_Page_Setup_ID,	;
		HyperLabel{#RTFPageSetup,	;
			"Page Set&up",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_File_Exit_ID,	;
		HyperLabel{#FileExit,	;
			"E&xit	Alt+F4",	;
			"End of Application",	;
			"File_Exit"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_ID,	;
		HyperLabel{#Edit,	;
			"&Edit",	;
			"Edit information",	;
			"Edit"},self:Handle( ),1)
	self:RegisterItem(IDM_StandardPadMenu_Edit_Font_ID,	;
		HyperLabel{#SetFont,	;
			"F&ont",	;
			"Open Font Dialog",	;
			"Undo"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Margins_ID,	;
		HyperLabel{#SetMargins,	;
			"&Margins",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_ID,	;
		HyperLabel{#Edit_AlignMent,	;
			"&AlignMent",	;
			,	;
			,},GetSubMenu(self:Handle( ),1),3)
	self:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Left_ID,	;
		HyperLabel{#Edit_AlignMent_Left,	;
			"Left",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Center_ID,	;
		HyperLabel{#Edit_AlignMent_Center,	;
			"Center",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Edit_AlignMent_Right_ID,	;
		HyperLabel{#Edit_AlignMent_Right,	;
			"Right",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Cut_ID,	;
		HyperLabel{#Cut,	;
			"Cu&t	Ctrl+X",	;
			"Cut to Clipboard",	;
			"Cut"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Copy_ID,	;
		HyperLabel{#Copy,	;
			"&Copy	Ctrl+C",	;
			"Copy to Clipboard",	;
			"Copy"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Paste_ID,	;
		HyperLabel{#Paste,	;
			"&Paste	Ctrl+V",	;
			"Paste from Clipboard",	;
			"Paste"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Find_ID,	;
		HyperLabel{#Edit_Find,	;
			"&Find	Alt+F3",	;
			"Find a specific expression",	;
			"Find"})
	self:RegisterItem(IDM_StandardPadMenu_Edit_Find_Next_ID,	;
		HyperLabel{#Edit_Find_Next,	;
			"Find Ne&xt	F3",	;
			"Find next matching expression",	;
			"FindNext"})
	self:RegisterItem(IDM_StandardPadMenu_Window_ID,	;
		HyperLabel{#Window,	;
			"&Window",	;
			"Arrange child windows",	;
			,},self:Handle( ),2)
	self:RegisterItem(IDM_StandardPadMenu_Window_Cascade_ID,	;
		HyperLabel{#WindowCascade,	;
			"&Cascade",	;
			"Arrange child windows in a cascade",	;
			"WindowCascade"})
	self:RegisterItem(IDM_StandardPadMenu_Window_Tile_ID,	;
		HyperLabel{#WindowTile,	;
			"&Tile",	;
			"Arrange child windows tiled",	;
			"WindowTile"})
	self:RegisterItem(IDM_StandardPadMenu_Window_Close_ID,	;
		HyperLabel{#Close,	;
			"C&lose",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Window_Close_All_ID,	;
		HyperLabel{#CloseAll,	;
			"Close &All",	;
			,	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Help_ID,	;
		HyperLabel{#Help,	;
			"&Help",	;
			,	;
			,},self:Handle( ),3)
	self:RegisterItem(IDM_StandardPadMenu_Help_Index_ID,	;
		HyperLabel{#HelpIndex,	;
			"&Index",	;
			"Index of help",	;
			,})
	self:RegisterItem(IDM_StandardPadMenu_Help_About_ID,	;
		HyperLabel{#HelpAbout,	;
			"&About...",	;
			"About VOPad",	;
			,})

	self:SetAutoUpDate( 2 )

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_NEWSHEET,IDM_StandardPadMenu_File_New_ID)
	oTB:AddTipText(IDT_NEWSHEET,IDM_StandardPadMenu_File_New_ID,"Edit new file")

	oTB:AppendItem(IDT_OPEN,IDM_StandardPadMenu_File_Open_ID)
	oTB:AddTipText(IDT_OPEN,IDM_StandardPadMenu_File_Open_ID,"Open  a file")

	oTB:AppendItem(IDT_SAVE,IDM_StandardPadMenu_File_Save_ID)
	oTB:AddTipText(IDT_SAVE,IDM_StandardPadMenu_File_Save_ID,"Save this file")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_PRINT,IDM_StandardPadMenu_File_Print_ID)
	oTB:AddTipText(IDT_PRINT,IDM_StandardPadMenu_File_Print_ID,"Print")

	oTB:AppendItem(IDT_SIZEPLUS,IDM_StandardPadMenu_Edit_Font_ID)
	oTB:AddTipText(IDT_SIZEPLUS,IDM_StandardPadMenu_Edit_Font_ID,"Font")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT,IDM_StandardPadMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT,IDM_StandardPadMenu_Edit_Cut_ID,"Cut")

	oTB:AppendItem(IDT_COPY,IDM_StandardPadMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY,IDM_StandardPadMenu_Edit_Copy_ID,"Copy")

	oTB:AppendItem(IDT_PASTE,IDM_StandardPadMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE,IDM_StandardPadMenu_Edit_Paste_ID,"Paste")

	oTB:AppendItem(IDT_LEFT,IDM_StandardPadMenu_Edit_AlignMent_Left_ID)
	oTB:AddTipText(IDT_LEFT,IDM_StandardPadMenu_Edit_AlignMent_Left_ID,"Left")

	oTB:AppendItem(IDT_CENTER,IDM_StandardPadMenu_Edit_AlignMent_Center_ID)
	oTB:AddTipText(IDT_CENTER,IDM_StandardPadMenu_Edit_AlignMent_Center_ID,"Center")

	oTB:AppendItem(IDT_RIGHT,IDM_StandardPadMenu_Edit_AlignMent_Right_ID)
	oTB:AddTipText(IDT_RIGHT,IDM_StandardPadMenu_Edit_AlignMent_Right_ID,"Right")

	oTB:AppendItem(IDT_HELP,IDM_StandardPadMenu_Help_About_ID)
	oTB:AddTipText(IDT_HELP,IDM_StandardPadMenu_Help_About_ID,"About")

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := StandardPadMenu_Accelerator{ }
self:DisableItem(IDM_StandardPadMenu_Edit_AlignMent_ID)

	return self
END CLASS
CLASS StandardPadMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_StandardPadMenu, _GetInst( )})

	return self
END CLASS
