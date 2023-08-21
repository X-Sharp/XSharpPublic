#region DEFINES
Define IDA_CustomerExplorerMenu := "CustomerExplorerMenu"
Define IDA_EditOrdersMenu := "EditOrdersMenu"
Define IDA_EMPTYSHELLMENU := "EMPTYSHELLMENU"
Define IDA_OrdersListViewMenu := "OrdersListViewMenu"
Define IDM_CustomerExplorerMenu := "CustomerExplorerMenu"
Define IDM_CustomerExplorerMenu_Edit_Copy_ID := 26588
Define IDM_CustomerExplorerMenu_Edit_Cut_ID := 26587
Define IDM_CustomerExplorerMenu_Edit_ID := 26586
Define IDM_CustomerExplorerMenu_Edit_Paste_ID := 26589
Define IDM_CustomerExplorerMenu_File_Close_ID := 26581
Define IDM_CustomerExplorerMenu_File_Exit_ID := 26585
Define IDM_CustomerExplorerMenu_File_ID := 26579
Define IDM_CustomerExplorerMenu_File_Open_ID := 26580
Define IDM_CustomerExplorerMenu_File_Print_Setup_ID := 26583
Define IDM_CustomerExplorerMenu_Help_About_ID := 26604
Define IDM_CustomerExplorerMenu_Help_Context_Help_ID := 26601
Define IDM_CustomerExplorerMenu_Help_ID := 26599
Define IDM_CustomerExplorerMenu_Help_Index_ID := 26600
Define IDM_CustomerExplorerMenu_Help_Using_Help_ID := 26602
Define IDM_CustomerExplorerMenu_View_Icon_View_ID := 26591
Define IDM_CustomerExplorerMenu_View_ID := 26590
Define IDM_CustomerExplorerMenu_View_List_View_ID := 26593
Define IDM_CustomerExplorerMenu_View_Report_View_ID := 26594
Define IDM_CustomerExplorerMenu_View_Small_Icon_View_ID := 26592
Define IDM_CustomerExplorerMenu_Window_Cascade_ID := 26596
Define IDM_CustomerExplorerMenu_Window_Close_All_ID := 26598
Define IDM_CustomerExplorerMenu_Window_ID := 26595
Define IDM_CustomerExplorerMenu_Window_Tile_ID := 26597
Define IDM_EditOrdersMenu := "EditOrdersMenu"
Define IDM_EditOrdersMenu_Edit_Clear_ID := 13997
Define IDM_EditOrdersMenu_Edit_Copy_ID := 13995
Define IDM_EditOrdersMenu_Edit_Cut_ID := 13994
Define IDM_EditOrdersMenu_Edit_ID := 13992
Define IDM_EditOrdersMenu_Edit_Paste_ID := 13996
Define IDM_EditOrdersMenu_Edit_Undo_ID := 13993
Define IDM_EditOrdersMenu_File_Exit_ID := 13991
Define IDM_EditOrdersMenu_File_ID := 13981
Define IDM_EditOrdersMenu_File_New_ID := 13982
Define IDM_EditOrdersMenu_File_Open_ID := 13983
Define IDM_EditOrdersMenu_File_Page_Setup_ID := 13988
Define IDM_EditOrdersMenu_File_Print_ID := 13987
Define IDM_EditOrdersMenu_File_Print_Setup_ID := 13989
Define IDM_EditOrdersMenu_File_Save_As_ID := 13985
Define IDM_EditOrdersMenu_File_Save_ID := 13984
Define IDM_EditOrdersMenu_Help_About_ID := 14006
Define IDM_EditOrdersMenu_Help_Commands_ID := 14003
Define IDM_EditOrdersMenu_Help_ID := 14001
Define IDM_EditOrdersMenu_Help_Index_ID := 14002
Define IDM_EditOrdersMenu_Help_Using_help_ID := 14004
Define IDM_EditOrdersMenu_Window_Cascade_ID := 13999
Define IDM_EditOrdersMenu_Window_ID := 13998
Define IDM_EditOrdersMenu_Window_Tile_ID := 14000
Define IDM_EMPTYSHELLMENU := "EMPTYSHELLMENU"
Define IDM_EMPTYSHELLMENU_File_Exit_ID := 21654
Define IDM_EMPTYSHELLMENU_File_ID := 21649
Define IDM_EMPTYSHELLMENU_File_Open_ID := 21650
Define IDM_EMPTYSHELLMENU_File_Print_Setup_ID := 21652
Define IDM_EMPTYSHELLMENU_Help_About_ID := 21659
Define IDM_EMPTYSHELLMENU_Help_ID := 21655
Define IDM_EMPTYSHELLMENU_Help_Index_ID := 21656
Define IDM_EMPTYSHELLMENU_Help_Using_help_ID := 21657
Define IDM_OrdersListViewMenu := "OrdersListViewMenu"
Define IDM_OrdersListViewMenu_X_Edit_Order_ID := 26128
Define IDM_OrdersListViewMenu_X_ID := 26127
Define IDM_OrdersListViewMenu_X_Refresh_ID := 26130
#endregion

CLASS CustomerExplorerMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_CustomerExplorerMenu, _GetInst( )})

	self:RegisterItem(IDM_CustomerExplorerMenu_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_CustomerExplorerMenu_File_Open_ID,	;
		HyperLabel{#FileOpen,	;
			"&Open...	CTRL+O",	;
			"Open a file",	;
			"File_Open"})
	self:RegisterItem(IDM_CustomerExplorerMenu_File_Close_ID,	;
		HyperLabel{#CliseExplorer,	;
			"&Close",	;
			"Close current explorer window",	;
			"Close_Explorer"})
	self:RegisterItem(IDM_CustomerExplorerMenu_File_Print_Setup_ID,	;
		HyperLabel{#FilePrinterSetup,	;
			"P&rint Setup...",	;
			"Setup printer options",	;
			"File_Printer_Setup"})
	self:RegisterItem(IDM_CustomerExplorerMenu_File_Exit_ID,	;
		HyperLabel{#FileExit,	;
			"E&xit	ALT+F4",	;
			"End of application",	;
			"File_Exit"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Edit_ID,	;
		HyperLabel{#Edit,	;
			"&Edit",	;
			"Edit information",	;
			"Edit"},self:Handle( ),1)
	self:RegisterItem(IDM_CustomerExplorerMenu_Edit_Cut_ID,	;
		HyperLabel{#Cut,	;
			"Cu&t	CTRL+X",	;
			"Cut to clipboard",	;
			"Cut"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Edit_Copy_ID,	;
		HyperLabel{#Copy,	;
			"&Copy	CTRL+C",	;
			"Copy to clipboard",	;
			"Copy"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Edit_Paste_ID,	;
		HyperLabel{#Paste,	;
			"&Paste	CTRL+V",	;
			"Paste from clipboard",	;
			"Paste"})
	self:RegisterItem(IDM_CustomerExplorerMenu_View_ID,	;
		HyperLabel{#View,	;
			"&View",	;
			,	;
			,},self:Handle( ),2)
	self:RegisterItem(IDM_CustomerExplorerMenu_View_Icon_View_ID,	;
		HyperLabel{#ViewIcon,	;
			"&Icon View	SHIFT+F2",	;
			"View as icons",	;
			"ViewIcon"})
	self:RegisterItem(IDM_CustomerExplorerMenu_View_Small_Icon_View_ID,	;
		HyperLabel{#ViewSmallIcon,	;
			"&Small Icon View	F2",	;
			"View as small icons",	;
			"ViewSmallIcon"})
	self:RegisterItem(IDM_CustomerExplorerMenu_View_List_View_ID,	;
		HyperLabel{#ViewList,	;
			"&List View",	;
			"View as a list",	;
			"ViewList"})
	self:RegisterItem(IDM_CustomerExplorerMenu_View_Report_View_ID,	;
		HyperLabel{#ViewReport,	;
			"&Report View",	;
			"View as a report",	;
			"ViewReport"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Window_ID,	;
		HyperLabel{#Window,	;
			"&Window",	;
			"Arrange child windows",	;
			,},self:Handle( ),3)
	self:RegisterItem(IDM_CustomerExplorerMenu_Window_Cascade_ID,	;
		HyperLabel{#WindowCascade,	;
			"&Cascade",	;
			"Arrange child windows in a cascade",	;
			"WindowCascade"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Window_Tile_ID,	;
		HyperLabel{#WindowTile,	;
			"&Tile",	;
			"Arrange child windows tiled",	;
			"WindowTile"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Window_Close_All_ID,	;
		HyperLabel{#CloseAllChildren,	;
			"Close A&ll",	;
			"Close all child windows",	;
			"WindowCloseAll"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Help_ID,	;
		HyperLabel{#Help,	;
			"&Help",	;
			,	;
			,},self:Handle( ),4)
	self:RegisterItem(IDM_CustomerExplorerMenu_Help_Index_ID,	;
		HyperLabel{#HelpIndex,	;
			"&Index	F1",	;
			"Index of help",	;
			"Help_Index"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Help_Context_Help_ID,	;
		HyperLabel{#HelpContext,	;
			"&Context Help	CTRL+F1",	;
			"Context sensitive help",	;
			"Help_ContextHelp"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Help_Using_Help_ID,	;
		HyperLabel{#HelpUsingHelp,	;
			"&Using Help",	;
			"How to use help",	;
			"Help_UsingHelp"})
	self:RegisterItem(IDM_CustomerExplorerMenu_Help_About_ID,	;
		HyperLabel{#HelpAboutDialog,	;
			"&About...",	;
			"About application",	;
			,})

	self:SetAutoUpDate( 3 )

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_CLOSE,IDM_CustomerExplorerMenu_File_Close_ID)
	oTB:AddTipText(IDT_CLOSE,IDM_CustomerExplorerMenu_File_Close_ID,"Close File")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT,IDM_CustomerExplorerMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT,IDM_CustomerExplorerMenu_Edit_Cut_ID,"Cut")

	oTB:AppendItem(IDT_COPY,IDM_CustomerExplorerMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY,IDM_CustomerExplorerMenu_Edit_Copy_ID,"Copy")

	oTB:AppendItem(IDT_PASTE,IDM_CustomerExplorerMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE,IDM_CustomerExplorerMenu_Edit_Paste_ID,"Paste")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_OBJECT,IDM_CustomerExplorerMenu_View_Icon_View_ID)
	oTB:AddTipText(IDT_OBJECT,IDM_CustomerExplorerMenu_View_Icon_View_ID,"Icon View")

	oTB:AppendItem(IDT_COLUMNFORMAT,IDM_CustomerExplorerMenu_View_Small_Icon_View_ID)
	oTB:AddTipText(IDT_COLUMNFORMAT,IDM_CustomerExplorerMenu_View_Small_Icon_View_ID,"Small Icon View")

	oTB:AppendItem(IDT_BULLETLIST,IDM_CustomerExplorerMenu_View_List_View_ID)
	oTB:AddTipText(IDT_BULLETLIST,IDM_CustomerExplorerMenu_View_List_View_ID,"List View")

	oTB:AppendItem(IDT_NUMBERLIST,IDM_CustomerExplorerMenu_View_Report_View_ID)
	oTB:AddTipText(IDT_NUMBERLIST,IDM_CustomerExplorerMenu_View_Report_View_ID,"Report View")

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := CustomerExplorerMenu_Accelerator{ }

	return self
END CLASS
CLASS CustomerExplorerMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_CustomerExplorerMenu, _GetInst( )})

	return self
END CLASS
CLASS EMPTYSHELLMENU INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_EMPTYSHELLMENU, _GetInst( )})

	self:RegisterItem(IDM_EMPTYSHELLMENU_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_EMPTYSHELLMENU_File_Open_ID,	;
		HyperLabel{#FileOpen,	;
			"&Open...	CTRL+O",	;
			"Open a file",	;
			"File_Open"})
	self:RegisterItem(IDM_EMPTYSHELLMENU_File_Print_Setup_ID,	;
		HyperLabel{#FilePrinterSetup,	;
			"P&rint Setup...",	;
			"Setup printer options",	;
			"File_Printer_Setup"})
	self:RegisterItem(IDM_EMPTYSHELLMENU_File_Exit_ID,	;
		HyperLabel{#FileExit,	;
			"E&xit	ALT+F4",	;
			"End of application",	;
			"File_Exit"})
	self:RegisterItem(IDM_EMPTYSHELLMENU_Help_ID,	;
		HyperLabel{#Help,	;
			"&Help",	;
			,	;
			,},self:Handle( ),1)
	self:RegisterItem(IDM_EMPTYSHELLMENU_Help_Index_ID,	;
		HyperLabel{#HelpIndex,	;
			"&Index	F1",	;
			"Index of help",	;
			"Help_Index"})
	self:RegisterItem(IDM_EMPTYSHELLMENU_Help_Using_help_ID,	;
		HyperLabel{#HelpUsingHelp,	;
			"&Using help",	;
			"How to use help",	;
			"Help_UsingHelp"})
	self:RegisterItem(IDM_EMPTYSHELLMENU_Help_About_ID,	;
		HyperLabel{#HelpAboutDialog,	;
			"&About...",	;
			"About application",	;
			,})

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_OPEN,IDM_EMPTYSHELLMENU_File_Open_ID)
	oTB:AddTipText(IDT_OPEN,IDM_EMPTYSHELLMENU_File_Open_ID,"Open File")

	oTB:AppendItem(IDT_HELP,IDM_EMPTYSHELLMENU_Help_About_ID)
	oTB:AddTipText(IDT_HELP,IDM_EMPTYSHELLMENU_Help_About_ID,"Help")

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := EMPTYSHELLMENU_Accelerator{ }

	return self
END CLASS
CLASS EMPTYSHELLMENU_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_EMPTYSHELLMENU, _GetInst( )})

	return self
END CLASS
CLASS OrdersListViewMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 

	SELF:PreInit()
	SUPER(ResourceID{IDM_OrdersListViewMenu, _GetInst( )})

	self:RegisterItem(IDM_OrdersListViewMenu_X_ID,	;
		HyperLabel{#_X,	;
			"X",	;
			,	;
			,},self:Handle( ),0)
	self:RegisterItem(IDM_OrdersListViewMenu_X_Edit_Order_ID,	;
		HyperLabel{#EditOrder,	;
			"&Edit Order",	;
			,	;
			,})
	self:RegisterItem(IDM_OrdersListViewMenu_X_Refresh_ID,	;
		HyperLabel{#Refresh,	;
			"&Refresh",	;
			,	;
			,})

	SELF:PostInit()
	return self
END CLASS
