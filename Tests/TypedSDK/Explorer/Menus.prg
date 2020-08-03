#region DEFINES
DEFINE IDM_OrdersListViewMenu_X_ID := 20000
DEFINE IDM_OrdersListViewMenu_X_Edit_Order_ID := 20001
DEFINE IDM_OrdersListViewMenu_X_Refresh_ID := 20003
DEFINE IDM_EMPTYSHELLMENU_File_ID := 20500
DEFINE IDM_EMPTYSHELLMENU_File_Open_ID := 20501
DEFINE IDM_EMPTYSHELLMENU_File_Print_Setup_ID := 20503
DEFINE IDM_EMPTYSHELLMENU_File_Exit_ID := 20505
DEFINE IDM_EMPTYSHELLMENU_Help_ID := 20506
DEFINE IDM_EMPTYSHELLMENU_Help_Index_ID := 20507
DEFINE IDM_EMPTYSHELLMENU_Help_Using_help_ID := 20508
DEFINE IDM_EMPTYSHELLMENU_Help_About_ID := 20510
DEFINE IDM_CustomerExplorerMenu_File_ID := 21000
DEFINE IDM_CustomerExplorerMenu_File_Open_ID := 21001
DEFINE IDM_CustomerExplorerMenu_File_Close_ID := 21002
DEFINE IDM_CustomerExplorerMenu_File_Print_Setup_ID := 21004
DEFINE IDM_CustomerExplorerMenu_File_Exit_ID := 21006
DEFINE IDM_CustomerExplorerMenu_Edit_ID := 21007
DEFINE IDM_CustomerExplorerMenu_Edit_Cut_ID := 21008
DEFINE IDM_CustomerExplorerMenu_Edit_Copy_ID := 21009
DEFINE IDM_CustomerExplorerMenu_Edit_Paste_ID := 21010
DEFINE IDM_CustomerExplorerMenu_View_ID := 21011
DEFINE IDM_CustomerExplorerMenu_View_Icon_View_ID := 21012
DEFINE IDM_CustomerExplorerMenu_View_Small_Icon_View_ID := 21013
DEFINE IDM_CustomerExplorerMenu_View_List_View_ID := 21014
DEFINE IDM_CustomerExplorerMenu_View_Report_View_ID := 21015
DEFINE IDM_CustomerExplorerMenu_Window_ID := 21016
DEFINE IDM_CustomerExplorerMenu_Window_Cascade_ID := 21017
DEFINE IDM_CustomerExplorerMenu_Window_Tile_ID := 21018
DEFINE IDM_CustomerExplorerMenu_Window_Close_All_ID := 21019
DEFINE IDM_CustomerExplorerMenu_Help_ID := 21020
DEFINE IDM_CustomerExplorerMenu_Help_Index_ID := 21021
DEFINE IDM_CustomerExplorerMenu_Help_Context_Help_ID := 21022
DEFINE IDM_CustomerExplorerMenu_Help_Using_Help_ID := 21023
DEFINE IDM_CustomerExplorerMenu_Help_About_ID := 21025
DEFINE IDA_CustomerExplorerMenu := "CustomerExplorerMenu"
DEFINE IDA_EditOrdersMenu := "EditOrdersMenu"
DEFINE IDA_EMPTYSHELLMENU := "EMPTYSHELLMENU"
DEFINE IDA_OrdersListViewMenu := "OrdersListViewMenu"
DEFINE IDM_CustomerExplorerMenu := "CustomerExplorerMenu"
DEFINE IDM_EditOrdersMenu := "EditOrdersMenu"
DEFINE IDM_EditOrdersMenu_Edit_Clear_ID := 13997
DEFINE IDM_EditOrdersMenu_Edit_Copy_ID := 13995
DEFINE IDM_EditOrdersMenu_Edit_Cut_ID := 13994
DEFINE IDM_EditOrdersMenu_Edit_ID := 13992
DEFINE IDM_EditOrdersMenu_Edit_Paste_ID := 13996
DEFINE IDM_EditOrdersMenu_Edit_Undo_ID := 13993
DEFINE IDM_EditOrdersMenu_File_Exit_ID := 13991
DEFINE IDM_EditOrdersMenu_File_ID := 13981
DEFINE IDM_EditOrdersMenu_File_New_ID := 13982
DEFINE IDM_EditOrdersMenu_File_Open_ID := 13983
DEFINE IDM_EditOrdersMenu_File_Page_Setup_ID := 13988
DEFINE IDM_EditOrdersMenu_File_Print_ID := 13987
DEFINE IDM_EditOrdersMenu_File_Print_Setup_ID := 13989
DEFINE IDM_EditOrdersMenu_File_Save_As_ID := 13985
DEFINE IDM_EditOrdersMenu_File_Save_ID := 13984
DEFINE IDM_EditOrdersMenu_Help_About_ID := 14006
DEFINE IDM_EditOrdersMenu_Help_Commands_ID := 14003
DEFINE IDM_EditOrdersMenu_Help_ID := 14001
DEFINE IDM_EditOrdersMenu_Help_Index_ID := 14002
DEFINE IDM_EditOrdersMenu_Help_Using_help_ID := 14004
DEFINE IDM_EditOrdersMenu_Window_Cascade_ID := 13999
DEFINE IDM_EditOrdersMenu_Window_ID := 13998
DEFINE IDM_EditOrdersMenu_Window_Tile_ID := 14000
DEFINE IDM_EMPTYSHELLMENU := "EMPTYSHELLMENU"
DEFINE IDM_OrdersListViewMenu := "OrdersListViewMenu"
#endregion

PARTIAL CLASS CustomerExplorerMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	LOCAL oTB AS Toolbar

	SELF:PreInit()

	SUPER( ResourceID { "CustomerExplorerMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_CustomerExplorerMenu_File_ID, ;
		HyperLabel{ #File , "&File" ,  , "File" } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_CustomerExplorerMenu_File_Open_ID, ;
		HyperLabel{ #FileOpen , "&Open...	Ctrl+O" , "Open a file" , "File_Open" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_File_Close_ID, ;
		HyperLabel{ #CliseExplorer , "&Close" , "Close current explorer window" , "Close_Explorer" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_File_Print_Setup_ID, ;
		HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_File_Exit_ID, ;
		HyperLabel{ #FileExit , "E&xit	Alt+F4" , "End of application" , "File_Exit" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Edit_ID, ;
		HyperLabel{ #Edit , "&Edit" , "Edit information" , "Edit" } , SELF:Handle() , 1)

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Edit_Cut_ID, ;
		HyperLabel{ #Cut , "Cu&t	Ctrl+X" , "Cut to clipboard" , "Cut" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Edit_Copy_ID, ;
		HyperLabel{ #Copy , "&Copy	Ctrl+C" , "Copy to clipboard" , "Copy" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Edit_Paste_ID, ;
		HyperLabel{ #Paste , "&Paste	Ctrl+V" , "Paste from clipboard" , "Paste" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_View_ID, ;
		HyperLabel{ #View , "&View" ,  ,  } , SELF:Handle() , 2)

	SELF:RegisterItem(IDM_CustomerExplorerMenu_View_Icon_View_ID, ;
		HyperLabel{ #ViewIcon , "&Icon View	Shift+F2" , "View as icons" , "ViewIcon" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_View_Small_Icon_View_ID, ;
		HyperLabel{ #ViewSmallIcon , "&Small Icon View	F2" , "View as small icons" , "ViewSmallIcon" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_View_List_View_ID, ;
		HyperLabel{ #ViewList , "&List View" , "View as a list" , "ViewList" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_View_Report_View_ID, ;
		HyperLabel{ #ViewReport , "&Report View" , "View as a report" , "ViewReport" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Window_ID, ;
		HyperLabel{ #Window , "&Window" , "Arrange child windows" ,  } , SELF:Handle() , 3)

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Window_Cascade_ID, ;
		HyperLabel{ #WindowCascade , "&Cascade" , "Arrange child windows in a cascade" , "WindowCascade" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Window_Tile_ID, ;
		HyperLabel{ #WindowTile , "&Tile" , "Arrange child windows tiled" , "WindowTile" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Window_Close_All_ID, ;
		HyperLabel{ #CloseAllChildren , "Close A&ll" , "Close all child windows" , "WindowCloseAll" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Help_ID, ;
		HyperLabel{ #Help , "&Help" ,  ,  } , SELF:Handle() , 4)

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Help_Index_ID, ;
		HyperLabel{ #HelpIndex , "&Index	F1" , "Index of help" , "Help_Index" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Help_Context_Help_ID, ;
		HyperLabel{ #HelpContext , "&Context Help	Ctrl+F1" , "Context sensitive help" , "Help_ContextHelp" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Help_Using_Help_ID, ;
		HyperLabel{ #HelpUsingHelp , "&Using Help" , "How to use help" , "Help_UsingHelp" })

	SELF:RegisterItem(IDM_CustomerExplorerMenu_Help_About_ID, ;
		HyperLabel{ #HelpAboutDialog , "&About..." , "About application" ,  })

	SELF:SetAutoUpdate( 3 )

	oTB := Toolbar{}

	oTB:ButtonStyle := TB_ICONONLY
	oTB:Flat := TRUE
	oTB:EnableBands(FALSE)

	oTB:AppendItem(IDT_CLOSE , IDM_CustomerExplorerMenu_File_Close_ID)
	oTB:AddTipText(IDT_CLOSE , IDM_CustomerExplorerMenu_File_Close_ID , "Close File")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT , IDM_CustomerExplorerMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT , IDM_CustomerExplorerMenu_Edit_Cut_ID , "Cut")

	oTB:AppendItem(IDT_COPY , IDM_CustomerExplorerMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY , IDM_CustomerExplorerMenu_Edit_Copy_ID , "Copy")

	oTB:AppendItem(IDT_PASTE , IDM_CustomerExplorerMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE , IDM_CustomerExplorerMenu_Edit_Paste_ID , "Paste")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_OBJECT , IDM_CustomerExplorerMenu_View_Icon_View_ID)
	oTB:AddTipText(IDT_OBJECT , IDM_CustomerExplorerMenu_View_Icon_View_ID , "Icon View")

	oTB:AppendItem(IDT_COLUMNFORMAT , IDM_CustomerExplorerMenu_View_Small_Icon_View_ID)
	oTB:AddTipText(IDT_COLUMNFORMAT , IDM_CustomerExplorerMenu_View_Small_Icon_View_ID , "Small Icon View")

	oTB:AppendItem(IDT_BULLETLIST , IDM_CustomerExplorerMenu_View_List_View_ID)
	oTB:AddTipText(IDT_BULLETLIST , IDM_CustomerExplorerMenu_View_List_View_ID , "List View")

	oTB:AppendItem(IDT_NUMBERLIST , IDM_CustomerExplorerMenu_View_Report_View_ID)
	oTB:AddTipText(IDT_NUMBERLIST , IDM_CustomerExplorerMenu_View_Report_View_ID , "Report View")


	SELF:ToolBar := oTB
	SELF:Accelerator := CustomerExplorerMenu_Accelerator{ }

	SELF:PostInit()

	RETURN

END CLASS
PARTIAL CLASS CustomerExplorerMenu_Accelerator INHERIT Accelerator

CONSTRUCTOR()
	SUPER( ResourceID { "CustomerExplorerMenu_Accelerator" , _GetInst( ) } )
RETURN


END CLASS
PARTIAL CLASS EMPTYSHELLMENU INHERIT Menu

CONSTRUCTOR( oOwner )

	LOCAL oTB AS Toolbar

	SELF:PreInit()

	SUPER( ResourceID { "EMPTYSHELLMENU" , _GetInst( ) } )

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_File_ID, ;
		HyperLabel{ #File , "&File" ,  , "File" } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_File_Open_ID, ;
		HyperLabel{ #FileOpen , "&Open...	Ctrl+O" , "Open a file" , "File_Open" })

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_File_Print_Setup_ID, ;
		HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_File_Exit_ID, ;
		HyperLabel{ #FileExit , "E&xit	Alt+F4" , "End of application" , "File_Exit" })

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_Help_ID, ;
		HyperLabel{ #Help , "&Help" ,  ,  } , SELF:Handle() , 1)

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_Help_Index_ID, ;
		HyperLabel{ #HelpIndex , "&Index	F1" , "Index of help" , "Help_Index" })

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_Help_Using_help_ID, ;
		HyperLabel{ #HelpUsingHelp , "&Using help" , "How to use help" , "Help_UsingHelp" })

	SELF:RegisterItem(IDM_EMPTYSHELLMENU_Help_About_ID, ;
		HyperLabel{ #HelpAboutDialog , "&About..." , "About application" ,  })

	oTB := Toolbar{}

	oTB:ButtonStyle := TB_ICONONLY
	oTB:Flat := TRUE
	oTB:EnableBands(FALSE)

	oTB:AppendItem(IDT_OPEN , IDM_EMPTYSHELLMENU_File_Open_ID)
	oTB:AddTipText(IDT_OPEN , IDM_EMPTYSHELLMENU_File_Open_ID , "Open File")

	oTB:AppendItem(IDT_HELP , IDM_EMPTYSHELLMENU_Help_About_ID)
	oTB:AddTipText(IDT_HELP , IDM_EMPTYSHELLMENU_Help_About_ID , "Help")


	SELF:ToolBar := oTB
	SELF:Accelerator := EMPTYSHELLMENU_Accelerator{ }

	SELF:PostInit()

	RETURN

END CLASS
PARTIAL CLASS EMPTYSHELLMENU_Accelerator INHERIT Accelerator

CONSTRUCTOR()
	SUPER( ResourceID { "EMPTYSHELLMENU_Accelerator" , _GetInst( ) } )
RETURN


END CLASS
PARTIAL CLASS OrdersListViewMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "OrdersListViewMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_OrdersListViewMenu_X_ID, ;
		HyperLabel{ #OrdersListViewMenu_X , "X" ,  ,  } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_OrdersListViewMenu_X_Edit_Order_ID, ;
		HyperLabel{ #EditOrder , "&Edit Order" ,  ,  })

	SELF:RegisterItem(IDM_OrdersListViewMenu_X_Refresh_ID, ;
		HyperLabel{ #Refresh , "&Refresh" ,  ,  })

	SELF:PostInit()

	RETURN

END CLASS
