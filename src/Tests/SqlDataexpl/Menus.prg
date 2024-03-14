#region DEFINES
Define IDA_DataExplorerMenu := "DataExplorerMenu"
Define IDA_DataLVContext := "DataLVContext"
Define IDA_EditMenu := "EditMenu"
Define IDM_DataExplorerMenu := "DataExplorerMenu"
Define IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID := 20739
Define IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID := 20737
Define IDM_DataExplorerMenu_Edit_Orders_ID := 20735
Define IDM_DataExplorerMenu_Edit_Orders_New_Order_ID := 20736
Define IDM_DataExplorerMenu_File_Exit_ID := 20734
Define IDM_DataExplorerMenu_File_ID := 20733
Define IDM_DataExplorerMenu_Help_About_ID := 32031
Define IDM_DataExplorerMenu_Help_Context_Help_ID := 20744
Define IDM_DataExplorerMenu_Help_ID := 20742
Define IDM_DataExplorerMenu_Help_Index_ID := 20743
Define IDM_DataExplorerMenu_Help_Using_Help_ID := 20745
Define IDM_DataExplorerMenu_View_ID := 20740
Define IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID := 20741
Define IDM_DataLVContext := "DataLVContext"
Define IDM_DataLVContext_DataListView_Delete_Order_ID := 21246
Define IDM_DataLVContext_DataListView_Edit_Order_ID := 21244
Define IDM_DataLVContext_DataListView_ID := 21242
Define IDM_DataLVContext_DataListView_New_Order_ID := 21243
Define IDM_EditMenu := "EditMenu"
Define IDM_EditMenu_Edit_Close_Window_ID := 20758
Define IDM_EditMenu_Edit_Copy_ID := 20755
Define IDM_EditMenu_Edit_Cut_ID := 20754
Define IDM_EditMenu_Edit_ID := 20753
Define IDM_EditMenu_Edit_Paste_ID := 20756
#endregion

CLASS DataExplorerMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_DataExplorerMenu, _GetInst( )})

	self:RegisterItem(IDM_DataExplorerMenu_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_DataExplorerMenu_File_Exit_ID,	;
		HyperLabel{#EndWindow,	;
			"E&xit	Alt+F4",	;
			"End application",	;
			,})
	self:RegisterItem(IDM_DataExplorerMenu_Edit_Orders_ID,	;
		HyperLabel{#_Edit_Orders,	;
			"&Edit Orders",	;
			,	;
			,},self:Handle( ),1)
	self:RegisterItem(IDM_DataExplorerMenu_Edit_Orders_New_Order_ID,	;
		HyperLabel{#NewLVRecord,	;
			"&New Order",	;
			,	;
			,})
	self:RegisterItem(IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID,	;
		HyperLabel{#EditLVRecord,	;
			"&Edit Order",	;
			,	;
			,})
	self:RegisterItem(IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID,	;
		HyperLabel{#DeleteLVRecord,	;
			"&Delete Order",	;
			,	;
			,})
	self:RegisterItem(IDM_DataExplorerMenu_View_ID,	;
		HyperLabel{#_View,	;
			"&View",	;
			,	;
			,},self:Handle( ),2)
	self:RegisterItem(IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID,	;
		HyperLabel{#SetShowFields,	;
			"&Show all Fields in Treeview",	;
			,	;
			,})
	self:RegisterItem(IDM_DataExplorerMenu_Help_ID,	;
		HyperLabel{#Help,	;
			"&Help",	;
			,	;
			,},self:Handle( ),3)
	self:RegisterItem(IDM_DataExplorerMenu_Help_Index_ID,	;
		HyperLabel{#HelpIndex,	;
			"&Index	F1",	;
			"Index of help",	;
			"Help_Index"})
	self:RegisterItem(IDM_DataExplorerMenu_Help_Context_Help_ID,	;
		HyperLabel{#HelpContext,	;
			"&Context Help	Ctrl+F1",	;
			"Context sensitive help",	;
			"Help_ContextHelp"})
	self:RegisterItem(IDM_DataExplorerMenu_Help_Using_Help_ID,	;
		HyperLabel{#HelpUsingHelp,	;
			"&Using Help",	;
			"How to use help",	;
			"Help_UsingHelp"})
	self:RegisterItem(IDM_DataExplorerMenu_Help_About_ID,	;
		HyperLabel{#HelpAbout,	;
			"&About...",	;
			"About application",	;
			,})

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_CLOSE,IDM_DataExplorerMenu_File_Exit_ID)
	oTB:AddTipText(IDT_CLOSE,IDM_DataExplorerMenu_File_Exit_ID,"Exit Application")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_PREVIEW,IDM_DataExplorerMenu_Edit_Orders_New_Order_ID)
	oTB:AddTipText(IDT_PREVIEW,IDM_DataExplorerMenu_Edit_Orders_New_Order_ID,"New Order")

	oTB:AppendItem(IDT_SIZEPLUS,IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID)
	oTB:AddTipText(IDT_SIZEPLUS,IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID,"Edit Order")

	oTB:AppendItem(IDT_UNDERLINE,IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID)
	oTB:AddTipText(IDT_UNDERLINE,IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID,"Delete Order")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CROSSOUT,IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID)
	oTB:AddTipText(IDT_CROSSOUT,IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID,"Show all Fields in the Treeview")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_ITALIC,IDM_DataExplorerMenu_Help_About_ID)
	oTB:AddTipText(IDT_ITALIC,IDM_DataExplorerMenu_Help_About_ID,"About Simple DataExplorer...")

	oTB:Bitmap := rbnToolbar{}
	oTB:ButtonSize := Dimension{16, 16}

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := DataExplorerMenu_Accelerator{ }
self:DisableItem(IDM_DataExplorerMenu_Edit_Orders_New_Order_ID)
self:DisableItem(IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID)
self:DisableItem(IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID)

	return self
END CLASS
CLASS DataExplorerMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_DataExplorerMenu, _GetInst( )})

	return self
END CLASS
CLASS DataLVContext INHERIT Menu
 
CONSTRUCTOR(oOwner) 

	SUPER(ResourceID{IDM_DataLVContext, _GetInst( )})

	self:RegisterItem(IDM_DataLVContext_DataListView_ID,	;
		HyperLabel{#NewRecordDialog,	;
			"&DataListView",	;
			,	;
			,},self:Handle( ),0)
	self:RegisterItem(IDM_DataLVContext_DataListView_New_Order_ID,	;
		HyperLabel{#NewLVRecord,	;
			"&New Order",	;
			,	;
			,})
	self:RegisterItem(IDM_DataLVContext_DataListView_Edit_Order_ID,	;
		HyperLabel{#EditLVRecord,	;
			"&Edit Order",	;
			,	;
			,})
	self:RegisterItem(IDM_DataLVContext_DataListView_Delete_Order_ID,	;
		HyperLabel{#DeleteLVRecord,	;
			"&Delete Order",	;
			,	;
			,})

	return self
END CLASS
CLASS EditMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_EditMenu, _GetInst( )})

	self:RegisterItem(IDM_EditMenu_Edit_ID,	;
		HyperLabel{#_Edit,	;
			"&Edit",	;
			,	;
			,},self:Handle( ),0)
	self:RegisterItem(IDM_EditMenu_Edit_Cut_ID,	;
		HyperLabel{#cut,	;
			"Cut	Ctrl+X",	;
			,	;
			,})
	self:RegisterItem(IDM_EditMenu_Edit_Copy_ID,	;
		HyperLabel{#copy,	;
			"&Copy	Ctrl+C",	;
			,	;
			,})
	self:RegisterItem(IDM_EditMenu_Edit_Paste_ID,	;
		HyperLabel{#paste,	;
			"&Paste	Ctrl+V",	;
			,	;
			,})
	self:RegisterItem(IDM_EditMenu_Edit_Close_Window_ID,	;
		HyperLabel{#CloseWindow,	;
			"Clos&e Window	Ctrl+F4",	;
			,	;
			,})

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_CLOSE,IDM_EditMenu_Edit_Close_Window_ID)
	oTB:AddTipText(IDT_CLOSE,IDM_EditMenu_Edit_Close_Window_ID,"Save record and close window")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CUT,IDM_EditMenu_Edit_Cut_ID)
	oTB:AddTipText(IDT_CUT,IDM_EditMenu_Edit_Cut_ID,"Cut")

	oTB:AppendItem(IDT_COPY,IDM_EditMenu_Edit_Copy_ID)
	oTB:AddTipText(IDT_COPY,IDM_EditMenu_Edit_Copy_ID,"Copy")

	oTB:AppendItem(IDT_PASTE,IDM_EditMenu_Edit_Paste_ID)
	oTB:AddTipText(IDT_PASTE,IDM_EditMenu_Edit_Paste_ID,"Paste")

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := EditMenu_Accelerator{ }

	return self
END CLASS
CLASS EditMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_EditMenu, _GetInst( )})

	return self
END CLASS
