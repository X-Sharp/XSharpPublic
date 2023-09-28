#region DEFINES
Define IDA_GraphMenu := "GraphMenu"
Define IDA_ShellMenu := "ShellMenu"
Define IDM_GraphMenu := "GraphMenu"
Define IDM_GraphMenu_Data_ID := 21215
Define IDM_GraphMenu_Data_Sample_Set_1_ID := 21216
Define IDM_GraphMenu_Data_Sample_Set_2_ID := 21217
Define IDM_GraphMenu_Data_Sample_Set_3_ID := 21218
Define IDM_GraphMenu_File_Exit_ID := 21210
Define IDM_GraphMenu_File_ID := 21207
Define IDM_GraphMenu_File_New_ID := 21208
Define IDM_GraphMenu_Type_Bar_ID := 21213
Define IDM_GraphMenu_Type_Column_ID := 32000
Define IDM_GraphMenu_Type_ID := 21211
Define IDM_GraphMenu_Type_Pie_ID := 21214
Define IDM_GraphMenu_Window_Cascade_ID := 21220
Define IDM_GraphMenu_Window_Close_All_ID := 21222
Define IDM_GraphMenu_Window_ID := 21219
Define IDM_GraphMenu_Window_Tile_ID := 21221
Define IDM_ShellMenu := "ShellMenu"
Define IDM_ShellMenu_File_Exit_ID := 20596
Define IDM_ShellMenu_File_ID := 20593
Define IDM_ShellMenu_File_New_ID := 32000
Define IDM_ShellMenu_Window_Cascade_ID := 20598
Define IDM_ShellMenu_Window_Close_All_ID := 20600
Define IDM_ShellMenu_Window_ID := 20597
Define IDM_ShellMenu_Window_Tile_ID := 20599
#endregion

CLASS GMENRIB INHERIT Bitmap
CONSTRUCTOR() 
   SUPER(ResourceID{"GMENRIB", _GetInst()})
   return self

END CLASS
CLASS GraphMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_GraphMenu, _GetInst( )})

	self:RegisterItem(IDM_GraphMenu_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_GraphMenu_File_New_ID,	;
		HyperLabel{#FileNEw,	;
			"&New	Ctrl+N",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_File_Exit_ID,	;
		HyperLabel{#FileExit,	;
			"E&xit	Alt+F4",	;
			"End of application",	;
			"File_Exit"})
	self:RegisterItem(IDM_GraphMenu_Type_ID,	;
		HyperLabel{#_Type,	;
			"&Type",	;
			,	;
			,},self:Handle( ),1)
	self:RegisterItem(IDM_GraphMenu_Type_Column_ID,	;
		HyperLabel{#ChangeType,	;
			"&Column	Ctrl+C",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_Type_Bar_ID,	;
		HyperLabel{#ChangeType,	;
			"&Bar	Ctrl+B",	;
			,	;
			"File_Close"})
	self:RegisterItem(IDM_GraphMenu_Type_Pie_ID,	;
		HyperLabel{#ChangeType,	;
			"&Pie	Ctrl+P",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_Data_ID,	;
		HyperLabel{#_Data,	;
			"&Data",	;
			,	;
			,},self:Handle( ),2)
	self:RegisterItem(IDM_GraphMenu_Data_Sample_Set_1_ID,	;
		HyperLabel{#ChangeData,	;
			"Sample Set &1	Ctrl+1",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_Data_Sample_Set_2_ID,	;
		HyperLabel{#ChangeData,	;
			"Sample Set &2	Ctrl+2",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_Data_Sample_Set_3_ID,	;
		HyperLabel{#ChangeData,	;
			"Sample Set &3	Ctrl+3",	;
			,	;
			,})
	self:RegisterItem(IDM_GraphMenu_Window_ID,	;
		HyperLabel{#_Window,	;
			"&Window",	;
			"Arrange child windows",	;
			,},self:Handle( ),3)
	self:RegisterItem(IDM_GraphMenu_Window_Cascade_ID,	;
		HyperLabel{#WindowCascade,	;
			"&Cascade	F4",	;
			"Arranges child windows in a cascade",	;
			,})
	self:RegisterItem(IDM_GraphMenu_Window_Tile_ID,	;
		HyperLabel{#WindowTile,	;
			"&Tile	Shift+F4",	;
			"Arranges child windows tiled",	;
			,})
	self:RegisterItem(IDM_GraphMenu_Window_Close_All_ID,	;
		HyperLabel{#CloseAllChildren,	;
			"Close &All",	;
			,	;
			,})

	self:SetAutoUpDate( 3 )

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_NEWSHEET,IDM_GraphMenu_Data_Sample_Set_1_ID)
	oTB:AddTipText(IDT_NEWSHEET,IDM_GraphMenu_Data_Sample_Set_1_ID,"Data Set 1")

	oTB:AppendItem(IDT_OPEN,IDM_GraphMenu_Data_Sample_Set_2_ID)
	oTB:AddTipText(IDT_OPEN,IDM_GraphMenu_Data_Sample_Set_2_ID,"Data Set 2")

	oTB:AppendItem(IDT_CLOSE,IDM_GraphMenu_Data_Sample_Set_3_ID)
	oTB:AddTipText(IDT_CLOSE,IDM_GraphMenu_Data_Sample_Set_3_ID,"Data Set 3")

	oTB:Bitmap := gmenrib{}
	oTB:ButtonSize := Dimension{16, 16}

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := GraphMenu_Accelerator{ }

	return self
END CLASS
CLASS GraphMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_GraphMenu, _GetInst( )})

	return self
END CLASS
CLASS ShellMenu INHERIT Menu
 
CONSTRUCTOR(oOwner) 
	local oTB as ToolBar

	SUPER(ResourceID{IDM_ShellMenu, _GetInst( )})

	self:RegisterItem(IDM_ShellMenu_File_ID,	;
		HyperLabel{#File,	;
			"&File",	;
			,	;
			"File"},self:Handle( ),0)
	self:RegisterItem(IDM_ShellMenu_File_New_ID,	;
		HyperLabel{#FileNew,	;
			"&New	Ctrl+N",	;
			"Creates a new MDI child window",	;
			,})
	self:RegisterItem(IDM_ShellMenu_File_Exit_ID,	;
		HyperLabel{#FileExit,	;
			"E&xit	Alt+F4",	;
			"End of application",	;
			"File_Exit"})
	self:RegisterItem(IDM_ShellMenu_Window_ID,	;
		HyperLabel{#_Window,	;
			"&Window",	;
			"Arrange child windows",	;
			,},self:Handle( ),1)
	self:RegisterItem(IDM_ShellMenu_Window_Cascade_ID,	;
		HyperLabel{#WindowCascade,	;
			"&Cascade	F4",	;
			"Arranges child windows in a cascade",	;
			,})
	self:RegisterItem(IDM_ShellMenu_Window_Tile_ID,	;
		HyperLabel{#WindowTile,	;
			"&Tile	Shift+F4",	;
			"Arranges child windows tiled",	;
			,})
	self:RegisterItem(IDM_ShellMenu_Window_Close_All_ID,	;
		HyperLabel{#CloseAllChildren,	;
			"Close &All",	;
			,	;
			,})

	self:SetAutoUpDate( 1 )

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_ICONONLY

	oTB:AppendItem(IDT_NEW,IDM_ShellMenu_File_New_ID)
	oTB:AddTipText(IDT_NEW,IDM_ShellMenu_File_New_ID,"New Window")

	oTB:Flat := true

	self:ToolBar := oTB

	self:Accelerator := ShellMenu_Accelerator{ }

	return self
END CLASS
CLASS ShellMenu_Accelerator INHERIT Accelerator
 
CONSTRUCTOR( ) 
	SUPER(ResourceID{IDA_ShellMenu, _GetInst( )})

	return self

END CLASS
