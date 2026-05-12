PUBLIC CONSTRUCTOR( InitParamsList ) CLIPPER
	Super( InitParamsList )
	//
	PreInitializeComponent()
	InitializeComponent()
	InitContainers()
	InitGrids()
	SetDataEnvironment()
	<@userdefProps@>

PRIVATE METHOD PreInitializeComponent() AS VOID
    self:components := System.ComponentModel.Container{}
    SELF:_vfpToolTip	:=	System.Windows.Forms.ToolTip{SELF:components}
	
PRIVATE METHOD InitContainers() AS Void
	// The following lines contains EventHandlers settings for Child-elements of CustomControls
	<@InitContainers@>

PRIVATE METHOD InitGrids() AS Void
	// The following lines contains settings for Columns of Grids
	<@InitGrids@>

VIRTUAL METHOD SetDataEnvironment() AS VOID
	<@setdataenvironment@>


	<@EventHandlers@>


