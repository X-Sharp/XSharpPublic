/// <summary>
/// Clean up any resources being used.
/// </summary>
/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
protected method Dispose(disposing as logic) as void strict

	if (disposing .AND. (components != null))
		components:Dispose()
	endif
	Super:Dispose(disposing)
	return

PRIVATE METHOD PreInitializeComponent() AS VOID
    self:components := System.ComponentModel.Container{}
    SELF:_vfpToolTip	:=	System.Windows.Forms.ToolTip{SELF:components}

PRIVATE METHOD InitializeComponent() AS VOID STRICT
	SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.None

    <@childsInstantiate@>

    SELF:SuspendLayout()

    <@childsInitialize@>

	<@addChildsToParent@>

	<@formProps@>

	SELF:ResumeLayout( FALSE )

PUBLIC CONSTRUCTOR( InitParamsList ) CLIPPER
	SUPER( InitParamsList )
	//
	PreInitializeComponent()
	InitializeComponent()
	InitContainers()
	SetDataEnvironment()
	<@userdefProps@>

METHOD InitContainers() AS Void
	<@InitContainers@>

	<@EventHandlers@>

METHOD SetDataEnvironment() AS VOID
	<@setdataenvironment@>




