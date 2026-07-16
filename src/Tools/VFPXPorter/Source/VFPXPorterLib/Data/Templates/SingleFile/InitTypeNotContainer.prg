


PRIVATE METHOD InitializeComponent() AS VOID STRICT
	SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.None

    <@childsInstantiate@>

    <@childsInitialize@>

	<@addChildsToParent@>

	<@formProps@>

PUBLIC CONSTRUCTOR( InitParamsList PARAMS USUAL[] )
	SUPER( InitParamsList )

	InitializeComponent()
	<@userdefProps@>

	<@EventHandlers@>
