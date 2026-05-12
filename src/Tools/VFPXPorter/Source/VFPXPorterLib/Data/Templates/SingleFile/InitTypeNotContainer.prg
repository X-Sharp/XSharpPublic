


PRIVATE METHOD InitializeComponent() AS VOID STRICT
	SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.None

    <@childsInstantiate@>

    <@childsInitialize@>

	<@addChildsToParent@>

	<@formProps@>

PUBLIC CONSTRUCTOR( InitParamsListt PARAMS USUAL[] )
	SUPER( InitParamsList )

	InitializeComponent()
	<@userdefProps@>

	<@EventHandlers@>
