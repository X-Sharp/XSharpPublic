


PRIVATE METHOD InitializeComponent() AS VOID STRICT
	SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.None

    <@childsInstantiate@>

    <@childsInitialize@>

	<@addChildsToParent@>

	<@formProps@>

PUBLIC CONSTRUCTOR( InitParamsList  ) CLIPPER
	SUPER( InitParamsList )

	InitializeComponent()
	<@userdefProps@>

	<@EventHandlers@>
