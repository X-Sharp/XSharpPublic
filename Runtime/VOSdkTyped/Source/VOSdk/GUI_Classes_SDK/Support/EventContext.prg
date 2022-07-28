/// <include file="Gui.xml" path="doc/EventContext/*" />


CLASS EventContext INHERIT VObject

/// <include file="Gui.xml" path="doc/EventContext.Destroy/*" />
//METHOD Destroy() AS USUAL STRICT
	//SUPER:Destroy()
	//RETURN SELF

/// <include file="Gui.xml" path="doc/EventContext.ctor/*" />
CONSTRUCTOR()
	SUPER()
	RETURN

/// <include file="Gui.xml" path="doc/EventContext.Override/*" />
METHOD Override()
	RETURN NIL

END CLASS

