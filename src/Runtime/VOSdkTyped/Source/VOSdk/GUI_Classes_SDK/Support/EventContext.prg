//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/EventContext/*" />


CLASS EventContext INHERIT VObject

/// <include file="Gui.xml" path="doc/EventContext.Destroy/*" />
//METHOD Destroy() AS USUAL STRICT
	//SUPER:Destroy()
	//RETURN SELF

/// <include file="Gui.xml" path="doc/EventContext.ctor/*" />
CONSTRUCTOR() STRICT
	SUPER()
	RETURN

/// <include file="Gui.xml" path="doc/EventContext.Override/*" />
METHOD Override() STRICT
	RETURN NIL

END CLASS

