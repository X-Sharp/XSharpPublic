//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/ModelessDialog/*" />
CLASS ModelessDialog INHERIT DialogWindow

/// <include file="Gui.xml" path="doc/ModelessDialog.ctor/*" />
	CONSTRUCTOR(oOwner, xResourceID)
		SUPER(oOwner, xResourceID, FALSE)
		RETURN

END CLASS

