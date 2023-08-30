//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/DataDialog/*" />
CLASS DataDialog INHERIT DataWindow
    /// <include file="Gui.xml" path="doc/DataDialog.ctor/*" />
    CONSTRUCTOR(oOwner, oSource, nResourceID, nDialogStyle)
        // The only thing now left in this methods is that it sets
        // the WS_DLGFRAME style. The rest is all handled inside DataWindow:Init()
        // You can also call a datawindow directly and pass the WS_DLGFRAME dialog
        // style
        LOCAL dwStyle as LONG
        IF IsLong(nDialogStyle)
            dwStyle := _OR(WS_DLGFRAME, (LONG) nDialogStyle)
        ELSE
            dwStyle := WS_DLGFRAME
        ENDIF
        SUPER(oOwner, oSource, nResourceID, dwStyle)
        RETURN

END CLASS

