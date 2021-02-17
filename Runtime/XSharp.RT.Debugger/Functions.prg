//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

STATIC CLASS XSharp.RT.Debugger.Functions
/// <summary>Open a window to display the open workareas/cursors</summary>
STATIC METHOD DbgShowWorkAreas() AS VOID
    VAR oWin := XSharp.Debugger.WorkareasWindow{}
    oWin:ShowDialog()
    RETURN

/// <summary>Open a window to show the current settings.</summary>
STATIC METHOD DbgShowSettings() AS VOID
    VAR oWin := XSharp.Debugger.SettingsWindow{}
    oWin:ShowDialog()

/// <summary>Open a window to show the (public) symbols in the running app and the other loaded assemblies</summary>
STATIC METHOD DbgShowGlobals() AS VOID
    VAR oWin := XSharp.Debugger.GlobalsWindow{}
    oWin:ShowDialog()

/// <summary>Open a window to show the Publics and privates</summary>
STATIC METHOD DbgShowMemvars() AS VOID
    VAR oWin := XSharp.Debugger.MemVarsWindow{}
    oWin:ShowDialog()
END CLASS
