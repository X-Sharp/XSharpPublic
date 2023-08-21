//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

PARTIAL STATIC CLASS XSharp.RT.Debugger.Functions
/// <summary>Open a window to display the open workareas/cursors for the current thread.</summary>
/// <remarks>You can add this function behind a (hidden) menu option in your appliction to display
/// the open areas for you as developer </remarks>
/// <seealso cref='T:XSharp.Debugger.WorkareasWindow' />
STATIC METHOD DbgShowWorkAreas() AS VOID
    VAR oWin := XSharp.Debugger.WorkareasWindow{}
    oWin:ShowDialog()
    RETURN

/// <summary>Open a window to show the global settings for the current thread.</summary>
/// <remarks>You can add this function behind a (hidden) menu option in your appliction to display
/// the settings to you as developer </remarks>
/// <seealso cref='T:XSharp.Debugger.SettingsWindow' />
STATIC METHOD DbgShowSettings() AS VOID
    VAR oWin := XSharp.Debugger.SettingsWindow{}
    oWin:ShowDialog()

/// <summary>Open a window to show the (public) symbols in the running app and the other loaded assemblies</summary>
/// <remarks>You can add this function behind a (hidden) menu option in your appliction to display
/// the globals to you as developer </remarks>
/// <seealso cref='T:XSharp.Debugger.GlobalsWindow' />
STATIC METHOD DbgShowGlobals() AS VOID
    VAR oWin := XSharp.Debugger.GlobalsWindow{}
    oWin:ShowDialog()

/// <summary>Open a window to show the publics and privates for the current thread.</summary>
/// <remarks>You can add this function behind a (hidden) menu option in your appliction to display
/// the memory variables to you as developer </remarks>
/// <seealso cref='T:XSharp.Debugger.MemVarsWindow' />
STATIC METHOD DbgShowMemvars() AS VOID
    VAR oWin := XSharp.Debugger.MemVarsWindow{}
    oWin:ShowDialog()
END CLASS
