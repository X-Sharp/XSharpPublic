//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

FUNCTION DbgShowWorkAreas() AS VOID
    VAR oWin := XSharp.Debugger.WorkareasWindow{}
    oWin:ShowDialog()
    RETURN

FUNCTION DbgShowSettings() AS VOID
    VAR oWin := XSharp.Debugger.SettingsWindow{}
    oWin:ShowDialog()

FUNCTION DbgShowGlobals() AS VOID
    VAR oWin := XSharp.Debugger.GlobalsWindow{}
    oWin:ShowDialog()

FUNCTION DbgShowMemvars() AS VOID
    VAR oWin := XSharp.Debugger.MemVarsWindow{}
    oWin:ShowDialog()
