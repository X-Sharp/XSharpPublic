//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgMemvarsWindow)]
    internal sealed class CommandViewMemvars : XSharpDebuggerBaseCommand<CommandViewMemvars>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await XSharp.Debugger.UI.MemvarsWindow.ShowAsync();
        }
    }
    [Command(PackageIds.idDebuggerSubMenu)]
    internal sealed class DebuggerSubMenu : XSharpDebuggerBaseCommand<DebuggerSubMenu>
    {
    }

}
