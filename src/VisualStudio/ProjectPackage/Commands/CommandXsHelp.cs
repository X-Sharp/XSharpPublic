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
    [Command(PackageIds.idHelpOffLine)]
    internal sealed class CommandXsHelp : BaseCommand<CommandXsHelp>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string docPath = Commands.GetXsPath(@"Help\XSharp.chm");
            await Commands.StartProcessAsync(docPath);
        }
    }
}
