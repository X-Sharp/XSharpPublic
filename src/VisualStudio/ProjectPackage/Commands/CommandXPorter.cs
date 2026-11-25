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
    [Command(PackageIds.idXporter)]
    internal sealed class CommandXPorter : BaseCommand<CommandXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"Bin\Xporter.exe");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
