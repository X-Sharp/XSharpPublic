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
    [Command(PackageIds.idFoxXporter)]
    internal sealed class CommandFoxXPorter : BaseCommand<CommandFoxXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"VFPXPorter\VFPXPorter.exe ");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
