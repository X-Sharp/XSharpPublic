//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
#if REPOWINDOW
namespace XSharp.Project
{
    [Command(PackageIds.idViewRepo)]
    internal sealed class CommandViewRepo : BaseCommand<CommandViewRepo>
    {

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await RepositoryWindow.ShowAsync();
        }
        protected override Task InitializeCompletedAsync()
        {
            Command.Visible = true;

            return base.InitializeCompletedAsync();
        }

        protected override void BeforeQueryStatus(EventArgs e)
        {
            //base.BeforeQueryStatus(e);
            Command.Enabled = XSolution.HasProject;
            Command.Visible = true;

        }
    }
}
#endif
