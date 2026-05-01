// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel.Design;
using System.Threading.Tasks;
using XSharp.LanguageService.Commands;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Handles the "View &gt; Other Windows &gt; X# Document Outline" command
    /// (defined in XSharpLanguagePackage.vsct, command ID 0x7001).
    /// </summary>
    internal sealed class DocumentOutlineCommand : AbstractCommand
    {
        /// <summary>
        /// Called by <see cref="AbstractCommand.InitializeCommands"/> via reflection.
        /// Registers the OLE menu-command handler with the VS shell.
        /// </summary>
        public static async Task InitializeAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var commandService =
                await XSharpLanguagePackage.Instance.GetServiceAsync(typeof(IMenuCommandService))
                as OleMenuCommandService;

            if (commandService == null)
                return;

            var cmdId = new CommandID(
                new Guid(GuidStrings.guidXSharpLanguageServiceCmdSetString),
                XSharpConstants.cmdidDocumentOutlineWindow);

            var menuCmd = new MenuCommand(OnExecute, cmdId);
            commandService.AddCommand(menuCmd);
        }

        private static void OnExecute(object sender, EventArgs e)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                var package = XSharpLanguagePackage.Instance as AsyncPackage;
                if (package != null)
                {
                    await package.ShowToolWindowAsync(
                        typeof(DocumentOutlineToolWindow),
                        0,
                        true,
                        package.DisposalToken);
                }
            });
        }
    }
}
