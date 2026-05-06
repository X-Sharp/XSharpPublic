// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
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
        public static async System.Threading.Tasks.Task InitializeAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var cmdOutline = await VS.Commands.FindCommandAsync("View.DocumentOutline");

            // We need to manually intercept the commenting command, because language services swallow these commands.
            if (cmdOutline != null)
                await VS.Commands.InterceptAsync(cmdOutline, () => Execute(Show));
        }

        private static void Show(DocumentView doc)
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
