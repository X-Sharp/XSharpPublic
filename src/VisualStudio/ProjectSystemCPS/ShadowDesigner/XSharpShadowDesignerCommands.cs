//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ProjectSystem;
using XSharpModel;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Opens the shadow WinForms designer for X# forms in CPS projects (double click / Enter and View Designer).
    /// </summary>
    /// <remarks>
    /// A .prg is treated as a form when a matching .Designer.prg exists next to it (SDK-style projects rarely
    /// have an explicit SubType). When the shadow designer
    /// cannot be opened the command is not handled, so CPS falls back to its default action.
    /// </remarks>
    internal static class XSharpShadowDesignerCommands
    {
        public static bool TryGetFormFile(IImmutableSet<IProjectTree> nodes, out string path)
        {
            path = null;
            if (nodes == null || nodes.Count != 1)
                return false;
            var node = nodes.First();
            if (node.IsFolder || string.IsNullOrEmpty(node.FilePath))
                return false;
            if (!ShadowDesignerBridge.HasDesignerFile(node.FilePath))
                return false;
            path = node.FilePath;
            return true;
        }

        // Set while an open waits for the project load or a build (UI thread only): a second double click in that
        // time is swallowed instead of starting a second wait/build.
        private static bool opening;

        public static async Task<bool> TryOpenAsync(IProjectThreadingService threading, string path)
        {
            await threading.SwitchToUIThread();
            if (opening)
                return true;
            opening = true;
            try
            {
                var xProject = XSolution.FindFile(path)?.Project;
                if (xProject == null)
                {
                    Logger.Information("XSharp ShadowDesigner: no X# code model for " + path);
                    return false;
                }
                var (ok, error) = await ShadowDesignerBridge.EnsureReferencesAsync(xProject);
                await threading.SwitchToUIThread();
                // The project can have been unloaded or reloaded while waiting
                xProject = XSolution.FindFile(path)?.Project;
                if (ok && xProject != null)
                {
                    (ok, error) = await ShadowDesignerBridge.TryOpenAsync(path, xProject);
                    if (ok)
                        return true;
                }
                Logger.Information("XSharp ShadowDesigner: " + (error ?? "no X# code model for " + path));
                return false;
            }
            finally
            {
                opening = false;
            }
        }

        public static Task<CommandStatusResult> EnabledStatusAsync(string commandText) =>
            new CommandStatusResult(true, commandText, CommandStatus.Enabled | CommandStatus.Supported).AsTask();
    }

    /// <summary>
    /// Double click / Enter on an X# form in Solution Explorer (UIHierarchyWindow commands).
    /// </summary>
    [ExportCommandGroup(UIHierarchyWindowCommandSet)]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    [Order(1000)]
    internal sealed class XSharpShadowDesignerDefaultActionCommand : IAsyncCommandGroupHandler
    {
        private const string UIHierarchyWindowCommandSet = "{60481700-078B-11D1-AAF8-00A0C9055A90}";
        private const long DoubleClick = 2;
        private const long EnterKey = 3;

        private readonly IProjectThreadingService threading;

        [ImportingConstructor]
        public XSharpShadowDesignerDefaultActionCommand(IProjectThreadingService threading)
        {
            this.threading = threading;
        }

        public Task<CommandStatusResult> GetCommandStatusAsync(IImmutableSet<IProjectTree> nodes, long commandId, bool focused, string commandText, CommandStatus progressiveStatus)
        {
            if ((commandId == DoubleClick || commandId == EnterKey) && XSharpShadowDesignerCommands.TryGetFormFile(nodes, out _))
                return XSharpShadowDesignerCommands.EnabledStatusAsync(commandText);
            return CommandStatusResult.Unhandled.AsTask();
        }

        public Task<bool> TryHandleCommandAsync(IImmutableSet<IProjectTree> nodes, long commandId, bool focused, long commandExecuteOptions, IntPtr variantArgIn, IntPtr variantArgOut)
        {
            if ((commandId == DoubleClick || commandId == EnterKey) && XSharpShadowDesignerCommands.TryGetFormFile(nodes, out var path))
                return XSharpShadowDesignerCommands.TryOpenAsync(threading, path);
            return Task.FromResult(false);
        }
    }

    /// <summary>
    /// "View Designer" (Shift+F7) on an X# form.
    /// </summary>
    [ExportCommandGroup(VSConstants.CMDSETID.StandardCommandSet97_string)]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    [Order(1000)]
    internal sealed class XSharpShadowDesignerViewFormCommand : IAsyncCommandGroupHandler
    {
        private const long ViewForm = (long)VSConstants.VSStd97CmdID.ViewForm;

        private readonly IProjectThreadingService threading;

        [ImportingConstructor]
        public XSharpShadowDesignerViewFormCommand(IProjectThreadingService threading)
        {
            this.threading = threading;
        }

        public Task<CommandStatusResult> GetCommandStatusAsync(IImmutableSet<IProjectTree> nodes, long commandId, bool focused, string commandText, CommandStatus progressiveStatus)
        {
            if (commandId == ViewForm && XSharpShadowDesignerCommands.TryGetFormFile(nodes, out _))
                return XSharpShadowDesignerCommands.EnabledStatusAsync(commandText);
            return CommandStatusResult.Unhandled.AsTask();
        }

        public Task<bool> TryHandleCommandAsync(IImmutableSet<IProjectTree> nodes, long commandId, bool focused, long commandExecuteOptions, IntPtr variantArgIn, IntPtr variantArgOut)
        {
            if (commandId == ViewForm && XSharpShadowDesignerCommands.TryGetFormFile(nodes, out var path))
                return XSharpShadowDesignerCommands.TryOpenAsync(threading, path);
            return Task.FromResult(false);
        }
    }
}
