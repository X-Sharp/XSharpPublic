//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharp.LanguageService.FindReferences;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace XSharp.LanguageService.Rename
{
    /// <summary>
    /// The Visual Studio "Preview Changes" dialog for a rename: one node per file and one per change, each with a
    /// check box, and the text of the file with the checked changes applied.
    /// </summary>
    internal static class RenamePreview
    {
        /// <summary>
        /// The plan without the unchecked changes, or null when the user cancels. UI thread.
        /// <paramref name="readText"/> returns the current text of a file (the editor text when it is open).
        /// </summary>
        internal static RenamePlan Show(RenamePlan plan, string oldName, string newName, Func<string, string> readText)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var service = Package.GetGlobalService(typeof(SVsPreviewChangesService)) as IVsPreviewChangesService;
            var componentModel = Package.GetGlobalService(typeof(SComponentModel)) as IComponentModel;
            var oleServices = Package.GetGlobalService(typeof(IOleServiceProvider)) as IOleServiceProvider;
            if (service == null || componentModel == null)
                return plan;
            var engine = new Engine(plan, oldName, newName, componentModel.GetService<IVsEditorAdaptersFactoryService>(), oleServices, readText);
            ErrorHandler.ThrowOnFailure(service.PreviewChanges(engine));
            if (!engine.Applied)
                return null;
            var dropped = new HashSet<ReferenceHit>(engine.Changes.Where(c => !c.Checked).Select(c => c.Hit));
            return RenamePlanner.Without(plan, dropped);
        }

        /// <summary>A node of the tree: a file (with its changes as children) or one change.</summary>
        private sealed class Node
        {
            internal string Text;
            internal bool Checked = true;
            internal ReferenceHit Hit;          // null for a file
            internal string FilePath;
            internal NodeList Children;          // null for a change
            internal Node Parent;
        }

        private sealed class Engine : IVsPreviewChangesEngine
        {
            private readonly string _oldName;
            private readonly string _newName;
            private readonly NodeList _root;
            private readonly IVsEditorAdaptersFactoryService _adapters;
            private readonly IOleServiceProvider _services;
            private readonly Dictionary<string, string> _texts = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            private readonly Func<string, string> _readText;

            internal Engine(RenamePlan plan, string oldName, string newName, IVsEditorAdaptersFactoryService adapters, IOleServiceProvider services,
                Func<string, string> readText)
            {
                _readText = readText;
                _oldName = oldName;
                _newName = newName;
                _adapters = adapters;
                _services = services;
                var files = new List<Node>();
                foreach (var file in plan.Files)
                {
                    var fileNode = new Node { Text = Path.GetFileName(file), FilePath = file };
                    var changes = plan.Changes.Where(h => string.Equals(h.FilePath, file, StringComparison.OrdinalIgnoreCase))
                        .OrderBy(h => h.Start)
                        .Select(h => new Node { Text = $"{h.Line + 1}: {h.LineText?.Trim()}", Hit = h, FilePath = file, Parent = fileNode })
                        .ToList();
                    fileNode.Children = new NodeList(this, changes);
                    files.Add(fileNode);
                }
                _root = new NodeList(this, files);
            }

            internal bool Applied { get; private set; }
            internal IEnumerable<Node> Changes => _root.Items.SelectMany(f => f.Children.Items);

            public int ApplyChanges() { Applied = true; return VSConstants.S_OK; }
            public int GetConfirmation(out string pbstrConfirmation) { pbstrConfirmation = "Apply"; return VSConstants.S_OK; }
            public int GetDescription(out string pbstrDescription) { pbstrDescription = $"Rename '{_oldName}' to '{_newName}':"; return VSConstants.S_OK; }
            public int GetHelpContext(out string pbstrHelpContext) { pbstrHelpContext = ""; return VSConstants.S_OK; }
            public int GetRootChangesList(out object ppIUnknownPreviewChangesList) { ppIUnknownPreviewChangesList = _root; return VSConstants.S_OK; }
            public int GetTextViewDescription(out string pbstrTextViewDescription) { pbstrTextViewDescription = "Preview code changes:"; return VSConstants.S_OK; }
            public int GetTitle(out string pbstrTitle) { pbstrTitle = "Preview Changes - Rename"; return VSConstants.S_OK; }
            public int GetWarning(out string pbstrWarning, out int ppcwlWarningLevel) { pbstrWarning = null; ppcwlWarningLevel = 0; return VSConstants.S_OK; }

            /// <summary>Shows the file with its checked changes applied and selects the node's change.</summary>
            internal void Show(Node node, object textView)
            {
                if (!(textView is IVsTextView view) || _adapters == null)
                    return;
                var fileNode = node.Hit == null ? node : node.Parent;
                if (!_texts.TryGetValue(fileNode.FilePath, out var original))
                {
                    original = _readText?.Invoke(fileNode.FilePath) ?? "";
                    _texts[fileNode.FilePath] = original;
                }
                var text = new StringBuilder(original);
                int selectStart = -1;
                // from the end to the start, so that earlier positions stay valid
                foreach (var change in fileNode.Children.Items.Where(c => c.Checked).OrderByDescending(c => c.Hit.Start))
                {
                    if (change.Hit.Start + change.Hit.Length > text.Length)
                        continue;
                    text.Remove(change.Hit.Start, change.Hit.Length).Insert(change.Hit.Start, _newName);
                }
                if (node.Hit != null)
                {
                    int shift = fileNode.Children.Items.Where(c => c.Checked && c.Hit.Start < node.Hit.Start)
                        .Sum(c => _newName.Length - c.Hit.Length);
                    selectStart = node.Hit.Start + shift;
                }
                var lines = _adapters.CreateVsTextBufferAdapter(_services) as IVsTextLines;
                if (lines == null)
                    return;
                var content = text.ToString();
                lines.InitializeContent(content, content.Length);
                view.SetBuffer(lines);
                if (selectStart >= 0 && lines.GetLineIndexOfPosition(selectStart, out var line, out var column) == VSConstants.S_OK)
                {
                    var length = node.Checked ? _newName.Length : node.Hit.Length;
                    view.SetSelection(line, column, line, column + length);
                    view.CenterLines(line, 1);
                }
            }
        }

        /// <summary>The interop interfaces do not inherit: the list is both a tree list and a preview list.</summary>
        private sealed class NodeList : IVsPreviewChangesList, IVsLiteTreeList
        {
            private readonly Engine _engine;
            internal readonly List<Node> Items;
            private uint _version;

            internal NodeList(Engine engine, List<Node> items)
            {
                _engine = engine;
                Items = items;
            }

            public int GetFlags(out uint pFlags) { pFlags = 0; return VSConstants.S_OK; }
            public int GetItemCount(out uint pCount) { pCount = (uint)Items.Count; return VSConstants.S_OK; }
            public int GetExpandable(uint index, out int pfExpandable)
            {
                pfExpandable = Items[(int)index].Children != null ? 1 : 0;
                return VSConstants.S_OK;
            }
            public int GetExpandedList(uint index, out int pfCanRecurse, out IVsLiteTreeList pptlNode)
            {
                pfCanRecurse = 0;
                pptlNode = Items[(int)index].Children;
                return pptlNode == null ? VSConstants.E_FAIL : VSConstants.S_OK;
            }
            public int LocateExpandedList(IVsLiteTreeList expandedList, out uint iIndex)
            {
                iIndex = (uint)Items.FindIndex(n => ReferenceEquals(n.Children, expandedList));
                return (int)iIndex >= 0 ? VSConstants.S_OK : VSConstants.E_FAIL;
            }
            public int OnClose(VSTREECLOSEACTIONS[] ptca) { return VSConstants.S_OK; }
            public int GetText(uint index, VSTREETEXTOPTIONS tto, out string ppszText) { ppszText = Items[(int)index].Text; return VSConstants.S_OK; }
            public int GetTipText(uint index, VSTREETOOLTIPTYPE eTipType, out string ppszText) { ppszText = Items[(int)index].FilePath; return VSConstants.S_OK; }
            public int GetDisplayData(uint index, VSTREEDISPLAYDATA[] pData)
            {
                // check box: state image 1 = unchecked, 2 = checked
                pData[0].StateMask = (uint)_VSTREEDISPLAYSTATE.TDS_STATEIMAGEMASK;
                pData[0].State = (uint)(Items[(int)index].Checked ? 2 : 1) << 12;
                return VSConstants.S_OK;
            }
            public int UpdateCounter(out uint pCurUpdate, out uint pgrfChanges)
            {
                pCurUpdate = _version;
                pgrfChanges = (uint)_VSTREEITEMCHANGESMASK.TCT_ITEMPROPSCHANGED;
                return VSConstants.S_OK;
            }
            public int GetListChanges(ref uint pcChanges, VSTREELISTITEMCHANGE[] prgListChanges) { return VSConstants.E_NOTIMPL; }
            public int ToggleState(uint index, out uint ptscr)
            {
                var node = Items[(int)index];
                node.Checked = !node.Checked;
                if (node.Children != null)
                {
                    foreach (var child in node.Children.Items)
                        child.Checked = node.Checked;
                    node.Children._version++;
                }
                else if (node.Parent != null)
                {
                    node.Parent.Checked = node.Parent.Children.Items.Any(c => c.Checked);
                }
                _version++;
                ptscr = (uint)_VSTREESTATECHANGEREFRESH.TSCR_ENTIRE;
                return VSConstants.S_OK;
            }
            public int OnRequestSource(uint index, object pIUnknownTextView)
            {
                _engine.Show(Items[(int)index], pIUnknownTextView);
                return VSConstants.S_OK;
            }
        }
    }
}
