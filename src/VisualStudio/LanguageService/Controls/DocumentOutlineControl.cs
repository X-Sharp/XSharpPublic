// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// WPF UserControl that renders the Document Outline tree.
    /// It shows types and their members from the active X# source file and
    /// lets the user navigate by clicking a node.
    /// </summary>
     sealed partial class DocumentOutlineControl : UserControl
    {
        // -----------------------------------------------------------------------
        // State
        // -----------------------------------------------------------------------
        private XFile _file;
        private bool _navigating;    // reentrancy guard
        private bool _sortByName;    // false = sort by document order (default)

        // -----------------------------------------------------------------------
        // Construction
        // -----------------------------------------------------------------------
        public DocumentOutlineControl()
        {
            InitializeComponent();
        }

        // -----------------------------------------------------------------------
        // Public API
        // -----------------------------------------------------------------------

        /// <summary>
        /// Update the tree for a new source file.  Must be called from the UI thread.
        /// </summary>
        public void SetFile(XFile file)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_file != null)
                _file.ContentsChanged -= OnFileContentsChanged;

            _file = file;

            if (_file != null)
                _file.ContentsChanged += OnFileContentsChanged;

            RefreshTree();
        }

        /// <summary>
        /// Highlight the node that covers <paramref name="line"/> (0-based).
        /// Must be called from the UI thread.
        /// </summary>
        public void SelectNodeAtLine(int line)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_navigating)
                return;

            OutlineTreeNode best = null;
            int bestLength = int.MaxValue;

            foreach (OutlineTreeNode root in _treeView.Items)
                FindBestNode(root, line, ref best, ref bestLength);

            if (best != null && _treeView.SelectedItem != best)
            {
                _navigating = true;
                try
                {
                    best.IsSelected = true;
                    best.BringIntoView();
                }
                finally
                {
                    _navigating = false;
                }
            }
        }

        // -----------------------------------------------------------------------
        // Tree building
        // -----------------------------------------------------------------------
        private void RefreshTree()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            _treeView.Items.Clear();
            if (_file == null)
                return;

            var types = new List<XSourceTypeSymbol>(_file.TypeList.Values);

            if (_sortByName)
                types.Sort((a, b) => string.Compare(a.FullName, b.FullName, StringComparison.OrdinalIgnoreCase));
            else
                types.Sort((a, b) => a.Range.StartLine - b.Range.StartLine);


            foreach (var type in types)
            {

                var typeNode = CreateNode(type);
                AddMemberNodes(typeNode, type);
                _treeView.Items.Add(typeNode);
                typeNode.IsExpanded = true;
            }
        }

        private OutlineTreeNode CreateNode(XSourceEntity entity)
        {
            // ComboPrototype includes parameters and return type for methods/properties,
            // which gives a more informative label than just the name.
            string label = entity.ComboPrototype;
            if (string.IsNullOrEmpty(label))
                label = entity.Name;
            return new OutlineTreeNode(entity, label, GetMoniker(entity.Kind, entity.Visibility));
        }

        private void AddMemberNodes(OutlineTreeNode parentNode, XSourceTypeSymbol type)
        {
            var members = GetFilteredMembers(type);

            if (_sortByName)
                members.Sort((a, b) => string.Compare(a.Name, b.Name, StringComparison.OrdinalIgnoreCase));
            else
                members.Sort((a, b) => a.Range.StartLine - b.Range.StartLine);

            foreach (var member in members)
            {
                var node = CreateNode(member);
                if (member is XSourceTypeSymbol nested)
                    AddMemberNodes(node, nested);
                parentNode.Items.Add(node);
            }
        }

        private static List<XSourceEntity> GetFilteredMembers(XSourceTypeSymbol type)
        {
            var result = new List<XSourceEntity>();
            foreach (var m in type.XMembers)
            {
                if (m.Kind == Kind.Local || m.Kind == Kind.Parameter || m.Kind == Kind.DbField)
                    continue;
                result.Add(m);
            }
            foreach (XSourceTypeSymbol child in type.XChildren)
                result.Add(child);
            return result;
        }

        private static void FindBestNode(OutlineTreeNode node, int line, ref OutlineTreeNode best, ref int bestLength)
        {
            if (node.Entity != null)
            {
                var r = node.Entity.Range;
                if (r.StartLine <= line && r.EndLine >= line)
                {
                    int len = r.EndLine - r.StartLine;
                    if (len < bestLength)
                    {
                        best = node;
                        bestLength = len;
                    }
                }
            }
            foreach (OutlineTreeNode child in node.Items)
                FindBestNode(child, line, ref best, ref bestLength);
        }

        // -----------------------------------------------------------------------
        // Kind + Visibility -> ImageMoniker mapping
        // -----------------------------------------------------------------------

        /// <summary>
        /// Returns the <see cref="ImageMoniker"/> from <see cref="KnownMonikers"/> that best
        /// represents the given entity <paramref name="kind"/> and <paramref name="visibility"/>.
        /// </summary>
        private static ImageMoniker GetMoniker(Kind kind, Modifiers visibility)
        {
            switch (kind)
            {
                case Kind.Class:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ClassPublic, KnownMonikers.ClassProtected,
                        KnownMonikers.ClassPrivate, KnownMonikers.ClassInternal);

                case Kind.Structure:
                case Kind.VOStruct:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ValueTypePublic, KnownMonikers.ValueTypeProtected,
                        KnownMonikers.ValueTypePrivate, KnownMonikers.ValueTypeInternal);

                case Kind.Interface:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.InterfacePublic, KnownMonikers.InterfaceProtected,
                        KnownMonikers.InterfacePrivate, KnownMonikers.InterfaceInternal);

                case Kind.Delegate:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.DelegatePublic, KnownMonikers.DelegateProtected,
                        KnownMonikers.DelegatePrivate, KnownMonikers.DelegateInternal);

                case Kind.Enum:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.EnumerationPublic, KnownMonikers.EnumerationProtected,
                        KnownMonikers.EnumerationPrivate, KnownMonikers.EnumerationInternal);

                case Kind.EnumMember:
                    return KnownMonikers.EnumerationItemPublic;

                case Kind.Constructor:
                case Kind.Destructor:
                case Kind.Method:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.MethodPublic, KnownMonikers.MethodProtected,
                        KnownMonikers.MethodPrivate, KnownMonikers.MethodInternal);

                case Kind.Function:
                case Kind.Procedure:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                    // Global/module-level routines have no class visibility
                    return KnownMonikers.MethodPublic;

                case Kind.Access:
                case Kind.Assign:
                case Kind.Property:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.PropertyPublic, KnownMonikers.PropertyProtected,
                        KnownMonikers.PropertyPrivate, KnownMonikers.PropertyInternal);

                case Kind.Event:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.EventPublic, KnownMonikers.EventProtected,
                        KnownMonikers.EventPrivate, KnownMonikers.EventInternal);

                case Kind.Field:
                case Kind.VOGlobal:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.FieldPublic, KnownMonikers.FieldProtected,
                        KnownMonikers.FieldPrivate, KnownMonikers.FieldInternal);

                case Kind.Operator:
                    return KnownMonikers.Operator;

                case Kind.Namespace:
                    return KnownMonikers.Namespace;

                case Kind.Union:
                    return KnownMonikers.Union;

                case Kind.VODefine:
                case Kind.Define:
                case Kind.Undefine:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ConstantPublic, KnownMonikers.ConstantProtected,
                        KnownMonikers.ConstantPrivate, KnownMonikers.ConstantInternal);

                case Kind.Command:
                case Kind.XCommand:
                case Kind.YCommand:
                case Kind.Translate:
                case Kind.XTranslate:
                case Kind.YTranslate:
                    return KnownMonikers.MacroPublic;

                case Kind.Include:
                    return KnownMonikers.Library;

                case Kind.Keyword:
                    return KnownMonikers.IntellisenseKeyword;

                case Kind.Attribute:
                    return KnownMonikers.Attribute;

                case Kind.TypeParameter:
                    return KnownMonikers.Type;

                case Kind.Local:
                case Kind.Parameter:
                case Kind.MemVar:
                case Kind.DbField:
                case Kind.Undeclared:
                    return KnownMonikers.LocalVariable;

                default:
                    return KnownMonikers.Item;
            }
        }

        /// <summary>
        /// Picks one of four visibility-specific monikers based on <paramref name="visibility"/>.
        /// <see cref="Modifiers.ProtectedInternal"/> maps to the <paramref name="protectedMoniker"/>
        /// because that is the closest visual equivalent available in the catalog.
        /// Any other value (including <see cref="Modifiers.None"/> and <see cref="Modifiers.Public"/>)
        /// returns <paramref name="publicMoniker"/>.
        /// </summary>
        private static ImageMoniker VisibilityMoniker(Modifiers visibility,
            ImageMoniker publicMoniker, ImageMoniker protectedMoniker,
            ImageMoniker privateMoniker, ImageMoniker internalMoniker)
        {
            switch (visibility)
            {
                case Modifiers.Protected:
                case Modifiers.ProtectedInternal:
                    return protectedMoniker;
                case Modifiers.Private:
                    return privateMoniker;
                case Modifiers.Internal:
                    return internalMoniker;
                default:
                    return publicMoniker;
            }
        }

        // -----------------------------------------------------------------------
        // Event handlers
        // -----------------------------------------------------------------------
        private void OnFileContentsChanged()
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                RefreshTree();
            });
        }

        private void OnNodeSelected(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            if (_navigating)
                return;

            if (e.NewValue is OutlineTreeNode node && node.Entity != null)
            {
                _navigating = true;
                try
                {
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        var fileName = node.Entity.File?.FullPath;

                        var doc = await VS.Documents.GetActiveDocumentViewAsync();
                        if (doc != null)
                        {
                            var buffer = doc.TextBuffer;
                            var file = doc.TextBuffer.GetFile();
                            if (!string.Equals(file?.FullPath, fileName, StringComparison.OrdinalIgnoreCase))
                            {
                                node.Entity.OpenEditor();
                            }
                        }
                        doc = await VS.Documents.GetDocumentViewAsync(fileName);
                        if (doc != null)
                        {
                            var line = node.Entity.Range.StartLine;
                            var column = node.Entity.Range.StartColumn;
                            var view = doc.TextView;
                            TextSpan span = new TextSpan();
                            span.iStartLine = line;
                            span.iStartIndex = column;
                            span.iEndLine = line;
                            span.iEndIndex = column;
                            //
                            IVsTextView textView = null;
                            textView = await view.ToIVsTextViewAsync();

                            textView.SetCaretPos(span.iStartLine, span.iStartIndex);
                            textView.EnsureSpanVisible(span);
                        }

                    });
                }
                catch (Exception)
                {
                    // Silently ignore navigation errors
                }
                finally
                {
                    _navigating = false;
                }
            }
        }

        private void OnSortToggle(object sender, RoutedEventArgs e)
        {
            _sortByName = !_sortByName;
            _btnSortByName.Content = _sortByName ? "A\u2193" : "#\u2193";
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                RefreshTree();
            });
        }

        private void OnCollapseAll(object sender, RoutedEventArgs e)
        {
            foreach (OutlineTreeNode node in _treeView.Items)
                CollapseAll(node);
        }

        private static void CollapseAll(OutlineTreeNode node)
        {
            node.IsExpanded = false;
            foreach (OutlineTreeNode child in node.Items)
                CollapseAll(child);
        }

        private void OnExpandAll(object sender, RoutedEventArgs e)
        {
            foreach (OutlineTreeNode node in _treeView.Items)
                ExpandAll(node);
        }
        private static void ExpandAll(OutlineTreeNode node)
        {
            node.IsExpanded = true;
            foreach (OutlineTreeNode child in node.Items)
                ExpandAll(child);
        }


        // -----------------------------------------------------------------------
        // Disposal (UserControl does not implement IDisposable, but we clean up here)
        // -----------------------------------------------------------------------
        internal void Cleanup()
        {
            if (_file != null)
            {
                _file.ContentsChanged -= OnFileContentsChanged;
                _file = null;
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Helper: a WPF TreeViewItem that carries the entity and its image
    // ---------------------------------------------------------------------------
    internal sealed class OutlineTreeNode : TreeViewItem
    {
        public XSourceEntity Entity { get; }

        public OutlineTreeNode(XSourceEntity entity, string label, ImageMoniker moniker)
        {
            Entity = entity;

            var panel = new StackPanel { Orientation = Orientation.Horizontal };
            panel.Children.Add(new CrispImage
            {
                Moniker = moniker,
                Width = 16,
                Height = 16,
                Margin = new Thickness(0, 0, 4, 0),
                VerticalAlignment = VerticalAlignment.Center
            });
            panel.Children.Add(new TextBlock
            {
                Text = label,
                VerticalAlignment = VerticalAlignment.Center
            });

            Header = panel;
        }
    }
}

