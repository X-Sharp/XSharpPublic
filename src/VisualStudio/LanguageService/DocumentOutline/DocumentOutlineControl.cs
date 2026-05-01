// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// WPF UserControl that renders the Document Outline tree.
    /// It shows types and their members from the active X# source file and
    /// lets the user navigate by clicking a node.
    /// </summary>
    internal sealed class DocumentOutlineControl : UserControl
    {
        // -----------------------------------------------------------------------
        // Controls
        // -----------------------------------------------------------------------
        private readonly TreeView _treeView;
        private readonly StackPanel _toolPanel;
        private readonly Button _btnSortByName;
        private readonly Button _btnCollapseAll;

        // -----------------------------------------------------------------------
        // State
        // -----------------------------------------------------------------------
        private XFile _file;
        private bool _navigating;    // reentrancy guard
        private bool _sortByName;    // false = sort by document order (default)

        // -----------------------------------------------------------------------
        // Image source cache: maps glyph index → ImageSource
        // -----------------------------------------------------------------------
        private static readonly Dictionary<int, ImageSource> _glyphCache = new Dictionary<int, ImageSource>();
        private static System.Drawing.ImageList _imageList;

        static DocumentOutlineControl()
        {
            _imageList = new System.Drawing.ImageList
            {
                ImageSize = new System.Drawing.Size(16, 16),
                TransparentColor = System.Drawing.Color.FromArgb(255, 0, 255)
            };
            try
            {
                Stream stream = typeof(Microsoft.VisualStudio.Package.LanguageService)
                                    .Assembly
                                    .GetManifestResourceStream("Resources.completionset.bmp");
                if (stream != null)
                    _imageList.Images.AddStrip(new System.Drawing.Bitmap(stream));
            }
            catch (Exception)
            {
                // If images cannot be loaded the tree will still work without icons.
            }
        }

        // -----------------------------------------------------------------------
        // Construction
        // -----------------------------------------------------------------------
        public DocumentOutlineControl()
        {
            // -- Toolbar --
            _btnSortByName = new Button
            {
                Content = "#↓",
                Width = 32,
                Height = 24,
                Margin = new Thickness(2, 2, 2, 2),
                ToolTip = "Toggle sort: document order / alphabetical",
                Padding = new Thickness(2)
            };
            _btnSortByName.Click += OnSortToggle;

            _btnCollapseAll = new Button
            {
                Content = "−",
                Width = 32,
                Height = 24,
                Margin = new Thickness(0, 2, 2, 2),
                ToolTip = "Collapse all",
                Padding = new Thickness(2)
            };
            _btnCollapseAll.Click += OnCollapseAll;

            _toolPanel = new StackPanel
            {
                Orientation = Orientation.Horizontal,
                Height = 28
            };
            _toolPanel.Children.Add(_btnSortByName);
            _toolPanel.Children.Add(_btnCollapseAll);

            // -- Tree --
            _treeView = new TreeView();
            _treeView.SelectedItemChanged += OnNodeSelected;

            // -- Layout --
            var grid = new Grid();
            grid.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            grid.RowDefinitions.Add(new RowDefinition { Height = new GridLength(1, GridUnitType.Star) });

            Grid.SetRow(_toolPanel, 0);
            Grid.SetRow(_treeView, 1);
            grid.Children.Add(_toolPanel);
            grid.Children.Add(_treeView);

            Content = grid;
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

            bool hasNonGlobalTypes = false;
            foreach (var t in types)
                if (!t.IsGlobal) { hasNonGlobalTypes = true; break; }

            // When there are no explicit types, show all global members under a file root node.
            if (!hasNonGlobalTypes && _file.GlobalType != null)
            {
                var globalMembers = GetFilteredMembers(_file.GlobalType);
                if (globalMembers.Count > 0)
                {
                    var globalNode = new OutlineTreeNode(_file.GlobalType, _file.Name, GetGlyphImage(_file.GlobalType.Glyph));
                    foreach (var m in globalMembers)
                        globalNode.Items.Add(CreateNode(m));
                    _treeView.Items.Add(globalNode);
                    globalNode.IsExpanded = true;
                }
                return;
            }

            foreach (var type in types)
            {
                if (type.IsGlobal)
                    continue;

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
            return new OutlineTreeNode(entity, label, GetGlyphImage(entity.Glyph));
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
        // Glyph → ImageSource conversion
        // -----------------------------------------------------------------------
        private static ImageSource GetGlyphImage(int glyphIndex)
        {
            if (_imageList == null || glyphIndex < 0 || glyphIndex >= _imageList.Images.Count)
                return null;

            if (_glyphCache.TryGetValue(glyphIndex, out var cached))
                return cached;

            var bmp = _imageList.Images[glyphIndex] as System.Drawing.Bitmap;
            if (bmp == null)
                return null;

            ImageSource source;
            try
            {
                var handle = bmp.GetHbitmap();
                try
                {
                    source = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(
                        handle,
                        IntPtr.Zero,
                        Int32Rect.Empty,
                        BitmapSizeOptions.FromEmptyOptions());
                    source.Freeze();
                }
                finally
                {
                    NativeMethods.DeleteObject(handle);
                }
            }
            catch (Exception)
            {
                return null;
            }

            _glyphCache[glyphIndex] = source;
            return source;
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
                    node.Entity.OpenEditor();
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
            _btnSortByName.Content = _sortByName ? "A↓" : "#↓";
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

        public OutlineTreeNode(XSourceEntity entity, string label, ImageSource icon)
        {
            Entity = entity;

            var panel = new StackPanel { Orientation = Orientation.Horizontal };
            if (icon != null)
            {
                panel.Children.Add(new System.Windows.Controls.Image
                {
                    Source = icon,
                    Width = 16,
                    Height = 16,
                    Margin = new Thickness(0, 0, 4, 0),
                    VerticalAlignment = VerticalAlignment.Center
                });
            }
            panel.Children.Add(new TextBlock
            {
                Text = label,
                VerticalAlignment = VerticalAlignment.Center
            });

            Header = panel;
        }
    }

    // ---------------------------------------------------------------------------
    // P/Invoke helper for bitmap conversion
    // ---------------------------------------------------------------------------
    internal static class NativeMethods
    {
        [System.Runtime.InteropServices.DllImport("gdi32.dll")]
        [return: System.Runtime.InteropServices.MarshalAs(System.Runtime.InteropServices.UnmanagedType.Bool)]
        internal static extern bool DeleteObject(IntPtr hObject);
    }
}
