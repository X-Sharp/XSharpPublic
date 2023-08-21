//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#if NOTUSED

// Note rewrite this using the [Export(typeof(ITaggerProvider))] and [TagType(typeof(IErrorTag))]
// See Roslyn src\EditorFeatures\Core\Implementation\Diagnostics\DiagnosticsSquiggleTaggerProvider.cs 
// Roslyn has an IDiagnosticService, which this tagger provider listens to.



using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using XSharpColorizer;
using Microsoft.VisualStudio.Text.Tagging;

namespace XSharp.LanguageService
{

    [Export(typeof(ITaggerProvider))]

    [ContentType(XSharpConstants.LanguageName)]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    internal sealed class XSharpErrorColorizerFactory : IWpfTextViewCreationListener
    {
        [Export(typeof(AdornmentLayerDefinition))]
        [Name("XSharpErrorColorizer")]
        [Order(After = PredefinedAdornmentLayers.Selection, Before = PredefinedAdornmentLayers.Text)]
        public AdornmentLayerDefinition editorAdornmentLayer = null;

        public void TextViewCreated(IWpfTextView textView)
        {
            new XSharpErrorColorizer(textView);
        }
    }

    public class XSharpErrorColorizer
    {
        IAdornmentLayer layer;
        IWpfTextView view;
        XFile file;

        public XSharpErrorColorizer(IWpfTextView view)
        {
            this.view = view;
            file = this.view.TextBuffer.GetFile();
            if (file == null)
            {
                // Uhh !??, Something went wrong
                return;
            }
            //
            layer = view.GetAdornmentLayer("XSharpErrorColorizer");
            // Disable the Adornment painting
            this.view.LayoutChanged += OnLayoutChanged;
            //this.view.TextBuffer.ChangedLowPriority += TextBuffer_ChangedLowPriority;
            this.view.Closed += View_Closed;
        }

        private void View_Closed(object sender, EventArgs e)
        {
            this.view.LayoutChanged -= OnLayoutChanged;
            //this.view.TextBuffer.ChangedLowPriority -= TextBuffer_ChangedLowPriority;
            this.view.Closed -= View_Closed;
        }

        private void TextBuffer_ChangedLowPriority(object sender, TextContentChangedEventArgs e)
        {
            //throw new NotImplementedException();
        }

        private void OnLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            layer.RemoveAllAdornments();
            // Retrieve the current list of Intellisense Error for the file
            if (file?.Project?.ProjectNode == null)
                return;
            List<IXErrorPosition> errors = file.Project.ProjectNode.GetIntellisenseErrorPos(file.FullPath);
            if ((errors == null) || (errors.Count == 0))
            {
                return;
            }
            //
            foreach( var line in view.TextViewLines)
            {
                var fullSpan = new SnapshotSpan(line.Snapshot, Span.FromBounds(line.Start, line.End));
                var snapLine = fullSpan.Start.GetContainingLine();
                int lineNumber = fullSpan.Start.GetContainingLine().LineNumber + 1;
                IXErrorPosition error = errors.Find(t => t.Line == lineNumber);
                if (error != null)
                    this.CreateVisuals(line, error.Column, error.Length);
            }

        }

        private void CreateVisuals(ITextViewLine line, int errorColumn, int Length)
        {
            IWpfTextViewLineCollection textViewLines = view.TextViewLines;
            int tabSize = this.view.Options.GetTabSize();
            var fullSpan = new SnapshotSpan(line.Snapshot, Span.FromBounds(line.Start, line.End));
            var snapLine = fullSpan.Start.GetContainingLine();
            string text = snapLine.GetText();
            int lineNumber = fullSpan.Start.GetContainingLine().LineNumber;
            int offset = this.GetLineOffsetFromColumn(text, errorColumn-1, tabSize);
            // If Error does span on several lines, the length might be negative as we are using Column to calculate length
            if (Length < 1)
                Length = 1;
            //
            var iMax = line.End.Position;
            var iEnd = line.Start.Position + offset + Length ;
            if (iEnd > iMax)
                iEnd = iMax;
            var start = line.Start + offset;
            var end = line.Start + (iEnd - line.Start.Position);
                
            var span = new SnapshotSpan(line.Snapshot, Span.FromBounds(start, end));
            //
            Geometry g = textViewLines.GetMarkerGeometry(span);
            if (g != null)
            {
                var border = new Border
                {
                    Width = g.Bounds.Width,
                    Height = g.Bounds.Height / 8,
                    BorderBrush = Brushes.Red,
                    BorderThickness = new Thickness(0.8, 0, 0.8, 0.8),
                };
                Canvas.SetLeft(border, g.Bounds.Left);
                Canvas.SetTop(border, g.Bounds.Bottom - g.Bounds.Height / 8);
                //
                layer.AddAdornment( span, null, border);
            }
        }

        // Code extract from StringExtensions.cs, in namespace Microsoft.CodeAnalysis.Shared.Extensions, from Roslyn code
        private int GetLineOffsetFromColumn(string line, int column, int tabSize)
        {
            var currentColumn = 0;

            for (int i = 0; i < line.Length; i++)
            {
                if (currentColumn >= column)
                {
                    return i;
                }

                if (line[i] == '\t')
                {
                    currentColumn += tabSize - (currentColumn % tabSize);
                }
                else
                {
                    currentColumn++;
                }
            }

            // We're asking for a column past the end of the line, so just go to the end.
            return line.Length;
        }
    }
}
#endif
