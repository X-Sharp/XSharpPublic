using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
using Microsoft.VisualStudio.Text.Tagging;

namespace XSharp.Project
{

    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType("XSharp")]
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
            string fileName = this.GetDocumentFileName(this.view.TextBuffer);
            file = XSharpModel.XSolution.FindFullPath(fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
                return;
            }
            //
            layer = view.GetAdornmentLayer("XSharpErrorColorizer");
            // Disable the Adorment painting
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
            List<Tuple<int, int>> errors = file.Project.ProjectNode.GetIntellisenseErrorPos(file.FullPath);
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
                Tuple<int, int> error = errors.Find(t => t.Item1 == lineNumber);
                if (error != null)
                    this.CreateVisuals(line, error.Item2);
            }

        }

        private void CreateVisuals(ITextViewLine line, int errorColumn)
        {
            IWpfTextViewLineCollection textViewLines = view.TextViewLines;
            int tabSize = this.view.Options.GetTabSize();
            var fullSpan = new SnapshotSpan(line.Snapshot, Span.FromBounds(line.Start, line.End));
            var snapLine = fullSpan.Start.GetContainingLine();
            string text = snapLine.GetText();
            int lineNumber = fullSpan.Start.GetContainingLine().LineNumber;
            int offset = this.GetLineOffsetFromColumn(text, errorColumn, tabSize);
            //
            var span = new SnapshotSpan(line.Snapshot, Span.FromBounds(line.Start+offset, line.Start+offset + 1));
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

        private String GetDocumentFileName(ITextBuffer TextBuffer)
        {
            String fileName = "";
            ITextDocument textDoc;
            var rc = TextBuffer.Properties.TryGetProperty<ITextDocument>(typeof(ITextDocument), out textDoc);
            if (rc == true)
            {
                fileName = textDoc.FilePath;
            }
            return fileName;
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
