//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using XSharp.Support;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>The grey text of one hint, with its tooltip and (type hints) the double-click edit.</summary>
    internal sealed class InlayHintAdornment : Border
    {
        private readonly InlayHint _hint;
        private readonly ITextSnapshot _snapshot;
        private readonly ITextView _view;
        private readonly XProject _project;

        internal InlayHintAdornment(InlayHint hint, ITextSnapshot snapshot, IWpfTextView view, IEditorFormatMap formatMap, XProject project)
        {
            _hint = hint;
            _snapshot = snapshot;
            _view = view;
            _project = project;
            var properties = formatMap.GetProperties(InlayHintFormat.FormatName);
            var text = new TextBlock
            {
                Text = hint.Text,
                Foreground = properties[EditorFormatDefinition.ForegroundBrushId] as Brush ?? Brushes.Gray,
            };
            var line = view.FormattedLineSource?.DefaultTextProperties;
            if (line != null)
            {
                text.FontFamily = line.Typeface.FontFamily;
                text.FontSize = line.FontRenderingEmSize * 0.9;
            }
            Child = text;
            Background = properties[EditorFormatDefinition.BackgroundBrushId] as Brush ?? Brushes.Transparent;
            CornerRadius = new CornerRadius(2);
            Padding = new Thickness(1, 0, 1, 0);
            VerticalAlignment = VerticalAlignment.Center;
            ToolTip = hint.Text;
            ToolTipOpening += OnToolTipOpening;
            if (hint.Edit != null)
            {
                Cursor = Cursors.Hand;
                MouseLeftButtonDown += OnMouseDown;
            }
        }

        private void OnToolTipOpening(object sender, ToolTipEventArgs e)
        {
            try
            {
                ToolTip = _hint.Kind == InlayHintKind.Parameter ? ParameterTip() : TypeTip();
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Inlay hints: tooltip");
            }
        }

        private string ParameterTip()
        {
            var parameter = _hint.Parameter;
            var text = new StringBuilder();
            text.Append(parameter.Name);
            if (!string.IsNullOrEmpty(parameter.TypeName))
                text.Append(" AS ").Append(parameter.TypeName);
            var names = new List<string>();
            var descriptions = new List<string>();
            if (XSharpXMLDocMember.GetMemberParameters(_hint.Member, _project, names, descriptions))
            {
                var index = names.FindIndex(n => string.Equals(n, parameter.Name, StringComparison.OrdinalIgnoreCase));
                if (index >= 0 && index < descriptions.Count && !string.IsNullOrWhiteSpace(descriptions[index]))
                    text.AppendLine().Append(descriptions[index].Trim());
            }
            text.AppendLine().Append(_hint.Member.Prototype);
            return text.ToString();
        }

        private string TypeTip()
        {
            var type = _hint.Type;
            if (type == null)
                return _hint.TypeName;
            var text = new StringBuilder(type.FullName);
            var summary = XSharpXMLDocMember.GetTypeSummary(type, _project, out _, out _);
            if (!string.IsNullOrWhiteSpace(summary))
                text.AppendLine().Append(summary.Trim());
            return text.ToString();
        }

        private void OnMouseDown(object sender, MouseButtonEventArgs e)
        {
            if (e.ClickCount != 2)
                return;
            e.Handled = true;
            try
            {
                var buffer = _snapshot.TextBuffer;
                var current = buffer.CurrentSnapshot;
                using (var edit = buffer.CreateEdit())
                {
                    foreach (var change in _hint.Edit.Changes)
                    {
                        var span = new SnapshotSpan(_snapshot, change.Start, change.Length).TranslateTo(current, SpanTrackingMode.EdgeExclusive);
                        edit.Replace(span.Span, change.Text);
                    }
                    edit.Apply();
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Inlay hints: double-click edit");
            }
        }
    }
}
