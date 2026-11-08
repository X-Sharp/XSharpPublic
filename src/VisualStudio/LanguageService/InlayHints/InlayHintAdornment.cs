//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using Microsoft.VisualStudio.Core.Imaging;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
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
        private readonly IToolTipService _toolTips;
        private IToolTipPresenter _presenter;
        private DispatcherTimer _timer;

        internal InlayHintAdornment(InlayHint hint, ITextSnapshot snapshot, IWpfTextView view, IEditorFormatMap formatMap, XProject project, IToolTipService toolTips)
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
            Background = InlayHintColors.Background(properties[EditorFormatDefinition.BackgroundBrushId] as Brush);
            CornerRadius = new CornerRadius(2);
            Padding = new Thickness(1, 0, 1, 0);
            VerticalAlignment = VerticalAlignment.Center;
            _toolTips = toolTips;
            MouseEnter += OnMouseEnter;
            MouseLeave += OnMouseLeave;
            Unloaded += OnUnloaded;
            if (hint.Edit != null)
            {
                Cursor = Cursors.Hand;
                MouseLeftButtonDown += OnMouseDown;
            }
        }

        private void OnMouseEnter(object sender, MouseEventArgs e)
        {
            if (_toolTips == null)
                return;
            if (_timer == null)
            {
                _timer = new DispatcherTimer(DispatcherPriority.Normal, Dispatcher) { Interval = TimeSpan.FromMilliseconds(400) };
                _timer.Tick += (s, a) => { _timer.Stop(); ShowToolTip(); };
            }
            _timer.Start();
        }

        private void OnMouseLeave(object sender, MouseEventArgs e)
        {
            _timer?.Stop();
        }

        private void OnUnloaded(object sender, RoutedEventArgs e)
        {
            _timer?.Stop();
            _presenter?.Dismiss();
            _presenter = null;
        }

        /// <summary>The editor's own tooltip, as QuickInfo shows it: themed, with the classification colours.</summary>
        private void ShowToolTip()
        {
            try
            {
                if (!IsMouseOver || _view.IsClosed)
                    return;
                var content = _hint.Kind == InlayHintKind.Parameter ? ParameterTip() : TypeTip();
                if (content == null)
                    return;
                _presenter?.Dismiss();
                // the presenter closes when the mouse leaves the (empty) span, so it stays open while it is over the hint
                _presenter = _toolTips.CreatePresenter(_view, new ToolTipParameters(trackMouse: true, ignoreBufferChange: false, keepOpenFunc: () => IsMouseOver));
                var span = _snapshot.CreateTrackingSpan(new Span(_hint.Position, 0), SpanTrackingMode.EdgeInclusive);
                _presenter.StartOrUpdate(span, new object[] { content });
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Inlay hints: tooltip");
            }
        }

        private object ParameterTip()
        {
            var parameter = _hint.Parameter;
            if (parameter == null)
                return null;
            var lines = new List<object>
            {
                new ContainerElement(ContainerElementStyle.Wrapped,
                    new ImageElement(KnownMonikers.Parameter.ToImageId()),
                    new ClassifiedTextElement(new XSharpQuickInfoSource.QuickInfoVariable(parameter).WPFDescription))
            };
            var names = new List<string>();
            var descriptions = new List<string>();
            if (_hint.Member != null && XSharpXMLDocMember.GetMemberParameters(_hint.Member, _project, names, descriptions))
            {
                var index = names.FindIndex(n => string.Equals(n, parameter.Name, StringComparison.OrdinalIgnoreCase));
                if (index >= 0 && index < descriptions.Count && !string.IsNullOrWhiteSpace(descriptions[index]))
                    lines.Add(new ClassifiedTextElement(new ClassifiedTextRun(PredefinedClassificationTypeNames.NaturalLanguage, descriptions[index].Trim())));
            }
            return new ContainerElement(ContainerElementStyle.Stacked, lines);
        }

        private object TypeTip()
        {
            var type = _hint.Type;
            if (type == null)
                return new ClassifiedTextElement(new ClassifiedTextRun(PredefinedClassificationTypeNames.Identifier, _hint.TypeName));
            var analysis = new XTypeAnalysis(type);
            var lines = new List<object>
            {
                new ContainerElement(ContainerElementStyle.Wrapped,
                    new ImageElement(analysis.Image.ToImageId()),
                    new ClassifiedTextElement(analysis.WPFDescription))
            };
            var summary = XSharpXMLDocMember.GetTypeSummary(type, _project, out _, out _);
            if (!string.IsNullOrWhiteSpace(summary))
                lines.Add(new ClassifiedTextElement(new ClassifiedTextRun(PredefinedClassificationTypeNames.NaturalLanguage, summary.Trim())));
            return new ContainerElement(ContainerElementStyle.Stacked, lines);
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
