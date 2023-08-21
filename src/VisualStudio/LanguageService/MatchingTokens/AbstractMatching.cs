//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using XSharpModel;
using XSharp.Settings;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text.Classification;

namespace XSharp.LanguageService
{

    // Tagger that matches keyword pairs

    internal abstract class AbstractMatchingTagger : ITagger<TextMarkerTag>
    {
        protected readonly ITextView _view;
        protected readonly ITextBuffer _buffer;
        protected readonly XDocument _document;
        protected readonly IBufferTagAggregatorFactoryService _aggregator;
        protected readonly ITagAggregator<IClassificationTag> _tagAggregator;
        protected SnapshotPoint? _currentChar;
        
        protected string _prefix;

        internal AbstractMatchingTagger(ITextView view, IBufferTagAggregatorFactoryService aggregator)
        {
            _view = view;
            _buffer = view.TextBuffer;
            _document = _buffer.GetDocument();
            _aggregator = aggregator;
            _tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_buffer);

            _view.Caret.PositionChanged += CaretPositionChanged;
            _view.LayoutChanged += ViewLayoutChanged;
            _view.Closed += ViewClosed;
        }

        private void ViewClosed(object sender, EventArgs e)
        {
            _view.Closed -= ViewClosed;
            _view.Caret.PositionChanged -= CaretPositionChanged;
            _view.LayoutChanged -= ViewLayoutChanged;
        }


        protected void WriteOutputMessage(string sMessage)
        {
            if (XSettings.EnableBraceMatchLog && XSettings.EnableLogging)
            {
                Logger.Information(_prefix + sMessage);
            }
        }

        private void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            if (e.NewSnapshot != e.OldSnapshot) //make sure that there has really been a change
            {
                UpdateAtCaretPosition(_view.Caret.Position);
            }
        }

        private void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPosition)
        {
            _currentChar = caretPosition.Point.GetPoint(_buffer, caretPosition.Affinity);

            if (_currentChar.HasValue)
            {
                SnapshotSpan snapshot = new SnapshotSpan(_buffer.CurrentSnapshot, 0, _buffer.CurrentSnapshot.Length);
                TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(snapshot));
            }
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        protected SnapshotSpan MakeSnapshotSpan(IToken token, ITextSnapshot snapshot)
        {
            return new SnapshotSpan(snapshot, token.StartIndex, token.StopIndex - token.StartIndex + 1);
        }

        internal static bool IsInActiveSpan(ClassificationSpan span)
        {
            if (XSolution.IsClosing)
            {
                return false;
            }
            if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.Comment))
            {
                return true;
            }
            else if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.String))
            {
                return true;
            }
            else if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.ExcludedCode))
            {
                return true;
            }
            return false;
        }
        protected bool matchesPosition(IToken token)
        {
            return token.StartIndex <= _currentChar && token.StopIndex >= _currentChar;
        }

        public abstract IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans);
        internal abstract TextMarkerTag Tag { get; }
    }
}

