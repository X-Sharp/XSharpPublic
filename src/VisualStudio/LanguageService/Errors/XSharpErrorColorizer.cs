#if NOTUSED
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService
{
    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(IErrorTag))]
    [ContentType(XSharpConstants.LanguageName)]
    internal sealed class XSharpErrorTaggerProvider : ITaggerProvider
    {
        [Import]
        private ITextDocumentFactoryService factory = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            if (!factory.IsXSharpDocument(buffer))
            {
                return null;
            }

            return buffer.Properties.GetOrCreateSingletonProperty<ITagger<T>>(
                () => new XSharpErrorTagger(buffer) as ITagger<T>);
        }
    }

    internal sealed class XSharpErrorTagger : ITagger<IErrorTag>
    {
        private readonly ITextBuffer _buffer;
        private readonly IErrorTag _errorTag;
        private XFile _file;
        private XSharpClassifier _classifier;

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        public XSharpErrorTagger(ITextBuffer buffer)
        {
            _buffer = buffer;
            _file = buffer.GetFile();
            _errorTag = new ErrorTag(PredefinedErrorTypeNames.SyntaxError);
            _buffer.Changed += BufferChanged;

            _classifier = buffer.GetClassifier();
            if (_classifier != null)
            {
                _classifier.ClassificationChanged += ClassifierChanged;
            }
        }

        private void BufferChanged(object sender, TextContentChangedEventArgs e)
        {
            if (_classifier == null)
            {
                _classifier = _buffer.GetClassifier();
                if (_classifier != null)
                {
                    _classifier.ClassificationChanged += ClassifierChanged;
                }
            }

            RaiseTagsChanged(_buffer.CurrentSnapshot);
        }

        private void ClassifierChanged(object sender, ClassificationChangedEventArgs e)
        {
            RaiseTagsChanged(_buffer.CurrentSnapshot);
        }

        private void RaiseTagsChanged(ITextSnapshot snapshot)
        {
            if (snapshot == null)
            {
                return;
            }

            TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(new SnapshotSpan(snapshot, 0, snapshot.Length)));
        }

        public IEnumerable<ITagSpan<IErrorTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count == 0)
            {
                yield break;
            }

            _file = _file ?? _buffer.GetFile();
            if (_file?.Project?.ProjectNode == null)
            {
                yield break;
            }

            List<IXErrorPosition> errors = _file.Project.ProjectNode.GetIntellisenseErrors(_file.FullPath);
            if (errors == null || errors.Count == 0)
            {
                yield break;
            }

            var snapshot = spans[0].Snapshot;
            foreach (var error in errors)
            {
                if (!TryCreateErrorSpan(snapshot, error, out var errorSpan))
                {
                    continue;
                }

                if (spans.Any(requestedSpan => requestedSpan.IntersectsWith(errorSpan)))
                {
                    yield return new TagSpan<IErrorTag>(errorSpan, _errorTag);
                }
            }
        }

        private static bool TryCreateErrorSpan(ITextSnapshot snapshot, IXErrorPosition error, out SnapshotSpan span)
        {
            span = default;
            if (snapshot == null || error == null || snapshot.LineCount == 0)
            {
                return false;
            }

            int lineNumber = Math.Max((int)error.Line - 1, 0);
            if (lineNumber >= snapshot.LineCount)
            {
                return false;
            }

            var line = snapshot.GetLineFromLineNumber(lineNumber);
            if (line.Length == 0)
            {
                return false;
            }

            int start = line.Start.Position + Math.Max((int)error.Column - 1, 0);
            if (start >= line.End.Position)
            {
                start = line.End.Position - 1;
            }

            int length = Math.Max((int)error.Length, 1);
            int end = Math.Min(start + length, line.End.Position);
            if (end <= start)
            {
                end = Math.Min(start + 1, line.End.Position);
                if (end <= start)
                {
                    return false;
                }
            }

            span = new SnapshotSpan(snapshot, Span.FromBounds(start, end));
            return true;
        }
    }
}
#endif
