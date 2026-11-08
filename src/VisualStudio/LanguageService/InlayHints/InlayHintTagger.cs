//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.InlayHints
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [TagType(typeof(IntraTextAdornmentTag))]
    internal sealed class InlayHintTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal ITextDocumentFactoryService DocumentFactory { get; set; }

        [Import]
        internal IEditorFormatMapService FormatMapService { get; set; }

        [Import]
        internal IToolTipService ToolTipService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (!(textView is IWpfTextView view) || buffer != textView.TextBuffer || !DocumentFactory.IsXSharpDocument(buffer))
                return null;
            return view.Properties.GetOrCreateSingletonProperty(typeof(InlayHintTagger),
                () => new InlayHintTagger(view, buffer, FormatMapService.GetEditorFormatMap(view), ToolTipService)) as ITagger<T>;
        }
    }

    /// <summary>
    /// Shows the hints of the last computed snapshot, translated to the snapshot the editor asks for. The
    /// computation runs in <see cref="InlayHintQueue"/>; this class only reads its result.
    /// </summary>
    internal sealed class InlayHintTagger : ITagger<IntraTextAdornmentTag>
    {
        private readonly IWpfTextView _view;
        private readonly ITextBuffer _buffer;
        private readonly IEditorFormatMap _formatMap;
        private readonly IToolTipService _toolTips;
        private readonly XSharpClassifier _classifier;
        private readonly Dictionary<InlayHint, UIElement> _adornments = new Dictionary<InlayHint, UIElement>();
        private InlayHintSet _current;
        private bool _closed;

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal XFile File => _buffer.GetFile();

        internal InlayHintTagger(IWpfTextView view, ITextBuffer buffer, IEditorFormatMap formatMap, IToolTipService toolTips)
        {
            _view = view;
            _buffer = buffer;
            _formatMap = formatMap;
            _toolTips = toolTips;
            _classifier = buffer.GetClassifier();
            if (_classifier != null)
                _classifier.ParseCompleted += OnParseCompleted;
            InlayHintSettings.Changed += OnSettingsChanged;
            InlayHintSettings.HeldChanged += OnHeldChanged;
            _formatMap.FormatMappingChanged += OnFormatMappingChanged;
            _view.Closed += OnClosed;
            InlayHintTriggers.Register(this);
            // the first parse may be done before the view opened
            var snapshot = buffer.GetDocument()?.SnapShot;
            if (snapshot != null)
                Request(snapshot);
        }

        private void OnParseCompleted(ITextSnapshot snapshot) => Request(snapshot);

        private void OnSettingsChanged() => RequestDebounced();

        /// <summary>Alt+F1 pressed or released: hide or show at once, compute the missing kinds without delay.</summary>
        private void OnHeldChanged()
        {
            if (_closed)
                return;
            var set = _current;
            RaiseTagsChanged();
            if (set == null || (InlayHintSettings.ShowParameters && !set.Parameters) || (InlayHintSettings.ShowTypes && !set.Types))
            {
                var snapshot = _buffer.GetDocument()?.SnapShot;
                if (snapshot != null)
                    Request(snapshot);
            }
        }

        private void OnFormatMappingChanged(object sender, FormatItemsEventArgs e)
        {
            _adornments.Clear();
            RaiseTagsChanged();
        }

        private void OnClosed(object sender, EventArgs e)
        {
            _closed = true;
            if (_classifier != null)
                _classifier.ParseCompleted -= OnParseCompleted;
            InlayHintSettings.Changed -= OnSettingsChanged;
            InlayHintSettings.HeldChanged -= OnHeldChanged;
            _formatMap.FormatMappingChanged -= OnFormatMappingChanged;
            _view.Closed -= OnClosed;
            InlayHintTriggers.Unregister(this);
        }

        private void Request(ITextSnapshot snapshot)
        {
            if (_closed)
                return;
            InlayHintQueue.Instance.Request(this, ct => ComputeAsync(snapshot, ct));
        }

        /// <summary>Recompute the current snapshot after a quiet second (walker done, another file saved, options).</summary>
        internal void RequestDebounced()
        {
            if (_closed)
                return;
            InlayHintQueue.Instance.RequestDebounced(this, ct => ComputeAsync(_buffer.GetDocument()?.SnapShot, ct));
        }

        private async Task ComputeAsync(ITextSnapshot snapshot, CancellationToken ct)
        {
            if (snapshot == null || _closed)
                return;
            var parameters = InlayHintSettings.ShowParameters;
            var types = InlayHintSettings.ShowTypes;
            var set = parameters || types
                ? InlayHintComputer.Compute(_buffer.GetDocument(), File, snapshot, parameters, types, ct)
                : new InlayHintSet { Snapshot = snapshot, Hints = Array.Empty<InlayHint>() };
            if (set == null)
                return;
            set.Parameters = parameters;
            set.Types = types;
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(ct);
            // publish only the hints of the snapshot the document is at
            if (_closed || _buffer.GetDocument()?.SnapShot != set.Snapshot)
                return;
            _current = set;
            _adornments.Clear();
            RaiseTagsChanged();
        }

        private void RaiseTagsChanged()
        {
            var snapshot = _buffer.CurrentSnapshot;
            TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(new SnapshotSpan(snapshot, 0, snapshot.Length)));
        }

        public IEnumerable<ITagSpan<IntraTextAdornmentTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            var set = _current;
            if (set == null || spans.Count == 0 || set.Hints.Count == 0)
                yield break;
            var snapshot = spans[0].Snapshot;
            if (snapshot.TextBuffer != set.Snapshot.TextBuffer)
                yield break;
            var project = File?.Project;
            var parameters = InlayHintSettings.ShowParameters;
            var types = InlayHintSettings.ShowTypes;
            foreach (var hint in set.Hints)
            {
                // released Alt+F1: the kinds that are switched off disappear before the next computation
                if (!(hint.Kind == InlayHintKind.Parameter ? parameters : types))
                    continue;
                var point = new SnapshotPoint(set.Snapshot, hint.Position);
                var after = point.TranslateTo(snapshot, PointTrackingMode.Positive);
                // the text at the hint was edited: drop the hint until the next result
                if (after != point.TranslateTo(snapshot, PointTrackingMode.Negative))
                    continue;
                if (!Contains(spans, after))
                    continue;
                if (!_adornments.TryGetValue(hint, out var adornment))
                {
                    adornment = new InlayHintAdornment(hint, set.Snapshot, _view, _formatMap, project, _toolTips);
                    _adornments[hint] = adornment;
                }
                yield return new TagSpan<IntraTextAdornmentTag>(new SnapshotSpan(after, 0),
                    new IntraTextAdornmentTag(adornment, null, PositionAffinity.Successor));
            }
        }

        private static bool Contains(NormalizedSnapshotSpanCollection spans, SnapshotPoint point)
        {
            foreach (var span in spans)
            {
                if (span.Start <= point && point <= span.End)
                    return true;
            }
            return false;
        }
    }
}
