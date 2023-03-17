//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService
{

    // Tagger that matches keyword pairs

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class KeywordMatchingTaggerProvider : IViewTaggerProvider
    {

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (XEditorSettings.DisableKeywordMatching)
                return null;
            if (textView == null || buffer == null)
                return null;

            return new KeywordMatchingTagger(textView, BufferTagAggregatorFactoryService) as ITagger<T>;
        }
    }



    internal class KeyWordTag : TextMarkerTag
    {
        public KeyWordTag() : base(ColorizerConstants.KeyWordFormatDefinition)
        {
        }
    }
    internal class KeywordMatchingTagger : ITagger<TextMarkerTag>
    {
        private readonly ITextView _view;
        private readonly ITextBuffer _buffer;
        private readonly XDocument _document;
        private readonly IBufferTagAggregatorFactoryService _aggregator;
        private readonly ITagAggregator<IClassificationTag> _tagAggregator;
        private SnapshotPoint? _currentChar;
        private readonly TextMarkerTag _tag = new KeyWordTag();

        internal KeywordMatchingTagger(ITextView view, IBufferTagAggregatorFactoryService aggregator)
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


        void WriteOutputMessage(string sMessage)
        {
            if (XSettings.EnableBraceMatchLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("Keyword Matching: " + sMessage);
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

        private SnapshotSpan MakeSnapshotSpan(IToken token, ITextSnapshot snapshot)
        {
            return new SnapshotSpan(snapshot, token.StartIndex, token.StopIndex - token.StartIndex + 1);
        }

        private IList<SnapshotSpan> GetSpansForBlock(XSourceBlock b)
        {
            var spans = new List<SnapshotSpan>();
            var snapshot = _buffer.CurrentSnapshot;
            spans.Add(MakeSnapshotSpan(b.Token, snapshot));
            foreach (var child in b.Children)
            {
                spans.Add(MakeSnapshotSpan(child.Token, snapshot));
            }
            return spans;
        }

        private IList<SnapshotSpan> GetSpansForEntity(IToken token, XSourceEntity entity)
        {
            var spans = new List<SnapshotSpan>();
            var snapshot = _buffer.CurrentSnapshot;
            spans.Add(MakeSnapshotSpan(token, snapshot));
            foreach (var t in entity.BlockTokens)
            {
                spans.Add(MakeSnapshotSpan(t, snapshot));
            }
            return spans;
        }

        bool matchesPosition(IToken token)
        {
            return token.StartIndex <= _currentChar && token.StopIndex >= _currentChar;
        }

        IList<SnapshotSpan> GetBlockSpans(IEnumerable<XSourceBlock> blocks)
        {
            foreach (var block in blocks)
            {
                if (matchesPosition(block.Token))
                {
                    return GetSpansForBlock(block);
                }
                if (matchesPosition(block.Last.Token))
                {
                    return GetSpansForBlock(block);
                }
                foreach (var child in block.Children)
                {
                    if (matchesPosition(child.Token))
                    {
                        return GetSpansForBlock(block);
                    }
                }
            }
            return null;
        }
        IList<SnapshotSpan> GetEntitySpans(IEnumerable<XSourceEntity> entities)
        {
            // The blockTokens contains the start and end tokens for an entity
            // like CLASS .. END CLASS
            // When one of them is on the cursor location we mark them all
            foreach (var entity in entities)
            {
                // if the number of non modifier tokens <= 1 then we have no start and end tokens
                // but only PUBLIC METHOD or STATIC FUNCTION
                // in that case we do not want to match the keywords
                if (entity.BlockTokens.Count(t => t.Type == XSharpLexer.CLASS ||  !XSharpLexer.IsModifier(t.Type)) <= 1)
                    continue;

                foreach (var token in entity.BlockTokens)
                {
                    if (matchesPosition(token))
                    {
                        var spans = new List<SnapshotSpan>();
                        var snapshot = _buffer.CurrentSnapshot;
                        foreach (var t in entity.BlockTokens)
                        {
                            spans.Add(MakeSnapshotSpan(t, snapshot));
                        }
                        return spans;
                    }
                }
            }
            return null;
        }

        public IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            // Todo: the classifier is now marking open and close keywords with (invisible) classification
            // on another location the classifier is building regions based on Open/Close keywords.
            // During this process of building regions we can also 'remember' the open/close pairs
            // so we do not have to look for these in this code.

            DateTime oStart;
            TimeSpan timeSpan;
            if (XEditorSettings.DisableKeywordMatching)
            {
                yield break;
            }
            if (_document.Blocks == null)
            {
                yield break;
            }

            oStart = DateTime.Now;

            if (spans.Count == 0 || _currentChar == null)   //there is no content in the buffer
                yield break;

            //don't do anything if the current SnapshotPoint is not initialized or at the end of the buffer
            if (!_currentChar.HasValue || _currentChar.Value.Position >= _currentChar.Value.Snapshot.Length)
                yield break;

            SnapshotPoint currentChar = _currentChar.Value;
            if (spans[0].Snapshot != currentChar.Snapshot)
            {
                yield break;
            }
            //hold on to a snapshot of the current character
            var ch = currentChar.GetChar();
            if (char.IsWhiteSpace(ch))
                yield break;

            int currentLine = _currentChar.Value.GetContainingLine().LineNumber; 
            int tokenLine = currentLine + 1;// our tokens have 1 based line numbers
            IList<ITagSpan<TextMarkerTag>> result = new List<ITagSpan<TextMarkerTag>>();
            try
            {
                // get all the blocks that surround the current position
                var blocks = _document.Blocks.Where(b => b.Token.Line <= tokenLine && b.Last.Token.Line >= tokenLine);
                IList<SnapshotSpan> foundSpans = GetBlockSpans(blocks);
                if (foundSpans == null)
                {
                    var ents = _document.Entities.Where(e => e.Range.StartLine <= currentLine && e.Range.EndLine >= currentLine && e.BlockTokens.Count > 1);
                    foundSpans = GetEntitySpans(ents);
                }
                if (foundSpans == null || foundSpans.Count == 0)
                {
                    // when we are on a RETURN token then we match the return line with the current entity
                    var lineTokens = _document.GetTokensInLine(currentLine);
                    foreach (var token in lineTokens)
                    {
                        if (token.Type == XSharpLexer.RETURN)
                        {
                            if (matchesPosition(token))
                            {
                                // find current location
                                // add spans for the return keyword and the METHOD/ACCESS/FUNCTION
                                var ents = _document.Entities.Where(e => e is XSourceMemberSymbol
                                    && e.Interval.ContainsInclusive(_currentChar.Value.Position));
                                foreach (var entity in ents)
                                {
                                    foundSpans = GetSpansForEntity(token, entity);
                                    if (foundSpans != null)
                                        break;
                                }
                            }
                            break;
                        }
                    }

                }
                if (foundSpans != null)
                {
                    foreach (var span in foundSpans)
                    {
                        result.Add(new TagSpan<TextMarkerTag>(span, _tag));
                    }
                }
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "KeywordMatchingTagger.GetTags failed");
            }
            finally
            {
                timeSpan = DateTime.Now - oStart;
                WriteOutputMessage($"Finished Match Open/Close keywords: {result.Count} tokens found");
                WriteOutputMessage("Finished Match Open/Close keywords - total ms: " + timeSpan.TotalMilliseconds.ToString());
            }
            while (result.Count > 0)
            {
                var value = result[0];
                result.RemoveAt(0);
                yield return value;
            }
        }
    }
}

