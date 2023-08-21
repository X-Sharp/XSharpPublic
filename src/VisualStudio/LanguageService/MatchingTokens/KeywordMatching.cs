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
using System.Linq;
using XSharpModel;
using XSharp.Settings;
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

    internal class KeywordMatchingTagger : AbstractMatchingTagger
    {
        private TextMarkerTag _tag = new KeyWordTag();
        internal override TextMarkerTag Tag => _tag;

        internal KeywordMatchingTagger(ITextView view, IBufferTagAggregatorFactoryService aggregator) : base(view, aggregator)
        {
            _prefix = "Keyword Matching: ";
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

        public override IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            // Todo: the classifier is now marking open and close keywords with (invisible) classification
            // on another location the classifier is building regions based on Open/Close keywords.
            // During this process of building regions we can also 'remember' the open/close pairs
            // so we do not have to look for these in this code.

            DateTime oStart;
            TimeSpan timeSpan;
            if (XEditorSettings.DisableKeywordMatching || XDebuggerSettings.DebuggerIsRunning)
            {
                yield break;
            }

            if (_document == null || _document.Blocks == null)
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
                    var ents = _document.Entities.Where(e => e.Range.StartLine <= currentLine && e.Range.EndLine >= currentLine); // && e.BlockTokens.Count > 1);
                    foundSpans = GetEntitySpans(ents);
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
                Logger.Exception(e, "KeywordMatchingTagger.GetTags failed");
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

