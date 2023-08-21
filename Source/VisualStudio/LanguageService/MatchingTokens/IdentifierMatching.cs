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

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class IdentifierMatchingTaggerProvider : IViewTaggerProvider
    {

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (XEditorSettings.DisableKeywordMatching)
                return null;
            if (textView == null || buffer == null)
                return null;

            return new IdentifierMatchingTagger(textView, BufferTagAggregatorFactoryService) as ITagger<T>;
        }
    }



    internal class IdentifierTag : TextMarkerTag
    {
        public IdentifierTag() : base(ColorizerConstants.HighLightIdentifierFormatDefinition)
        {
            
        }
    }
    internal class IdentifierMatchingTagger : AbstractMatchingTagger
    {
        private TextMarkerTag _tag = new IdentifierTag();
        internal override TextMarkerTag Tag => _tag;

        internal IdentifierMatchingTagger(ITextView view, IBufferTagAggregatorFactoryService aggregator): base(view, aggregator)
        {
            _prefix = "Identifier Matching: ";
        }
        public override IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            // Todo: the classifier is now marking open and close keywords with (invisible) classification
            // on another location the classifier is building regions based on Open/Close keywords.
            // During this process of building regions we can also 'remember' the open/close pairs
            // so we do not have to look for these in this code.

            DateTime oStart;
            TimeSpan timeSpan;
            bool caseSensitive = false;
            if (XEditorSettings.DisableHighLightWord || XDebuggerSettings.DebuggerIsRunning)
            {
                yield break;
            }

            if (_document == null || _document.Identifiers == null)
            {
                yield break;
            }
            var file = _buffer.GetFile();
            caseSensitive = file.Project.ParseOptions.CaseSensitive;

            oStart = DateTime.Now;

            if (spans.Count == 0 || _currentChar == null)   //there is no content in the buffer
                yield break;

            SnapshotPoint currentChar = _currentChar.Value;
            if (currentChar.Position == currentChar.Snapshot.Length)
                yield break;

            //hold on to a snapshot of the current character
            var ch = currentChar.GetChar();
            if (char.IsWhiteSpace(ch))
                yield break;

            // Verify that the document and spans have the right version 
            if (currentChar.Snapshot.Version != _document.SnapShot.Version || spans[0].Snapshot != currentChar.Snapshot)
                yield break;

            // don't do anything if the current SnapshotPoint is not initialized or at the end of the buffer
            if (currentChar.Position >= currentChar.Snapshot.Length )
                yield break;


            int currentLine = _currentChar.Value.GetContainingLine().LineNumber; 
            IList<ITagSpan<TextMarkerTag>> result = new List<ITagSpan<TextMarkerTag>>();
            try
            {
                // get all the blocks that surround the current position
                // find the current token
                var lineTokens = _document.GetTokensInLineAndFollowing(currentLine);
                foreach (var token in lineTokens)
                {
                    if (token.Type == XSharpLexer.ID && matchesPosition(token))
                    {
                        // find all matching identifiers
                        // when the classifier is "behind" then we may not find the token yet
                        if (!_document.Identifiers.ContainsKey(token.Text))
                            yield break;
                        var matchingTokens = _document.Identifiers[token.Text];
                        if (matchingTokens.Count == 1)
                            yield break;
                        var snapshot = _buffer.CurrentSnapshot;
                        
                        foreach (var token2 in matchingTokens)
                        {
                            // Only match when text is identical. So do NOT ignore case
                            if (!caseSensitive || token2.Text == token.Text)
                            {
                                var span = MakeSnapshotSpan(token2, snapshot);
                                var tagspan = new TagSpan<TextMarkerTag>(span, _tag);
                                result.Add(tagspan);
                            }
                        }
                        if (result.Count == 1)
                        {
                           yield break;
                        }
                    }
                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "IdentifierMatchingTagger.GetTags failed");
            }
            finally
            {
                timeSpan = DateTime.Now - oStart;
                WriteOutputMessage($"Finished Matching identifiers: {result.Count} tokens found");
                WriteOutputMessage("Finished Matching identifiers - total ms: " + timeSpan.TotalMilliseconds.ToString());
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

