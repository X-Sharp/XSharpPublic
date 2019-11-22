using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using static XSharp.Project.XSharpConstants;
using XSharpColorizer;
using Microsoft.VisualStudio.Text.Classification;
using System.Collections.Immutable;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XSharpLanguage;
using LanguageService.CodeAnalysis.XSharp;
using XSharpModel;

namespace XSharp.Project.Editors.BraceMatching
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class BraceMatchingTaggerProvider : IViewTaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableBraceMatching)
                return null;
            if (textView == null || buffer == null)
                return null;

            //provide highlighting only on the top-level buffer
            if (textView.TextBuffer != buffer)
                return null;

            return new BraceMatchingTagger(textView, buffer) as ITagger<T>;

        }
    }



    internal class BraceMatchingTagger : ITagger<TextMarkerTag>
    {
        ITextView View { get; set; }
        ITextBuffer SourceBuffer { get; set; }
        SnapshotPoint? CurrentChar { get; set; }
        static private Dictionary<char, char> m_braceList;

        static BraceMatchingTagger()
        {
            //here the keys are the open braces, and the values are the close braces
            m_braceList = new Dictionary<char, char>();
            m_braceList.Add('{', '}');
            m_braceList.Add('[', ']');
            m_braceList.Add('(', ')');
        }

        internal BraceMatchingTagger(ITextView view, ITextBuffer sourceBuffer)
        {
            this.View = view;
            this.SourceBuffer = sourceBuffer;
            this.CurrentChar = null;

            this.View.Caret.PositionChanged += CaretPositionChanged;
            this.View.LayoutChanged += ViewLayoutChanged;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            if (e.NewSnapshot != e.OldSnapshot) //make sure that there has really been a change
            {
                UpdateAtCaretPosition(View.Caret.Position);
            }
        }

        void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }
        void UpdateAtCaretPosition(CaretPosition caretPosition)
        {
            CurrentChar = caretPosition.Point.GetPoint(SourceBuffer, caretPosition.Affinity);

            if (!CurrentChar.HasValue)
                return;

            var tempEvent = TagsChanged;
            if (tempEvent != null)
                tempEvent(this, new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0,
                    SourceBuffer.CurrentSnapshot.Length)));
        }

        public IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count == 0)   //there is no content in the buffer
                yield break;

            if (CurrentChar == null || SourceBuffer == null)
                yield break;

            //don't do anything if the current SnapshotPoint is not initialized or at the end of the buffer
            if (!CurrentChar.HasValue || CurrentChar.Value.Position >= CurrentChar.Value.Snapshot.Length)
                yield break;


            //hold on to a snapshot of the current character
            SnapshotPoint ssp = CurrentChar.Value;

            //if the requested snapshot isn't the same as the one the brace is on, translate our spans to the expected snapshot
            if (spans[0].Snapshot != ssp.Snapshot)
            {
                ssp = ssp.TranslateTo(spans[0].Snapshot, PointTrackingMode.Positive);
            }

            //get the current char and the previous char
            char currentText = '\0';
            char lastText = '\0';
            SnapshotSpan pairSpan = new SnapshotSpan();
            SnapshotPoint lastChar = new SnapshotPoint();
            try
            {
                currentText = ssp.GetChar();
                lastChar = ssp == 0 ? ssp : ssp - 1; //if ssp is 0 (beginning of buffer), don't move it back
                lastText = lastChar.GetChar();
            }
            catch (Exception)
            {
                yield break;
            }
            // use the tokens stored in the buffer properties
            XSharpTokens xTokens = null;
            IList<IToken> tokens = null;
            int offset = 0;
            if ( SourceBuffer.Properties != null && SourceBuffer.Properties.ContainsProperty(typeof(XSharpTokens)))
            {
                xTokens = SourceBuffer.Properties.GetProperty<XSharpTokens>(typeof(XSharpTokens));
                if (xTokens == null || xTokens.TokenStream == null || xTokens.SnapShot == null)
                    yield break;

                tokens = xTokens.TokenStream.GetTokens();
                if (tokens == null)
                    yield break;
                if (xTokens.SnapShot.Version != ssp.Snapshot.Version)
                {
                    // get source from the start of the file until the current entity
                    var xfile = SourceBuffer.GetFile();
                    var member = XSharpTokenTools.FindMemberAtPosition(ssp.Position, xfile);
                    if (member != null)
                    {
                        try
                        {
                            var sourceWalker = new SourceWalker(xfile);
                            string text = ssp.Snapshot.GetText();
                            var stream = (BufferedTokenStream) sourceWalker.Lex(text);
                            tokens = stream.GetTokens();
                        }
                        catch (Exception e)
                        {
                            // if it crashes, that might be because the snapshot used for the Lex/Parse is no more
                            // so, we may have a too much difference
                            // we do not break but simply use the 'old' tokens
                            System.Diagnostics.Debug.WriteLine(e.Message);
                        }
                    }
                }
            }
            // First, try to match Simple chars
            if (m_braceList.ContainsKey(currentText))   //the key is the open brace
            {
                char closeChar;
                m_braceList.TryGetValue(currentText, out closeChar);
                if (BraceMatchingTagger.FindMatchingCloseChar(ssp, currentText, closeChar, out pairSpan, tokens, offset) == true)
                {
                    yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(ssp, 1), new TextMarkerTag("blue"));
                    yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
                }
            }
            else if (m_braceList.ContainsValue(lastText))    //the value is the close brace, which is the *previous* character
            {
                var open = from n in m_braceList
                           where n.Value.Equals(lastText)
                           select n.Key;
                if (BraceMatchingTagger.FindMatchingOpenChar(lastChar, (char)open.ElementAt<char>(0), lastText, out pairSpan, tokens, offset) == true)
                {
                    yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(lastChar, 1), new TextMarkerTag("blue"));
                    yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
                }
            }
            else
            {
                // Second, try to Match Keywords
                // Try to retrieve an already parsed list of Tags
                XSharpClassifier xsClassifier = null;
                if (SourceBuffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
                {
                    xsClassifier = SourceBuffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
                }

                if (xsClassifier != null)
                {

                    ITextSnapshot snapshot = xsClassifier.Snapshot;
                    if (snapshot.Version != ssp.Snapshot.Version)
                        yield break;
                    SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                    var classifications = xsClassifier.GetTags();
                    // We cannot use SortedList, because we may have several Classification that start at the same position
                    List<ClassificationSpan> sortedTags = new List<ClassificationSpan>();
                    foreach (var tag in classifications)
                    {
                        // Only keep the Brace matching Tags
                        if ((tag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat)) ||
                                (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceCloseFormat)))
                            sortedTags.Add(tag);
                    }
                    sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position)*1000 + string.Compare(a.ClassificationType.Classification, b.ClassificationType.Classification));
                    //
                    var tags = sortedTags.Where(x => ssp.Position >= x.Span.Start.Position && ssp.Position <= x.Span.End.Position);
                    foreach (var currentTag in tags)
                    {
                        var index = sortedTags.IndexOf(currentTag);
                        if (currentTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat))
                        {
                            if (FindMatchingCloseTag(sortedTags, index, snapshot, out pairSpan))
                            {
                                var span = currentTag.Span;
                                yield return new TagSpan<TextMarkerTag>(span, new TextMarkerTag("bracehighlight"));
                                yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("bracehighlight"));
                            }
                        }
                        else
                        {
                            if (FindMatchingOpenTag(sortedTags, index, snapshot, out pairSpan))
                            {
                                var span = currentTag.Span;
                                yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("bracehighlight"));
                                yield return new TagSpan<TextMarkerTag>(span, new TextMarkerTag("bracehighlight"));
                            }
                        }
                    }
                }
            }
        }

        private bool FindMatchingCloseTag(List<ClassificationSpan> sortedTags, int indexTag, ITextSnapshot snapshot, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(snapshot, 1, 1);
            try
            {
                ClassificationSpan currentTag = sortedTags[indexTag];
                ITextSnapshotLine line = currentTag.Span.Start.GetContainingLine();
                int lineNumber = line.LineNumber;
                int nested = 0;
                for (int i = indexTag + 1; i < sortedTags.Count; i++)
                {
                    var closeTag = sortedTags[i];
                    if (closeTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceCloseFormat))
                    {
                        nested--;
                        if (nested < 0)
                        {
                            pairSpan = new SnapshotSpan(snapshot, closeTag.Span);
                            return true;
                        }
                    }
                    else
                    {
                        nested++;
                    }
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
            //
            return false;
        }


        private bool FindMatchingOpenTag(List<ClassificationSpan> sortedTags, int indexTag, ITextSnapshot snapshot, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(snapshot, 1, 1);
            try
            {
                ClassificationSpan currentTag = sortedTags[indexTag];
                ITextSnapshotLine line = currentTag.Span.Start.GetContainingLine();
                int lineNumber = line.LineNumber;
                int nested = 0;
                for (int i = indexTag - 1; i >= 0; i--)
                {
                    var openTag = sortedTags[i];
                    if (openTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat))
                    {
                        nested--;
                        if (nested < 0)
                        {
                            pairSpan = new SnapshotSpan(snapshot, openTag.Span);
                            return true;
                        }
                    }
                    else
                    {
                        nested++;
                    }
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
            //
            return false;
        }


        private static int findtokeninList(IList<IToken> tokens, int startpos)
        {
            int min = 0;
            int max = tokens.Count - 1;
            bool found = false;
            IToken token = null;
            int tokenpos = -1;
            while (true)
            {
                tokenpos = (min + max) / 2;
                token = tokens[tokenpos];
                // check position
                if (token.StartIndex <= startpos && token.StopIndex >= startpos)
                {
                    found = true;
                    break;
                }
                else if (token.StopIndex < startpos)
                {
                    min = tokenpos;
                }
                else
                {
                    max = tokenpos;
                }
                if (min == max - 1)
                {
                    token = tokens[min];
                    if (token.StartIndex <= startpos && token.StopIndex >= startpos)
                    {
                        tokenpos = min;
                        found = true;
                        break;
                    }
                    token = tokens[max];
                    if (token.StartIndex <= startpos && token.StopIndex >= startpos)
                    {
                        tokenpos = max;
                        found = true;
                        break;
                    }

                    found = false;
                    break;
                }
            }
            if (found)
                return tokenpos;
            return -1;
        }

        private static bool FindMatchingCloseChar(SnapshotPoint startPoint, char open, char close, out SnapshotSpan pairSpan, IList<IToken> tokens, int offset)
        {
            pairSpan = new SnapshotSpan(startPoint.Snapshot, 1, 1);
            try
            {
                int startpos = startPoint.Position;
                if (tokens != null)
                {
                    int tokenpos = findtokeninList(tokens, startpos - offset);
                    if (tokenpos == -1)
                        return false;
                    IToken token = tokens[tokenpos];
                    // open/close braces are operators
                    if (!XSharpLexer.IsOperator(token.Type))
                        return false;
                    int openCount = 0;
                    for (int i = tokenpos + 1; i < tokens.Count; i++)
                    {
                        token = tokens[i];
                        if (XSharpLexer.IsOperator(token.Type))
                        {
                            string text = token.Text;
                            if (text[0] == open)
                                openCount++;
                            if (text[0] == close)
                            {
                                if (openCount > 0)
                                    openCount--;
                                else
                                {
                                    pairSpan = new SnapshotSpan(startPoint.Snapshot, token.StartIndex + offset, 1);
                                    return true;
                                }
                            }
                        }
                    }
                }
                return false;
            }
            catch (System.Exception ex)
            {
                System.Diagnostics.Debug.WriteLine(ex.Message);
            }

            return false;
        }

        private static bool FindMatchingOpenChar(SnapshotPoint startPoint, char open, char close, out SnapshotSpan pairSpan, IList<IToken> tokens, int offset)
        {
            pairSpan = new SnapshotSpan(startPoint, startPoint);
            try
            {
                int startpos = startPoint.Position;
                if (tokens != null)
                {
                    int tokenpos = findtokeninList(tokens, startpos - offset);
                    if (tokenpos == -1)
                        return false;
                    IToken token = tokens[tokenpos];
                    // open/close braces are operators
                    if (!XSharpLexer.IsOperator(token.Type))
                        return false;

                    int closeCount = 0;
                    for (int i = tokenpos - 1; i >= 0; i--)
                    {
                        token = tokens[i];
                        if (XSharpLexer.IsOperator(token.Type))
                        {
                            string text = token.Text;
                            if (text[0] == close)
                                closeCount++;
                            if (text[0] == open)
                            {
                                if (closeCount > 0)
                                    closeCount--;
                                else
                                {
                                    pairSpan = new SnapshotSpan(startPoint.Snapshot, token.StartIndex + offset, 1);
                                    return true;
                                }
                            }
                        }
                    }
                }
                return false;

            }
            catch (System.Exception ex)
            {
                System.Diagnostics.Debug.WriteLine(ex.Message);
            }
            return false;
        }


    }
}
