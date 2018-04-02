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
using Microsoft.VisualStudio.Text.Operations;
using System.Windows.Media;
using System.Collections.ObjectModel;
using XSharpLanguage;

// Based on https://docs.microsoft.com/en-US/visualstudio/extensibility/walkthrough-highlighting-text
// But we wil only hightlight Text in the current Members of our XSharp model.


namespace XSharp.Project.Editors.HighlightWord
{


    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class HighlightWordTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal ITextSearchService TextSearchService { get; set; }

        [Import]
        internal ITextStructureNavigatorSelectorService TextStructureNavigatorSelector { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableHighLightWord)
                return null;

            // Sorry, not the same buffer
            if (textView.TextBuffer != buffer)
                return null;

            ITextStructureNavigator textStructureNavigator =
                TextStructureNavigatorSelector.GetTextStructureNavigator(buffer);

            return new HighlightWordTagger(textView, buffer, TextSearchService, textStructureNavigator) as ITagger<T>;
        }

    }
    internal class HighlightWordTag : TextMarkerTag
    {

        public HighlightWordTag() : base("MarkerFormatDefinition/XSharpHighlightWordFormatDefinition") { }

    }

    [Export(typeof(EditorFormatDefinition))]
    [Name("MarkerFormatDefinition/XSharpHighlightWordFormatDefinition")]
    [UserVisible(true)]
    internal class HighlightWordFormatDefinition : MarkerFormatDefinition
    {
        public HighlightWordFormatDefinition()
        {
            this.BackgroundColor = Colors.LightGray;
            this.DisplayName = "XSharp Highlighted Word";
            this.ZOrder = 5;
        }

    }

    internal class HighlightWordTagger : ITagger<HighlightWordTag>
    {
        ITextView View { get; set; }
        ITextBuffer SourceBuffer { get; set; }
        ITextSelection Selection { get; set; }
        ITextSearchService TextSearchService { get; set; }
        ITextStructureNavigator TextStructureNavigator { get; set; }
        NormalizedSnapshotSpanCollection WordSpans { get; set; }
        SnapshotSpan? selectedWord { get; set; }
        SnapshotSpan? currentWord { get; set; }

        SnapshotPoint RequestedPoint { get; set; }
        object updateLock = new object();
        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        public HighlightWordTagger(ITextView view, ITextBuffer sourceBuffer, ITextSearchService textSearchService, ITextStructureNavigator textStructureNavigator)
        {
            this.View = view;
            this.Selection = view.Selection;
            this.SourceBuffer = sourceBuffer;
            this.TextSearchService = textSearchService;
            this.TextStructureNavigator = textStructureNavigator;
            //
            this.WordSpans = new NormalizedSnapshotSpanCollection();
            this.selectedWord = null;
            this.currentWord = null;
            this.View.Caret.PositionChanged += OnCaretPositionChanged;
            this.View.LayoutChanged += OnLayoutChanged;
            this.Selection.SelectionChanged += new EventHandler(this.OnSelectionChanged);

        }

        void OnLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            // Not the same snapshot, we should redraw
            if (e.NewSnapshot != e.OldSnapshot)
            {
                // Commented, don't see the benefits
                //UpdateAtCaretPosition(View.Caret.Position);
            }
        }

        void OnCaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            // Try to get the word, unless Clear
            SynchronousUpdate(new NormalizedSnapshotSpanCollection());
            return;
        }

        private void OnSelectionChanged(object sender, object e)
        {
            try
            {
                String selectedText = this.View.Selection.StreamSelectionSpan.GetText();
                if (!string.IsNullOrEmpty(selectedText) && !string.IsNullOrWhiteSpace(selectedText))
                {
                    // where are we
                    SnapshotPoint currentRequest = this.View.Selection.Start.Position;
                    List<SnapshotSpan> wordSpans = new List<SnapshotSpan>();
                    // Search for me please
                    TextExtent word = TextStructureNavigator.GetExtentOfWord(currentRequest);
                    bool foundWord = true;
                    //
                    if (!WordExtentIsValid(currentRequest, word))
                    {
                        //Same context ?
                        if (word.Span.Start != currentRequest
                             || currentRequest == currentRequest.GetContainingLine().Start
                             || char.IsWhiteSpace((currentRequest - 1).GetChar()))
                        {
                            foundWord = false;
                        }
                        else
                        {
                            // Move back, and start again
                            word = TextStructureNavigator.GetExtentOfWord(currentRequest - 1);

                            //If the word still isn't valid, we're done
                            if (!WordExtentIsValid(currentRequest, word))
                                foundWord = false;
                        }
                    }

                    if (!foundWord)
                    {
                        //If we couldn't find a word, clear out the existing markers
                        SynchronousUpdate(new NormalizedSnapshotSpanCollection());
                        return;
                    }
                    SnapshotSpan currentWord = word.Span;
                    selectedWord = this.View.Selection.StreamSelectionSpan.SnapshotSpan;


                    //If this is the current word, and the caret moved within a word, we're done.
                    if (!(selectedWord.HasValue && currentWord == selectedWord))
                    {
                        return;
                    }
                    //Find the new spans
                    FindData findData = new FindData(currentWord.GetText(), currentWord.Snapshot);
                    findData.FindOptions = FindOptions.WholeWord | FindOptions.MatchCase;
                    // Values are zero-based
                    SnapshotPoint point = View.Caret.Position.BufferPosition;
                    // Retrieve the XFile
                    XSharpModel.XFile xFile = this.View.TextBuffer.GetFile();
                    if (xFile != null)
                    {
                        // Now, retrieve the current member
                        XSharpModel.XTypeMember member = XSharpTokenTools.FindMemberAtPosition(point.Position, xFile);
                        if (member == null)
                            return;
                        // Ok, so we now have the "range" of the Member, and will only select text in THIS member
                        SnapshotSpan memberSpan = new SnapshotSpan(currentWord.Snapshot, member.Interval.Start, member.Interval.Width);
                        // Get all the corresponding Words
                        Collection<SnapshotSpan> allFound = TextSearchService.FindAll(findData);
                        Collection<SnapshotSpan> memberFound = new Collection<SnapshotSpan>();
                        foreach (SnapshotSpan ssp in allFound)
                        {
                            // Inside the Member ?
                            if (memberSpan.Contains(ssp))
                                memberFound.Add(ssp);
                        }
                        //
                        wordSpans.AddRange(memberFound);
                        // Show please 
                        SynchronousUpdate(new NormalizedSnapshotSpanCollection(wordSpans));
                    }
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("HighlightWordTag Exception: " + ex.Message);
            }
        }
        static bool WordExtentIsValid(SnapshotPoint currentRequest, TextExtent word)
        {
            return word.IsSignificant 
                && currentRequest.Snapshot.GetText(word.Span).Any(c => char.IsLetter(c));
        }
   
        void SynchronousUpdate(NormalizedSnapshotSpanCollection newSpans)
        {
            lock (updateLock)
            {
                //if (currentRequest != RequestedPoint)
                //    return;

                WordSpans = newSpans;

                TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0, SourceBuffer.CurrentSnapshot.Length)));
            }
        }

        public IEnumerable<ITagSpan<HighlightWordTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (selectedWord == null)
                yield break;

            // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
            // collection throughout
            SnapshotSpan currentWord = selectedWord.Value;
            NormalizedSnapshotSpanCollection wordSpans = WordSpans;

            if (spans.Count == 0 || WordSpans.Count == 0)
                yield break;

            // If the requested snapshot isn't the same as the one our words are on, translate our spans to the expected snapshot
            if (spans[0].Snapshot != wordSpans[0].Snapshot)
            {
                wordSpans = new NormalizedSnapshotSpanCollection(
                    wordSpans.Select(span => span.TranslateTo(spans[0].Snapshot, SpanTrackingMode.EdgeExclusive)));

                currentWord = currentWord.TranslateTo(spans[0].Snapshot, SpanTrackingMode.EdgeExclusive);
            }

            // First, yield back the word the cursor is under (if it overlaps)
            // Note that we'll yield back the same word again in the wordspans collection;
            // the duplication here is expected.
            if (spans.OverlapsWith(new NormalizedSnapshotSpanCollection(currentWord)))
                yield return new TagSpan<HighlightWordTag>(currentWord, new HighlightWordTag());

            // Second, yield all the other words in the file
            foreach (SnapshotSpan span in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans))
            {
                yield return new TagSpan<HighlightWordTag>(span, new HighlightWordTag());
            }
        }

    }
}
