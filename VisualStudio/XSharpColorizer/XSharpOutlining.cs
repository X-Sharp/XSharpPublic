//------------------------------------------------------------------------------
// <copyright file="XSharpOutlining.cs" company="Company">
//     Copyright (c) Company.  All rights reserved.
// </copyright>
//------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Outlining;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text;

using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Text.Classification;
using XSharpColorizer;
using XSharpModel;

namespace XSharpColorizer
{

    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(IOutliningRegionTag))]
    [ContentType("XSharp")]
    internal sealed class XSharpOutliningTaggerProvider : ITaggerProvider
    {
        [Import]
        IClassifierAggregatorService aggregator = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            //create a single tagger for each buffer.
            Func<ITagger<T>> outliner = delegate () { return new XSharpOutliningTagger(buffer, aggregator, ClassificationRegistry) as ITagger<T>; };
            return buffer.Properties.GetOrCreateSingletonProperty<ITagger<T>>(outliner);
        }
    }





    internal sealed class XSharpOutliningTagger : ITagger<IOutliningRegionTag>
    {

        //string startHide = "[";     //the characters that start the outlining region
        //string endHide = "]";       //the characters that end the outlining region
        string ellipsis = "...";    //the characters that are displayed when the region is collapsed
        string hoverText = "hover text"; //the contents of the tooltip for the collapsed span
        ITextBuffer buffer;
        //ITextSnapshot snapshot;
        //List<Region> regions;
        private readonly IClassifier classifier;
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;
        //private XSharpTagger xsTagger;

        public XSharpOutliningTagger(ITextBuffer buffer, IClassifierAggregatorService AggregatorFactory, IClassificationTypeRegistryService registry)
        {
            this.buffer = buffer;
            //this.snapshot = buffer.CurrentSnapshot;
            //
            //xsTagger = new XSharpTagger(registry);
            //xsTagger.Parse(this.snapshot);
            //
            //this.buffer.Changed += OnBufferChanged;

            //this.classifier = AggregatorFactory.GetClassifier(buffer);
            xsharpRegionStartType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStopType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
        }


        public IEnumerable<ITagSpan<IOutliningRegionTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            //yield break;
            if (spans.Count == 0)
            {
                yield break;
            }
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = XSharpClassifier.GetColorizer(buffer);
            if (xsClassifier != null)
            {
                //
                ITextSnapshot snapshot = buffer.CurrentSnapshot;
                SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                IList<ClassificationSpan> classifications = xsClassifier.tagsRegion;
                //
                SnapshotSpan fullSpan = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive);
                int startLineNumber = fullSpan.Start.GetContainingLine().LineNumber;
                int endLineNumber = fullSpan.End.GetContainingLine().LineNumber;
                //
                Stack<ClassificationSpan> startStack = new Stack<ClassificationSpan>();
                // Now, let's have a look at all the Classifications we have in the document
                foreach (var tag in classifications)
                {
                    // Is it a Region ?
                    if (tag.ClassificationType.IsOfType(this.xsharpRegionStartType.Classification))
                    {
                        startStack.Push(tag);
                    }
                    else if (tag.ClassificationType.IsOfType(this.xsharpRegionStopType.Classification) && (startStack.Count > 0))
                    {
                        //
                        var startTag = startStack.Pop();
                        var startLine = startTag.Span.Start.GetContainingLine();
                        var endLine = tag.Span.End.GetContainingLine();
                        //
                        if (startLine.LineNumber <= endLineNumber && endLine.LineNumber >= startLineNumber)
                        {
                            SnapshotSpan sSpan = new SnapshotSpan(startLine.Start, endLine.End);
                            hoverText = sSpan.GetText();
                            //
                            sSpan = new SnapshotSpan(startLine.Start, startLine.End);
                            String lineText = sSpan.GetText();
                            // XSHARP : Temporary Solution - Remove Region marking
                            //yield break;
                            ////the region starts at the beginning of the entity, and goes until the *end* of the line that ends.
                            yield return new TagSpan<IOutliningRegionTag>(
                                new SnapshotSpan(startLine.End, endLine.End),
                                new OutliningRegionTag(false, true, ellipsis, hoverText));
                        }
                    }
                }
            }
            else
            {
                yield break;
            }
        }

    }
}



