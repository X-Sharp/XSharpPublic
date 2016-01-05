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
using Microsoft.VisualStudio.ComponentModelHost;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Text.Classification;
using XSharpColorizer;

namespace XSharpOutlining
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
        //string ellipsis = "...";    //the characters that are displayed when the region is collapsed
        string hoverText = "hover text"; //the contents of the tooltip for the collapsed span
        ITextBuffer buffer;
        ITextSnapshot snapshot;
        //List<Region> regions;
        private readonly IClassifier classifier;
        private IClassificationType xsharpRegionType;

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;


        public XSharpOutliningTagger(ITextBuffer buffer, IClassifierAggregatorService AggregatorFactory, IClassificationTypeRegistryService registry)
        {
            this.buffer = buffer;
            this.snapshot = buffer.CurrentSnapshot;
            //this.regions = new List<Region>();
            //this.ReParse();
            //this.buffer.Changed += BufferChanged;
            //
            this.classifier = AggregatorFactory.GetClassifier(buffer);
            this.xsharpRegionType = registry.GetClassificationType(Constants.XSharpRegionFormat);
        }

        public IEnumerable<ITagSpan<IOutliningRegionTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            //yield break;
            if (spans.Count == 0) 
            {
                yield break;
            }
            //
            //
            SnapshotSpan Span = new SnapshotSpan(this.snapshot, 0, this.snapshot.Length);
            IList<ClassificationSpan> classifications = this.classifier.GetClassificationSpans(Span);
            //
            SnapshotSpan entire = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(this.snapshot, SpanTrackingMode.EdgeExclusive);
            //
            foreach (var tag in classifications)
            {
                if ( tag.ClassificationType == this.xsharpRegionType)
                {
                    //
                    var startLine = tag.Span.Start.GetContainingLine();
                    var endLine = tag.Span.End.GetContainingLine();
                    //
                    SnapshotSpan sSpan = new SnapshotSpan(startLine.Start, endLine.End);
                    hoverText = sSpan.GetText();

                    // XSHARP : Temporary Solution - Remove Region marking
                    yield break;
                    ////the region starts at the beginning of the entity, and goes until the *end* of the line that ends.
                    //yield return new TagSpan<IOutliningRegionTag>(
                    //    new SnapshotSpan(startLine.End, endLine.End),
                    //    new OutliningRegionTag(false, false, ellipsis, hoverText));
                }
            }
            
        }

    }


}
