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
        ITextDocumentFactoryService factory = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            //create a single tagger for each buffer.
            if (!factory.IsXSharpDocument(buffer))
                return null;
            ITagger<T> outliner = buffer.Properties.GetOrCreateSingletonProperty<ITagger<T>>(
                () =>  new XSharpOutliningTagger(buffer, aggregator, ClassificationRegistry) as ITagger<T>);
            return outliner;

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
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;
        private XFile _file;
        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        public XSharpOutliningTagger(ITextBuffer buffer, IClassifierAggregatorService AggregatorFactory, IClassificationTypeRegistryService registry)
        {
            this.buffer = buffer;
            if (buffer.Properties.ContainsProperty(typeof(XFile)))
            {
                _file =  buffer.GetFile();
            }

            if (! buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                // should not happen we checked before we created the class
                return;    
            }
            this.buffer.Properties.AddProperty(typeof(XSharpOutliningTagger), this);
            xsharpRegionStartType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStopType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
        }


        public void Update ()
        {
            if (TagsChanged != null)
            {
                TagsChanged.Invoke(this, new SnapshotSpanEventArgs(new SnapshotSpan(buffer.CurrentSnapshot, 0,
                        buffer.CurrentSnapshot.Length)));
            }

        }
        public IEnumerable<ITagSpan<IOutliningRegionTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            //yield break;
            if (spans.Count == 0)
            {
                yield break;
            }
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }

            if (xsClassifier != null)
            {
                //
                ITextSnapshot snapshot = xsClassifier.Snapshot;
                SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                var classifications = xsClassifier.GetRegionTags();
                
                SnapshotSpan fullSpan = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive);
                int startLineNumber = fullSpan.Start.GetContainingLine().LineNumber;
                int endLineNumber = fullSpan.End.GetContainingLine().LineNumber;
                //
                Stack<ClassificationSpan> startStack = new Stack<ClassificationSpan>();
                // convert classifications to an array so there will be no crash when the classifications are changed
                // in another thread.
                
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
                        if ( startTag.Span.Start < tag.Span.End && 
                            startLine.LineNumber <= endLineNumber 
                            && endLine.LineNumber >= startLineNumber)
                        {
                            SnapshotSpan sSpan;
                            try
                            {
                                sSpan = new SnapshotSpan(startLine.Start, endLine.End);
                            }
                            catch (Exception e)
                            {
                                System.Diagnostics.Debug.WriteLine("Incorrect span " + e.Message);
                                sSpan = new SnapshotSpan(startLine.Start, startLine.Start);
                            }
                            hoverText = sSpan.GetText();
                            if (hoverText.Length > 1024)
                            {
                                hoverText = hoverText.Substring(0, 1024)+"\r\n......";
                            }
                            //
                            sSpan = new SnapshotSpan(startLine.Start, startLine.End);
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



