using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;

namespace XSharpLanguage
{
    /*
    [Export(typeof(IQuickInfoSourceProvider))]
    [ContentType("XSharp")]
    [Name("XSharpQuickInfo")]
    class XSharpQuickInfoSourceProvider : IQuickInfoSourceProvider
    {

        [Import]
        IBufferTagAggregatorFactoryService aggService = null;

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
        {
            return new XSharpQuickInfoSource(textBuffer, aggService.CreateTagAggregator<XSharpTokenTag>(textBuffer));
        }
    }

    class XSharpQuickInfoSource : IQuickInfoSource
    {
        private ITagAggregator<XSharpTokenTag> _aggregator;
        private ITextBuffer _buffer;
        private bool _disposed = false;


        public XSharpQuickInfoSource(ITextBuffer buffer, ITagAggregator<XSharpTokenTag> aggregator)
        {
            _aggregator = aggregator;
            _buffer = buffer;
        }

        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> quickInfoContent, out ITrackingSpan applicableToSpan)
        {
            applicableToSpan = null;

            if (_disposed)
                throw new ObjectDisposedException("XSharpQuickInfoSource");

            var triggerPoint = (SnapshotPoint) session.GetTriggerPoint(_buffer.CurrentSnapshot);

            if (triggerPoint == null)
                return;
            
            foreach (IMappingTagSpan<XSharpTokenTag> curTag in _aggregator.GetTags(new SnapshotSpan(triggerPoint, triggerPoint)))
            {
                if (curTag.Tag.type == XSharpTokenTypes.Keyword)
                {
                    var tagSpan = curTag.Span.GetSpans(_buffer).First();
                    applicableToSpan = _buffer.CurrentSnapshot.CreateTrackingSpan(tagSpan, SpanTrackingMode.EdgeExclusive);
                    quickInfoContent.Add("Keyword Quick Info !");
                }
            }
        }

        public void Dispose()
        {
            _disposed = true;
        }
    }
*/
}

