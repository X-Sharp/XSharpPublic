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
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using System.Threading.Tasks;
using System.Threading;

namespace XSharp.Project.Editors.LightBulb
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [Name("Create stub Suggested Actions")]
    [ContentType(LanguageName)]
    internal class CreateStubSuggestedActionsSourceProvider : ISuggestedActionsSourceProvider
    {
        [Import(typeof(ITextStructureNavigatorSelectorService))]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            if (textBuffer == null && textView == null)
            {
                return null;
            }
            return new CreateStubSuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class CreateStubSuggestedActionsSource : ISuggestedActionsSource
    {
        private CreateStubSuggestedActionsSourceProvider m_factory;
        private ITextBuffer m_textBuffer;
        private ITextView m_textView;

        #pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public CreateStubSuggestedActionsSource(CreateStubSuggestedActionsSourceProvider createStubSuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer)
        {
            this.m_factory = createStubSuggestedActionsSourceProvider;
            this.m_textView = textView;
            this.m_textBuffer = textBuffer;
            //
        }

        private bool TryGetWordUnderCaret(out TextExtent wordExtent)
        {
            ITextCaret caret = m_textView.Caret;
            SnapshotPoint point;

            if (caret.Position.BufferPosition > 0)
            {
                point = caret.Position.BufferPosition - 1;
            }
            else
            {
                wordExtent = default(TextExtent);
                return false;
            }

            ITextStructureNavigator navigator = m_factory.NavigatorService.GetTextStructureNavigator(m_textBuffer);

            wordExtent = navigator.GetExtentOfWord(point);
            return true;
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            //return Task.Factory.StartNew(() =>
            //{
            //    TextExtent extent;
            //    if (TryGetWordUnderCaret(out extent))
            //    {
            //        // don't display the action if the extent has whitespace  
            //        return extent.IsSignificant;
            //    }
            //    return false;
            //});

            return Task.Factory.StartNew(() =>
            {
                return false;
            });
        }

        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            //TextExtent extent;
            //if (TryGetWordUnderCaret(out extent) && extent.IsSignificant)
            //{
            //    ITrackingSpan trackingSpan = range.Snapshot.CreateTrackingSpan(extent.Span, SpanTrackingMode.EdgeInclusive);
            //    var createStubAction = new CreateStubSuggestedAction(trackingSpan);
            //    return new SuggestedActionSet[] { new SuggestedActionSet(new ISuggestedAction[] { createStubAction }) };
            //}
            return Enumerable.Empty<SuggestedActionSet>();
        }


        public void Dispose()
        {
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            // This is a sample provider and doesn't participate in LightBulb telemetry  
            telemetryId = Guid.Empty;
            return false;
        }
    }


}
