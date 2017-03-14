using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;

namespace XSharp.Project
{
    internal sealed class XSharpPeekItemSource : IPeekableItemSource
    {
        private readonly ITextBuffer _textBuffer;
        private readonly IPeekResultFactory _peekResultFactory;

        public XSharpPeekItemSource(ITextBuffer textBuffer, IPeekResultFactory peekResultFactory)
        {
            _textBuffer = textBuffer;
            _peekResultFactory = peekResultFactory;
        }

        public void AugmentPeekSession(IPeekSession session, IList<IPeekableItem> peekableItems)
        {
            if (!string.Equals(session.RelationshipName, PredefinedPeekRelationships.Definitions.Name, StringComparison.OrdinalIgnoreCase))
            {
                return;
            }

            var triggerPoint = session.GetTriggerPoint(_textBuffer.CurrentSnapshot);
            if (!triggerPoint.HasValue)
            {
                return;
            }


            string fileName = EditorHelpers.GetDocumentFileName(session.TextView.TextBuffer);

            var snapLine = triggerPoint.Value.GetContainingLine();

            //triggerPoint.Value.
            //snapLine.


            string text = snapLine.GetText();
         


            int lineNumber = triggerPoint.Value.GetContainingLine().LineNumber;
            //
            //int offset = this.GetLineOffsetFromColumn(text, errorColumn - 1, tabSize);


            //peekableItems.Add(new XSharpDefinitionPeekItem(triggerPoint.Value, _peekResultFactory, _textBuffer));
        }

        public void Dispose()
        {
        }
    }
}