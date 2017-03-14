using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;

namespace XSharp.Project
{
    class XSharpResultSource : IPeekResultSource
    {
        private readonly XSharpDefinitionPeekItem peekableItem;

        public XSharpResultSource(XSharpDefinitionPeekItem peekableItem)
        {
            this.peekableItem = peekableItem;
        }

        public void FindResults(string relationshipName, IPeekResultCollection resultCollection, CancellationToken cancellationToken, IFindPeekResultsCallback callback)
        {
            if (relationshipName != PredefinedPeekRelationships.Definitions.Name)
            {
                return;
            }

            using (var displayInfo = new PeekResultDisplayInfo(label: peekableItem._fileName, labelTooltip: peekableItem._fileName, title: Path.GetFileName(peekableItem._fileName), titleTooltip: peekableItem._fileName))
            {
                var result = peekableItem._peekResultFactory.Create
                (
                    displayInfo,
                    peekableItem._fileName,
                    new Span(peekableItem._point, 1),
                    peekableItem._point,
                    false
                );

                resultCollection.Add(result);
                callback.ReportProgress(1);
            }
        }

    }
}