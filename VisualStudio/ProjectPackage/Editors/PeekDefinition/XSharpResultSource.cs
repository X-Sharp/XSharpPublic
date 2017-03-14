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
            var fileName = Path.GetFileName(this.peekableItem._gotoElement.File.FullPath);
            
            var label = string.Format("{0} - ({1}, {2})", fileName, this.peekableItem._gotoElement.Range.StartLine, this.peekableItem._gotoElement.Range.StartColumn);

            using (var displayInfo = new PeekResultDisplayInfo(label: label, labelTooltip: this.peekableItem._gotoElement.File.FullPath, title: label, titleTooltip: this.peekableItem._gotoElement.File.FullPath))
            {
                var result = peekableItem._peekResultFactory.Create
                (
                    displayInfo,
                    this.peekableItem._gotoElement.File.FullPath,
                    this.peekableItem._gotoElement.Range.StartLine,
                    this.peekableItem._gotoElement.Range.StartColumn,
                    this.peekableItem._gotoElement.Range.EndLine,
                    this.peekableItem._gotoElement.Range.EndColumn,
                    this.peekableItem._gotoElement.Range.StartLine,
                    this.peekableItem._gotoElement.Range.StartColumn,
                    false
                );

                resultCollection.Add(result);
                callback.ReportProgress(1);
            }
        }

    }
}