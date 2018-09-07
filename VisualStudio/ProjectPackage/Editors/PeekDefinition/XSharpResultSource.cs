using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Imaging.Interop;
using System.Diagnostics;

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
            try
            {
                if (relationshipName != PredefinedPeekRelationships.Definitions.Name)
                {
                    return;
                }
                var fileName = Path.GetFileName(this.peekableItem._gotoElement.File.FullPath);
                var label = this.peekableItem._gotoElement.Name;
                var title = string.Format("{0} - ({1}, {2})", fileName, this.peekableItem._gotoElement.Range.StartLine, this.peekableItem._gotoElement.Range.StartColumn);

                using (var displayInfo = new PeekResultDisplayInfo2(label: label, labelTooltip: this.peekableItem._gotoElement.File.FullPath, title: title, titleTooltip: this.peekableItem._gotoElement.File.FullPath, startIndexOfTokenInLabel: 0, lengthOfTokenInLabel: label.Length))
                {
                    var result = peekableItem._peekResultFactory.Create
                    (
                        displayInfo,
                        default(ImageMoniker),
                        this.peekableItem._gotoElement.File.FullPath,
                        this.peekableItem._gotoElement.Range.StartLine - 1,
                        this.peekableItem._gotoElement.Range.StartColumn - 1,
                        this.peekableItem._gotoElement.Range.EndLine - 1,
                        this.peekableItem._gotoElement.Range.EndColumn - 1,
                        this.peekableItem._gotoElement.Range.StartLine - 1,
                        this.peekableItem._gotoElement.Range.StartColumn - 1,
                        this.peekableItem._gotoElement.Range.EndLine - 1,
                        this.peekableItem._gotoElement.Range.EndColumn - 1,
                        false,
                        new Guid(XSharpConstants.EditorFactoryGuidString)
                    );

                    resultCollection.Add(result);
                    callback.ReportProgress(1);
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpResultSource.FindResults failed : " );
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
        }

    }
}