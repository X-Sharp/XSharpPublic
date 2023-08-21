using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Imaging.Interop;
using System.Diagnostics;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService
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
                if (string.IsNullOrEmpty(fileName))
                    return;
                var label = this.peekableItem._gotoElement.Name;
                var title = string.Format("{0} - ({1}, {2})", fileName, this.peekableItem._gotoElement.Range.StartLine, this.peekableItem._gotoElement.Range.StartColumn+1);

                using (var displayInfo = new PeekResultDisplayInfo2(label: label, labelTooltip: this.peekableItem._gotoElement.File.FullPath, title: title, titleTooltip: this.peekableItem._gotoElement.File.FullPath, startIndexOfTokenInLabel: 0, lengthOfTokenInLabel: label.Length))
                {
                    var range = this.peekableItem._gotoElement.Range;

                    var result = peekableItem._peekResultFactory.Create
                    (
                        displayInfo,
                        default,
                        this.peekableItem._gotoElement.File.FullPath,
                        range.StartLine ,
                        range.StartColumn ,
                        range.EndLine ,
                        range.EndColumn ,
                        range.StartLine ,
                        range.StartColumn ,
                        range.EndLine ,
                        range.EndColumn ,
                        false,
                        new Guid(XSharpConstants.EditorFactoryGuidString)
                    );

                    resultCollection.Add(result);
                    callback.ReportProgress(1);
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "XSharpResultSource.FindResults failed : ");
            }
        }

    }
}
