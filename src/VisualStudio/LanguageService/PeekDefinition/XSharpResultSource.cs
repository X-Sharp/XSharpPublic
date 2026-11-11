using System;
using System.IO;
using System.Threading;
using Microsoft.VisualStudio.Language.Intellisense;
using XSharp.Support;
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
                var item = this.peekableItem;
                var fileName = Path.GetFileName(item.FilePath);
                if (string.IsNullOrEmpty(fileName))
                    return;
                var label = item.Label ?? "";
                var title = string.Format("{0} - ({1}, {2})", fileName, item.StartLine, item.StartColumn+1);

                using (var displayInfo = new PeekResultDisplayInfo2(label: label, labelTooltip: item.FilePath, title: title, titleTooltip: item.FilePath, startIndexOfTokenInLabel: 0, lengthOfTokenInLabel: label.Length))
                {
                    var result = item._peekResultFactory.Create
                    (
                        displayInfo,
                        default,
                        item.FilePath,
                        item.StartLine ,
                        item.StartColumn ,
                        item.EndLine ,
                        item.EndColumn ,
                        item.StartLine ,
                        item.StartColumn ,
                        item.EndLine ,
                        item.EndColumn ,
                        false,
                        item.EditorGuid
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
