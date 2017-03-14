using System.IO;
using System.Threading;
using Microsoft.VisualStudio.Language.Intellisense;

namespace XSharp.Project
{
    /*
    internal static class PeekHelpers
    {
        internal static IDocumentPeekResult CreateDocumentPeekResult(string filePath, LinePositionSpan identifierLocation, LinePositionSpan entityOfInterestSpan, IPeekResultFactory peekResultFactory)
        {
            var fileName = Path.GetFileName(filePath);
            var label = string.Format("{0} - ({1}, {2})", fileName, identifierLocation.Start.Line + 1, identifierLocation.Start.Character + 1);

            var displayInfo = new PeekResultDisplayInfo(label: label, labelTooltip: filePath, title: fileName, titleTooltip: filePath);

            return CreateDocumentPeekResult(
                filePath,
                identifierLocation,
                entityOfInterestSpan,
                displayInfo,
                peekResultFactory,
                isReadOnly: false);
        }

        internal static IDocumentPeekResult CreateDocumentPeekResult(string filePath, LinePositionSpan identifierLocation, LinePositionSpan entityOfInterestSpan, PeekResultDisplayInfo displayInfo, IPeekResultFactory peekResultFactory, bool isReadOnly)
        {
            return peekResultFactory.Create(
                displayInfo,
                filePath: filePath,
                startLine: entityOfInterestSpan.Start.Line,
                startIndex: entityOfInterestSpan.Start.Character,
                endLine: entityOfInterestSpan.End.Line,
                endIndex: entityOfInterestSpan.End.Character,
                idLine: identifierLocation.Start.Line,
                idIndex: identifierLocation.Start.Character,
                isReadOnly: isReadOnly);
        }
    }
    */
}