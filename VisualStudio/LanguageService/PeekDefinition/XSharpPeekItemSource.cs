using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using LanguageService.SyntaxTree;
using XSharpLanguage;
using XSharp.LanguageService;
using XSharpModel;
namespace XSharp.LanguageService
{
    internal sealed class XSharpPeekItemSource : IPeekableItemSource
    {
        private readonly ITextBuffer _textBuffer;
        private readonly IPeekResultFactory _peekResultFactory;
        private XFile _file;

        public XSharpPeekItemSource(ITextBuffer textBuffer, IPeekResultFactory peekResultFactory)
        {
            _textBuffer = textBuffer;
            _peekResultFactory = peekResultFactory;
            _file = textBuffer.GetFile();
        }

        public void AugmentPeekSession(IPeekSession session, IList<IPeekableItem> peekableItems)
        {
            try
            {
                XSharpModel.ModelWalker.Suspend();
                if (!string.Equals(session.RelationshipName, PredefinedPeekRelationships.Definitions.Name, StringComparison.OrdinalIgnoreCase))
                {
                    return;
                }
                //
                var tp = session.GetTriggerPoint(_textBuffer.CurrentSnapshot);
                if (!tp.HasValue)
                {
                    return;
                }
                //
                var triggerPoint = tp.Value;
                // Make sure we include the  closing ( or {
                var lineNumber = triggerPoint.GetContainingLine().LineNumber;
                var position = triggerPoint.Position ;
                //
                // Check if we can get the member where we are
                var member = XSharpLookup.FindMember(triggerPoint.GetContainingLine().LineNumber, _file);
                var currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, _file);

                var snapshot = _textBuffer.CurrentSnapshot;
                var tokens = _textBuffer.GetTokens();

                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement gotoElement;
                string currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                var location = new XSharpSearchLocation(member, snapshot) { LineNumber = lineNumber, Position = position, CurrentNamespace = currentNS};
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream);
                CompletionType cType = XSharpLookup.RetrieveType(location, tokenList, CompletionState.General, out gotoElement);
                //
                if ((gotoElement != null) && (gotoElement.IsSourceElement))
                {
                    peekableItems.Add(new XSharpDefinitionPeekItem(gotoElement.SourceElement, _peekResultFactory));
                }
            }
            catch (Exception ex)
            {
                XSettings.DisplayOutputMessage("XSharpPeekItemSource.AugmentPeekSession failed : " );
                XSettings.DisplayException(ex);
            }
            finally
            {
                ModelWalker.Resume();
            }
        }

        public void Dispose()
        {
        }
    }
}
