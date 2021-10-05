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

        public XSharpPeekItemSource(ITextBuffer textBuffer, IPeekResultFactory peekResultFactory, XFile file)
        {
            _textBuffer = textBuffer;
            _peekResultFactory = peekResultFactory;
            _file = file;
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
                var triggerPoint = tp.Value;

                // LookUp for the BaseType, reading the TokenList (From left to right)
                var location = _textBuffer.FindLocation(triggerPoint);
                if (location == null)
                    return;

                CompletionState state;
                var tokens = _textBuffer.GetTokens();
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream, out state);
                var result = new List<IXSymbol>();
                result.AddRange( XSharpLookup.RetrieveElement(location, tokenList, state, out var notProcessed, true));
                //
                if (result.Count > 0 && result[0] is XSourceSymbol symbol)
                {
                    peekableItems.Add(new XSharpDefinitionPeekItem(symbol, _peekResultFactory));
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
