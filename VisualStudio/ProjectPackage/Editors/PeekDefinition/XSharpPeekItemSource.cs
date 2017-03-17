using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using LanguageService.SyntaxTree;
using XSharpLanguage;

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
            //
            var tp = session.GetTriggerPoint(_textBuffer.CurrentSnapshot);
            if (!tp.HasValue)
            {
                return;
            }
            //
            var triggerPoint = tp.Value;
            string fileName = EditorHelpers.GetDocumentFileName(session.TextView.TextBuffer);
            IToken stopToken;
            //
            List<String> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, triggerPoint.GetContainingLine().LineNumber, _textBuffer.CurrentSnapshot.GetText(), out stopToken, false);
            // Check if we can get the member where we are
            XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(triggerPoint.Position, fileName);
            XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(triggerPoint.Position, fileName);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            XSharpModel.XElement gotoElement;
            System.Reflection.MemberInfo dummyElement;
            XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(fileName, tokenList, member, stopToken, out gotoElement, out dummyElement);
            //
            if (gotoElement != null)
            {
                peekableItems.Add(new XSharpDefinitionPeekItem(gotoElement, _peekResultFactory));
            }
        }

        public void Dispose()
        {
        }
    }
}