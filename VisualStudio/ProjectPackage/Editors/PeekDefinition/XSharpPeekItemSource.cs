using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using LanguageService.SyntaxTree;
using XSharpLanguage;
using XSharpColorizer;
using XSharpModel;
namespace XSharp.Project
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
                IToken stopToken;
                //
                // Check if we can get the member where we are
                XMemberDefinition member = XSharpLanguage.XSharpTokenTools.FindMember(triggerPoint.GetContainingLine().LineNumber, _file);
                XTypeDefinition currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(triggerPoint.Position, _file);

                var lineNumber = triggerPoint.GetContainingLine().LineNumber;
                var snapshot = _textBuffer.CurrentSnapshot;
                List<String> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, lineNumber, snapshot, out stopToken, true, _file, false, member);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement gotoElement;
                String currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, stopToken, out gotoElement, snapshot, lineNumber, _file.Project.Dialect);
                //
                if ((gotoElement != null) && (gotoElement.IsSourceElement))
                {
                    peekableItems.Add(new XSharpDefinitionPeekItem(gotoElement.SourceElement, _peekResultFactory));
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpPeekItemSource.AugmentPeekSession failed : " );
                XSharpProjectPackage.Instance.DisplayException(ex);
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
