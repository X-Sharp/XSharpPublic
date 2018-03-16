using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using LanguageService.SyntaxTree;
using XSharpLanguage;
using System.Diagnostics;
using XSharpColorizer;

namespace XSharp.Project
{
    internal sealed class XSharpPeekItemSource : IPeekableItemSource
    {
        private readonly ITextBuffer _textBuffer;
        private readonly IPeekResultFactory _peekResultFactory;
        private XSharpModel.XFile _file;

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
                List<String> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, triggerPoint.GetContainingLine().LineNumber, _textBuffer.CurrentSnapshot.GetText(), out stopToken, false, _file, false);
                // Check if we can get the member where we are
                XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(triggerPoint.Position, _file);
                XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(triggerPoint.Position, _file);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement gotoElement;
                String currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, stopToken, out gotoElement, _textBuffer.CurrentSnapshot.GetText());
                //
                if ((gotoElement != null) && (gotoElement.XSharpElement != null))
                {
                    peekableItems.Add(new XSharpDefinitionPeekItem(gotoElement.XSharpElement, _peekResultFactory));
                }
            }
            catch (Exception ex)
            {
                Trace.WriteLine("XSharpPeekItemSource.AugmentPeekSession Exception : " + ex.Message);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }

        public void Dispose()
        {
        }
    }
}