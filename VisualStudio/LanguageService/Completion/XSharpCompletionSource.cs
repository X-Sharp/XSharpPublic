//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Text.Tagging;

#if ! ASYNCCOMPLETION
namespace XSharp.LanguageService
{
    partial class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;

        private XFile _file;
        private bool _showTabs;
        private bool _keywordsInAll;
        private readonly ITagAggregator<IClassificationTag> _tagAggregator;

        private XSharpDialect _dialect;
        private CompletionHelpers helpers ;
        internal static bool StringEquals(string lhs, string rhs)
        {
            if (string.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XSharpCompletionSource(XSharpCompletionSourceProvider provider, ITextBuffer buffer, IBufferTagAggregatorFactoryService aggregator, XFile file)
        {
            _provider = provider;
            _buffer = buffer;
            _file = file;
            var prj = _file.Project.ProjectNode;
            _dialect = _file.Project.Dialect;
            helpers = new CompletionHelpers(_dialect, provider.GlyphService, file, !prj.ParseOptions.CaseSensitive);
            this._tagAggregator = aggregator.CreateTagAggregator<IClassificationTag>(_buffer);

        }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging  )
            {
                XSettings.LogMessage(strMessage);
            }
        }
        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            WriteOutputMessage("-->> AugmentCompletionSessions");
            try
            {
                if (XEditorSettings.DisableCodeCompletion)
                    return;
                XSharpModel.ModelWalker.Suspend();
                if (_disposed)
                    throw new ObjectDisposedException("XSharpCompletionSource");
                _showTabs = XEditorSettings.CompletionListTabs;
                _keywordsInAll = XEditorSettings.KeywordsInAll;

                // Where does the StartSession has been triggered ?
                ITextSnapshot snapshot = _buffer.CurrentSnapshot;
                var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
                if (triggerPoint == null)
                    return;
                // What is the character were it starts ?
                var line = triggerPoint.GetContainingLine();

                // The "parameters" coming from CommandFilter
                uint cmd = 0;
                char typedChar = '\0';
                bool autoType = false;
                session.Properties.TryGetProperty(XsCompletionProperties.Command, out cmd);
                VSConstants.VSStd2KCmdID nCmdId = (VSConstants.VSStd2KCmdID)cmd;
                session.Properties.TryGetProperty(XsCompletionProperties.Char, out typedChar);
                session.Properties.TryGetProperty(XsCompletionProperties.AutoType, out autoType);
                bool showInstanceMembers = (typedChar == ':') || ((typedChar == '.') && _file.Project.ParseOptions.AllowDotForInstanceMembers);

                ////////////////////////////////////////////
                //
                SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
                SnapshotPoint caret = triggerPoint;
                var tags = _tagAggregator.GetTags(lineSpan);
                IMappingTagSpan<IClassificationTag> lastTag = null;
                foreach (var tag in tags)
                {
                    //tagList.Add(tag);
                    SnapshotPoint ptStart = tag.Span.Start.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    SnapshotPoint ptEnd = tag.Span.End.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    if ((ptStart != null) && (ptEnd != null))
                    {
                        if (caret.Position >= ptEnd)
                        {
                            lastTag = tag;
                        }
                    }
                }
                if (lastTag != null)
                {
                    var name = lastTag.Tag.ClassificationType.Classification.ToLower();
                    // No Intellisense in Comment
                    if (name.IsClassificationCommentOrString())
                        return;
                }
                ////////////////////////////////////////////
                SnapshotPoint start = triggerPoint;
                var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                //
                if (_file == null)
                {
                    // Uhh !??, Something went wrong
                    return;
                }
                // The Completion list we are building
                XCompletionList compList = new XCompletionList(_file);
                XCompletionList kwdList = new XCompletionList(_file);
                IXTypeSymbol type = null;
                if (session.Properties.ContainsProperty(XsCompletionProperties.Type))
                {
                    type = (IXTypeSymbol)session.Properties[XsCompletionProperties.Type];
                }
                // Start of Process
                string filterText = "";
                // Check if we can get the member where we are
                // Standard TokenList Creation (based on colon Selector )
                bool includeKeywords;
                session.Properties.TryGetProperty(XsCompletionProperties.IncludeKeywords, out includeKeywords);
                CompletionState state;
                var member = session.TextView.FindMember(triggerPoint);
                var location = session.TextView.TextBuffer.FindLocation(triggerPoint);
                if (location == null)
                    return;
                var tokenList = XSharpTokenTools.GetTokenList(location, out state, includeKeywords);
                var lastToken = tokenList.LastOrDefault();
                if (lastToken != null && (lastToken.Type == XSharpLexer.RPAREN || lastToken.Type == XSharpLexer.RCURLY))
                {
                    tokenList.RemoveAt(tokenList.Count - 1);
                    lastToken = tokenList.LastOrDefault();
                }
                var addKeywords = typedChar != '.' && typedChar != ':';


                // We might be here due to a COMPLETEWORD command, so we have no typedChar
                // but we "may" have a incomplete word like System.String.To
                // Try to Guess what TypedChar could be
                if (typedChar == '\0' && (autoType || nCmdId == VSConstants.VSStd2KCmdID.COMPLETEWORD))
                {
                    if (tokenList.Count > 0)
                    {
                        string extract = tokenList[tokenList.Count - 1].Text;
                        typedChar = extract[extract.Length - 1];
                        if ((typedChar != '.') && (typedChar != ':'))
                        {
                            if (tokenList.Count == 1)
                            {
                                //
                                filterText = tokenList[0].Text;
                                int dotPos = extract.LastIndexOf(".");
                                if (dotPos > -1)
                                {
                                    string startToken = extract.Substring(0, dotPos);
                                    filterText = extract.Substring(dotPos + 1);
                                    typedChar = '.';
                                    var token = (XSharpToken)tokenList[0];
                                    token.Text = startToken + ".";
                                }
                            }
                            else
                            {
                                // So, we get the last Token as a Filter
                                filterText = tokenList[tokenList.Count - 1].Text;
                            }
                            // Include the filter as the text to replace
                            start -= filterText.Length;
                            applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                        }
                    }
                }
                bool dotSelector = (typedChar == '.');

                int tokenType = XSharpLexer.UNRECOGNIZED;

                var symbol = XSharpLookup.RetrieveElement(location, tokenList, CompletionState.General, out var notProcessed).FirstOrDefault();
                var isInstance = true;
                if (symbol is IXTypeSymbol)
                {
                    isInstance = false;
                }
                else
                {
                    isInstance = true;
                }
                if (symbol != null)
                {

                    switch (lastToken.Type)
                    {
                        case XSharpLexer.DOT:
                            if (symbol.Kind == Kind.Namespace )
                            {
                                filterText = symbol.FullName + ".";
                            }
                            else if (symbol is IXTypeSymbol)
                            {
                                filterText = symbol.FullName + ".";
                            }
                            break;
                        case XSharpLexer.COLON:
                            break;
                        default:
                            filterText = symbol.Name;
                            symbol = null;
                            break;
                    }

                }
                var memberName = "";
                // Check for members, locals etc and convert the type of these to IXTypeSymbol
                if (symbol != null)
                {
                    if (symbol is IXTypeSymbol xtype)
                    {
                        type = xtype;
                    }
                    else if (symbol is IXMemberSymbol xmember)
                    {
                        var typeName = xmember.TypeName;
                        if (xmember is XSourceMemberSymbol sourcemem)
                        {
                            type = sourcemem.File.FindType(typeName);
                        }
                        else
                        {
                            type = location.FindType(typeName);
                        }
                        memberName = xmember.Name;
                    }
                    else if (symbol is IXVariableSymbol xvar)
                    {
                        var typeName = "";
                        if (xvar is XSourceUndeclaredVariableSymbol)
                        {
                            type = null;
                            if (typedChar == ':' || typedChar == '.')
                            {
                                state = CompletionState.None;
                                filterText = "";
                            }
                            else
                            {
                                if (state == CompletionState.None)
                                    state = CompletionState.General;
                                filterText = xvar.Name;
                            }
                        }
                        else if (xvar is XSourceVariableSymbol sourcevar)
                        {
                            typeName = sourcevar.TypeName;
                            if (sourcevar.ResolvedType != null)
                            {
                                type = sourcevar.ResolvedType;
                                typeName = type.FullName;
                            }
                            else
                            {
                                type = sourcevar.File.FindType(typeName);
                            }

                        }
                        else if (xvar is XPEParameterSymbol par)
                        {
                            typeName = par.TypeName;
                            type = location.FindType(typeName);
                        }
                        memberName = xvar.Name;
                    }
                    else if (symbol.Kind == Kind.Keyword)
                    {
                        filterText = symbol.Name;
                    }

                    if (type != null)
                    {
                        switch (type.FullName)
                        {
                            case XSharpTypeNames.XSharpUsual:
                            case XSharpTypeNames.VulcanUsual:
                                type = null;
                                break;
                        }
                    }
                    session.Properties[XsCompletionProperties.Type] = type;
                }
                if (type == null)
                {
                    showInstanceMembers = false;
                }
                if ((dotSelector || state != CompletionState.None) )
                {
                    if (string.IsNullOrEmpty(filterText) && type ==null)
                    {
                        filterText = helpers.TokenListAsString(tokenList);
                        if (filterText.Length > 0 && !filterText.EndsWith(".") && state != CompletionState.General)
                            filterText += ".";
                    }
                    if (type == null && state.HasFlag(CompletionState.Namespaces))
                    {
                        helpers.AddNamespaces(compList, location, filterText);
                    }
                    if (state.HasFlag(CompletionState.Interfaces))
                    {
                        helpers.AddTypeNames(compList, location, filterText,  afterDot: typedChar == '.', onlyInterfaces: true);
                    }
                    if (state.HasFlag(CompletionState.Types)  )
                    {
                        helpers.AddTypeNames(compList, location, filterText, afterDot: typedChar == '.', onlyInterfaces: false);
                        if (addKeywords)
                            helpers.AddXSharpKeywordTypeNames(kwdList, filterText);
                    }
                    if (state.HasFlag(CompletionState.StaticMembers))
                    {
                        if (type != null && symbol is IXTypeSymbol)
                        {
                            // First we need to keep only the text AFTER the last dot
                           int dotPos = filterText.LastIndexOf('.');
                            if (dotPos > 0)
                                filterText = filterText.Substring(dotPos + 1, filterText.Length - dotPos - 1);
                            helpers.BuildCompletionListMembers(location, compList, type, Modifiers.Public, true, filterText);
                        }
                    }
                    if (type.IsVoStruct() && typedChar == '.')
                    {
                        // vostruct or union in other assembly
                        showInstanceMembers = true;
                        filterText = "";
                    }
                    if (state.HasFlag(CompletionState.InstanceMembers) )
                    {
                        showInstanceMembers = true;
                        if (typedChar == '.' || typedChar == ':')
                            filterText = "";
                    }
                }
                if (showInstanceMembers && isInstance)
                {
                    // Member call
                    if (type != null)
                    {
                        Modifiers visibleAs = Modifiers.Public;
                        if (type is XSourceTypeSymbol sourceType && sourceType.File.Project == member.File.Project)
                        {
                            visibleAs = Modifiers.Internal;
                            switch (memberName.ToLower())
                            {
                                case "self":
                                case "this":
                                    visibleAs = Modifiers.Private;
                                    break;
                                case "super":
                                    visibleAs = Modifiers.Protected;
                                    break;
                                default:
                                    if (member.ParentName == type.FullName)
                                    {
                                        visibleAs = Modifiers.Private;
                                    }
                                    break;
                            }
                            }
                        // Now, Fill the CompletionList with the available members, from there
                        helpers.BuildCompletionListMembers(location, compList, type, visibleAs, false, filterText);
                    }
                }
                //

                if (!dotSelector && !showInstanceMembers)
                {
                    switch (tokenType)
                    {
                        case XSharpLexer.USING:
                            // It can be a namespace
                            helpers.AddNamespaces(compList, location, filterText);
                            break;
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.INHERIT:
                        case XSharpLexer.PARAMS:
                            // It can be a namespace
                            helpers.AddNamespaces(compList, location, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            helpers.AddTypeNames(compList, location, filterText, onlyInterfaces: false, afterDot: false);
                            //
                            helpers.AddXSharpKeywordTypeNames(kwdList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            // It can be a namespace
                            helpers.AddNamespaces(compList, location, filterText);
                            helpers.AddTypeNames(compList, location, filterText,onlyInterfaces: true, afterDot: false);
                            break;
                        default:
                            if (state.HasFlag(CompletionState.General))
                            {
                                if (tokenList.Count == 1 && XSharpLexer.IsKeyword(tokenList[0].Type))
                                {
                                    notProcessed = tokenList[0].Text;
                                }
                                filterText = notProcessed;
                                helpers.AddGenericCompletion(compList, location, filterText);
                            }
                            break;
                    }
                }
                session.Properties[XsCompletionProperties.Filter] = filterText;
                if ((kwdList.Count > 0) && _keywordsInAll /*&& XSettings.CompleteKeywords*/)
                {
                    foreach (var item in kwdList.Values)
                        compList.Add(item,true);
                }
                // Sort in alphabetical order
                // and put in the SelectionList
                if (compList.Values.Count == 1)
                {
                    var value = compList.Values.First();
                    if (value.KeyText.ToLower() == filterText.ToLower())
                    {
                        compList.Clear();
                    }
                }
                var values = compList.Values;
                // Create the All Tab
                completionSets.Add(new CompletionSet("All", "All", applicableTo, values, Enumerable.Empty<Completion>()));
                if (_showTabs)
                {
                    helpers.BuildTabs(compList, completionSets, applicableTo);
                }
                // Keywords are ALWAYS in a separate Tab anyway
                if (kwdList.Count > 0)
                {
                    completionSets.Add(new CompletionSet("Keywords", "Keywords", applicableTo, kwdList.Values, Enumerable.Empty<Completion>()));
                }
            }
            catch (Exception ex)
            {
                XSettings.LogException(ex, "AugmentCompletionSessions failed");

            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
            WriteOutputMessage("<<-- AugmentCompletionSessions");
        }



        public void Dispose()
        {
            if (!_disposed)
            {
                // Was missing, but doesn't solved the Deadlock with Intellisense
                GC.SuppressFinalize(this);
                _disposed = true;
            }
        }
    }
}


#endif

