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
using System.Diagnostics;
using Microsoft.VisualStudio.Text.Tagging;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;

namespace XSharp.LanguageService
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [Name("XSharpCompletion")]
    class XSharpCompletionSourceProvider : ICompletionSourceProvider
    {
        [Import]
        internal SVsServiceProvider ServiceProvider = null;

        [Import]
        internal IGlyphService GlyphService = null;

        [Import]
        IBufferTagAggregatorFactoryService aggregator = null;

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {

            return new XSharpCompletionSource(this, textBuffer, aggregator);
        }
    }

    partial class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;
        private bool _settingIgnoreCase;
        // Keep a trace of the Context of the TokenList build
        private IToken _stopToken;

        private XFile _file;
        private bool _showTabs;
        private bool _keywordsInAll;
        private bool _dotUniversal;
        private IBufferTagAggregatorFactoryService aggregator;
        private XSharpDialect _dialect;

        internal static bool StringEquals(string lhs, string rhs)
        {
            if (string.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XSharpCompletionSource(XSharpCompletionSourceProvider provider, ITextBuffer buffer, IBufferTagAggregatorFactoryService aggregator)
        {
            _provider = provider;
            _buffer = buffer;
            _file = buffer.GetFile();
            // Currently, set as default, but should be VS Settings Based
            // Retrieve from Project properties later: _file.Project.ProjectNode.ParseOptions.
            var prj = _file.Project.ProjectNode;
            _dialect = _file.Project.Dialect;
            _settingIgnoreCase = true;
            _stopToken = null;
            this.aggregator = aggregator;
        }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage(strMessage);
            }
        }
        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            WriteOutputMessage("-->> AugmentCompletionSessions");
            try
            {
                if (XSettings.DisableCodeCompletion)
                    return;
                XSharpModel.ModelWalker.Suspend();
                if (_disposed)
                    throw new ObjectDisposedException("XSharpCompletionSource");
                _showTabs = XSettings.EditorCompletionListTabs;
                _keywordsInAll = XSettings.EditorKeywordsInAll;
                if (_dialect == XSharpDialect.FoxPro)
                {
                    _dotUniversal = true;
                }
                else if (_dialect == XSharpDialect.Core)
                {
                    _dotUniversal = XSettings.EditorUseDotAsUniversalSelector;
                }
                else
                {
                    _dotUniversal = false;
                }
                // Where does the StartSession has been triggered ?
                ITextSnapshot snapshot = _buffer.CurrentSnapshot;
                var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
                if (triggerPoint == null)
                    return;
                // The "parameters" coming from CommandFilter
                uint cmd;
                char typedChar;
                session.Properties.TryGetProperty(XsCompletionProperties.Command, out cmd);
                VSConstants.VSStd2KCmdID nCmdId = (VSConstants.VSStd2KCmdID)cmd;
                session.Properties.TryGetProperty(XsCompletionProperties.Char, out typedChar);
                if (typedChar == '\0')
                {

                }
                // Reset the StopToken
                this._stopToken = null;
                // What is the character were it starts ?
                var line = triggerPoint.GetContainingLine();
                ////////////////////////////////////////////
                //
                SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
                SnapshotPoint caret = triggerPoint;
                var tagAggregator = aggregator.CreateTagAggregator<IClassificationTag>(_buffer);
                var tags = tagAggregator.GetTags(lineSpan);
                //List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
                IMappingTagSpan<IClassificationTag> lastTag = null;
                foreach (var tag in tags)
                {
                    //tagList.Add(tag);
                    SnapshotPoint ptStart = tag.Span.Start.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    SnapshotPoint ptEnd = tag.Span.End.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    //tagList.Add(tag);
                    if ((ptStart != null) && (ptEnd != null))
                        if ((caret.Position >= ptStart.Position) && (caret.Position <= ptEnd.Position))
                        {
                            lastTag = tag;
                            break;
                        }
                }
                if (lastTag != null)
                {
                    var name = lastTag.Tag.ClassificationType.Classification.ToLower();
                    // No Intellisense in Comment
                    if (name == "comment" || name == "xsharp.text")
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
                CompletionList compList = new CompletionList(_file);
                CompletionList kwdList = new CompletionList(_file);
                // The CompletionType we will use to fill the CompletionList
                CompletionType cType = null;
                if (session.Properties.ContainsProperty(XsCompletionProperties.Type))
                {
                    cType = (CompletionType)session.Properties[XsCompletionProperties.Type];
                }
                // Start of Process
                string filterText = "";
                // Check if we can get the member where we are
                int currentLine = triggerPoint.GetContainingLine().LineNumber;
                var member = XSharpLookup.FindMember(currentLine, this._file);
                var currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, this._file);
                // Standard TokenList Creation (based on colon Selector )
                var caretPos = triggerPoint.Position;
                bool includeKeywords;
                session.Properties.TryGetProperty(XsCompletionProperties.IncludeKeywords, out includeKeywords);

                var tokenList = XSharpTokenTools.GetTokenList(caretPos, currentLine, _buffer.CurrentSnapshot, out _stopToken, _file, member, includeKeywords);



                // We might be here due to a COMPLETEWORD command, so we have no typedChar
                // but we "may" have a incomplete word like System.String.To
                // Try to Guess what TypedChar could be
                if (typedChar == '\0')
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
                                    tokenList[0].Text = startToken + ".";
                                }
                            }
                            else
                            {
                                // So, we get the last Token as a Filter
                                filterText = tokenList[tokenList.Count - 1].Text;
                            }
                            // but what could be the typedChar?
                            if (tokenList.Count > 1)
                            {
                                extract = tokenList[tokenList.Count - 2].Text;
                                typedChar = extract[extract.Length - 1];
                                tokenList.RemoveAt(tokenList.Count - 1);
                                //tokenLine = TokenListAsString(tokenList, 0);
                            }
                            // Include the filter as the text to replace
                            start -= filterText.Length;
                            applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                        }
                    }
                }
                // Special Phil
                bool dotSelector = (typedChar == '.');
                bool showInstanceMembers = (typedChar == ':');
                //
                if (dotSelector && _dotUniversal)
                {
                    showInstanceMembers = true;
                }
                // Alternative Token list (dot is a selector)
                List<XSharpToken> altTokenList;
                if (dotSelector && _dotUniversal)
                    altTokenList = XSharpTokenTools.GetTokenList(caretPos, currentLine, _buffer.CurrentSnapshot, out _stopToken, _file,  member);
                else
                    altTokenList = tokenList;

                HashSet<string> Usings = new HashSet<string>(_file.Usings, StringComparer.OrdinalIgnoreCase);
                if (currentNamespace != null)
                {
                    Usings.Add(currentNamespace.Name);
                }
                if (member != null && member.Parent != null && (member.Parent.Kind == Kind.Class || member.Parent.Kind == Kind.Interface))
                {
                    var ns = member.Parent.Parent;
                    string nsName = "";
                    if ((ns != null) && (ns.Kind == Kind.Namespace))
                    {
                        nsName = ns.Name + ".";
                    }
                    var name = nsName + member.Parent.Name;
                    var pos = name.LastIndexOf(".");
                    if (pos >= 0)
                    {
                        name = name.Substring(0, pos);
                        if (!Usings.Contains(name))
                        {
                            Usings.Add(name);
                        }
                    }
                }
                // TODO: Based on the Project.Settings, we should add the Vulcan.VO namespace
                int tokenType = XSharpLexer.UNRECOGNIZED;
                if (this._stopToken != null)
                {
                    tokenType = this._stopToken.Type;
                }
                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement foundElement;
                string currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                //
                cType = XSharpLookup.RetrieveType(_file, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, _dialect);
                if (!cType.IsEmpty())
                {
                    session.Properties[XsCompletionProperties.Type] = cType;
                }
                else
                {
                    if (dotSelector && _dotUniversal)
                    {
                        cType = XSharpLookup.RetrieveType(_file, altTokenList, member, currentNS, null, out foundElement, snapshot, currentLine, _dialect);
                        if (!cType.IsEmpty())
                        {
                            session.Properties[XsCompletionProperties.Type] = cType;
                        }
                    }
                }
                //
                if (dotSelector)
                {
                    if (string.IsNullOrEmpty(filterText))
                    {
                        filterText = TokenListAsString(tokenList, 0);
                        if (!filterText.EndsWith("."))
                            filterText += ".";
                    }
                    switch (tokenType)
                    {
                        case XSharpLexer.USING:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            break;
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.INHERIT:
                        case XSharpLexer.FROM:          // Xbase++ Parent clause
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, _file.Project, filterText, Usings);
                            //
                            AddXSharpKeywordTypeNames(kwdList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // TODO: add Interfaces only
                            break;
                        default:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, _file.Project, filterText, Usings);
                            //
                            AddXSharpKeywordTypeNames(kwdList, filterText);
                            // it can be a static Method/Property/Enum
                            if (cType != null)
                            {
                                // First we need to keep only the text AFTER the last dot
                                int dotPos = filterText.LastIndexOf('.');
                                filterText = filterText.Substring(dotPos + 1, filterText.Length - dotPos - 1);
                                BuildCompletionList(compList, cType, Modifiers.Public, true, filterText);
                            }
                            //
                            break;
                    }
                }
                //
                if (showInstanceMembers)
                {
                    // Member call
                    if (cType != null)
                    {
                        Modifiers visibleAs = Modifiers.Public; 
                        if (cType.XTypeDef != null && cType.XTypeDef.File.Project == member.File.Project)
                        {
                            visibleAs = Modifiers.Internal;
                        }
                        if (foundElement != null)
                        {
                            switch (foundElement.Name.ToLower())
                            {
                                case "self":
                                case "this":
                                    visibleAs = Modifiers.Private ;
                                    break;
                                case "super":
                                    visibleAs = Modifiers.Protected;
                                    break;
                                default:
                                    if (member.ParentName == cType.FullName)
                                    {
                                        visibleAs = Modifiers.Private;
                                    }
                                    break;
                            }
                        }
                        else if (member.ParentName == cType.FullName)
                        {
                            visibleAs = Modifiers.Private;
                        }
                        // Now, Fill the CompletionList with the available members, from there
                        BuildCompletionList(compList, cType, visibleAs, false, filterText);
                    }
                }
                //
                if (!dotSelector && !showInstanceMembers)
                {
                    // Empty line ?
                    // If we have only one Token, it can be the start of a Parameter/Local/Property/Method/Type/...
                    // .........
                    // .........
                    // We were able to determine the Type, so Get the Members
                    //if (cType != null)
                    //{
                    //    Modifiers visibleAs = Modifiers.Public;
                    //    if (member.ParentName == cType.FullName)
                    //    {
                    //        visibleAs = Modifiers.Private;
                    //    }
                    //    // Now, Fill the CompletionList with the available members, from there
                    //    BuildCompletionList(compList, cType, visibleAs, false, filterText);

                    //}
                    //else
                    {
                        switch (tokenType)
                        {
                            case XSharpLexer.USING:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                break;
                            case XSharpLexer.AS:
                            case XSharpLexer.IS:
                            case XSharpLexer.REF:
                            case XSharpLexer.INHERIT:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                // It can be Type, FullyQualified
                                // we should also walk all the USINGs, and the current Namespace if any, to search Types
                                AddTypeNames(compList, _file.Project, filterText, Usings);
                                //
                                AddXSharpKeywordTypeNames(kwdList, filterText);
                                break;
                            case XSharpLexer.IMPLEMENTS:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                // TODO: add Interfaces only
                                break;
                            default:

                                if (member != null)
                                {
                                    // Fill with the context ( Parameters and Locals )
                                    BuildCompletionList(compList, member, filterText, currentLine);
                                    AddXSharpKeywords(compList, filterText);
                                    // Context Type....
                                    cType = new CompletionType(((XSourceTypeSymbol) (member.Parent)).Clone);
                                    if (!cType.IsEmpty())
                                    {
                                        // Get the members also
                                        BuildCompletionList(compList, cType, Modifiers.Private, false, filterText);
                                    }
                                }
                                // Now Add Functions and Procedures
                                BuildCompletionList(compList, _file.Project.Lookup(XLiterals.GlobalName), Modifiers.Public, false, filterText);
                                foreach (var project in _file.Project.ReferencedProjects)
                                {
                                    BuildCompletionList(compList, project.Lookup(XLiterals.GlobalName), Modifiers.Public, false, filterText);
                                }
                                // and Add NameSpaces
                                AddNamespaces(compList, _file.Project, filterText);
                                // and Types
                                AddTypeNames(compList, _file.Project, filterText, Usings);
                                //
                                AddXSharpKeywordTypeNames(kwdList, filterText);
                                //
                                AddUsingStaticMembers(compList, _file, filterText);
                                break;
                        }
                    }
                }
                // Add Keywords to the ALL Tab ?
                if ((kwdList.Count > 0) && _keywordsInAll)
                {
                    foreach (var item in kwdList.Values)
                        compList.Add(item);
                }
                // Sort in alphabetical order
                // and put in the SelectionList
                var values = compList.Values;
                // Create the All Tab
                completionSets.Add(new CompletionSet("All", "All", applicableTo, values, Enumerable.Empty<Completion>()));
                if (_showTabs)
                {
                    if (compList.HasEnumMembers)
                    {
                        var sub = values.Where(item => item.Kind == Kind.EnumMember);
                        completionSets.Add(new CompletionSet("Values", "Values", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasMethods)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Method);
                        completionSets.Add(new CompletionSet("Methods", "Methods", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasFields)
                    {
                        var sub = values.Where(item => item.Kind.IsField());
                        completionSets.Add(new CompletionSet("Fields", "Fields", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasProperties)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Property);
                        completionSets.Add(new CompletionSet("Properties", "Properties", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasEvents)
                    {
                        var sub = compList.Values.Where(item => item.Kind == Kind.Event);
                        completionSets.Add(new CompletionSet("Events", "Events", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasTypes)
                    {
                        var sub = values.Where(item => item.Kind.IsType());
                        completionSets.Add(new CompletionSet("Types", "Types", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasNamespaces)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Namespace);
                        completionSets.Add(new CompletionSet("Namespaces", "Namespaces", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                }
                // Keywords are ALWAYS in a separate Tab anyway
                if (kwdList.Count > 0)
                {
                    completionSets.Add(new CompletionSet("Keywords", "Keywords", applicableTo, kwdList.Values, Enumerable.Empty<Completion>()));
                }
            }
            catch (Exception ex)
            {
                WriteOutputMessage("AugmentCompletionSessions failed: ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
            WriteOutputMessage("<<-- AugmentCompletionSessions");
        }

        private void AddUsingStaticMembers(CompletionList compList, XFile file, string filterText)
        {
            //
            foreach (string staticType in file.AllUsingStatics)
            {
                BuildCompletionList(compList, new CompletionType(staticType, file, ""), Modifiers.Public, true, filterText);
            }
            // And what about Global Types in referenced Assemblies ?
            var found = file.Project.FindGlobalMembersInAssemblyReferences(filterText);
            FillMembers(compList, found, Modifiers.Public, true);
        }
        public static bool isGenerated(IXTypeSymbol type)
        {
            return type.Name.IndexOf("$", StringComparison.Ordinal) > -1;
        }

        private void AddTypeNames(CompletionList compList, XProject project, string startWith, HashSet<string> usings)
        {
            if (string.IsNullOrEmpty(startWith))
                return;
      
            // We are looking for NameSpaces, in References
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            // Resolve projects. This adds them also to the AssemblyReferences
            //
            var sprjs = project.StrangerProjects;
            var prjs = project.ReferencedProjects;


            if (startWith.Length == 0)
                return;
            foreach (var type in project.FindSystemTypesByName(startWith, usings.ToArray()))
            {
                if (isGenerated(type))
                   continue;

                string realTypeName = type.FullName;
                if (IsHiddenTypeName(realTypeName))
                {
                    continue;
                }
                var typeAnalysis = new XTypeAnalysis(type);
                // Nested Type ?
                if (realTypeName.Contains("+"))
                {
                    realTypeName = realTypeName.Replace('+', '.');
                }
                // remove the start
                if (startLen > 0 && realTypeName.Length > startLen && realTypeName.StartsWith(startWith, StringComparison.OrdinalIgnoreCase))
                    realTypeName = realTypeName.Substring(startLen);
                // Do we have another part file
                dotPos = realTypeName.LastIndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realTypeName = realTypeName.Substring(dotPos+1);
                if (IsHiddenTypeName(realTypeName))
                    continue;

                //
                ImageSource icon = _provider.GlyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeAnalysis.Prototype, icon, null, Kind.Class,"")))
                    break;
            }
            //
            // And our own Types
            AddXSharpTypeNames(compList, project, startWith, usings.ToArray());
   
        }

        private bool IsHiddenTypeName(string realTypeName)
        {
            if (realTypeName.Length > 2 && realTypeName.StartsWith("__", StringComparison.Ordinal) && XSettings.EditorHideAdvancedMembers)
                return true;
            if (realTypeName.IndexOf('$') >= 0)
                return true;
            return false;
        }


        private bool IsHiddenMemberName(string realMemberName)
        {
            if (realMemberName.Length > 2 && realMemberName.StartsWith("__", StringComparison.Ordinal) && XSettings.EditorHideAdvancedMembers)
                return true;
            // suppress SELF properties
            if (string.Compare(realMemberName, "self", StringComparison.Ordinal) == 0)
                return true;
            if (realMemberName.IndexOf('$') >= 0)
                return true;

            if (realMemberName.Length > 4)
            {
                // event add
                if (realMemberName.StartsWith("add_",StringComparison.Ordinal))
                    return true;
                // property get
                if (realMemberName.StartsWith("get_", StringComparison.Ordinal))
                    return true;
                // property set
                if (realMemberName.StartsWith("set_", StringComparison.Ordinal))
                    return true;
                // operator
                if (realMemberName.StartsWith("op_", StringComparison.Ordinal))
                    return true;
            }
            // event remove
            if (realMemberName.Length > 7 && realMemberName.StartsWith("remove_", StringComparison.Ordinal))
                return true;
            return false;
        }

        private void AddXSharpKeywords(CompletionList compList, string startWith)
        {
            foreach (var kw in XSharpTypes.Get().Where(ti => nameStartsWith(ti.Name, startWith)))
            {
                ImageSource icon = _provider.GlyphService.GetGlyph(kw.getGlyphGroup(), kw.getGlyphItem());
                compList.Add(new XSCompletion(kw.Name, kw.Name, kw.Prototype, icon, null, Kind.Keyword,""));
            }
        }

        private void AddXSharpTypeNames(CompletionList compList, XProject project, string startWith, IReadOnlyList<string> usings)
        {
            var list = project.GetTypes(startWith, usings);
            foreach (var typeInfo in list)
            {
                
                // Then remove it
                ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.getGlyphGroup(), typeInfo.getGlyphItem());
                if (!compList.Add(new XSCompletion(typeInfo.Name, typeInfo.Name, typeInfo.FullName, icon, null, typeInfo.Kind,"")))
                    break;
            }
        }

        private void AddXSharpKeywordTypeNames(CompletionList compList, string startWith)
        {
            //
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            // And our own Types
            var xsharpTypes = XSharpTypes.GetTypes();
            foreach (var typeInfo in xsharpTypes.Where(ti => nameStartsWith(ti.Name, startWith)))
            {
                string realTypeName = typeInfo.FullName;
                // remove the start
                if (startLen > 0)
                    realTypeName = realTypeName.Substring(startLen);
                // Do we have another part
                dotPos = realTypeName.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realTypeName = realTypeName.Substring(0, dotPos);
                ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.getGlyphGroup(), typeInfo.getGlyphItem());
                if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Prototype, icon, null, Kind.Class,"")))
                    break;
            }
        }


        private void AddNamespaces(CompletionList compList, XProject project, string startWith)
        {
            // We are looking for NameSpaces, in References
            var namespaces = project.GetAssemblyNamespaces();
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            ImageSource icon = _provider.GlyphService.GetGlyph(StandardGlyphGroup.GlyphGroupNamespace, StandardGlyphItem.GlyphItemPublic);
            foreach (string nameSpace in namespaces.Where(ns => nameStartsWith(ns, startWith)))
            {
                string realNamespace = nameSpace;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                //
                if (!compList.Add(new XSCompletion(realNamespace, realNamespace, "Namespace " + nameSpace, icon, null, Kind.Namespace,"")))
                    break;
            }
            //
            // And our own Namespaces
            AddXSharpNamespaces(compList, project, startWith, icon);
            // We should also add the external NameSpaces
            var prjs = project.ReferencedProjects;
            foreach (var prj in prjs)
            {
                AddXSharpNamespaces(compList, prj, startWith, icon);
            }
            // And Stranger Projects
            //var sprjs = project.StrangerProjects;
            //foreach (var prj in sprjs)
            //{
            //    AddStrangerNamespaces(compList, prj, startWith, icon);
            //}
        }

        private void AddXSharpNamespaces(CompletionList compList, XProject project, string startWith, ImageSource icon)
        {
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            // And our own Namespaces
            var xsNamespaces = project.Namespaces;
            foreach (string nameSpace in xsNamespaces.Where(ns => nameStartsWith(ns, startWith)))
            {
                string realNamespace = nameSpace;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                if (!compList.Add(new XSCompletion(realNamespace, realNamespace, nameSpace, icon, null, Kind.Namespace,"")))
                    break;
            }
        }

        private void BuildCompletionList(CompletionList compList, XSourceMemberSymbol currentMember, string startWith, int currentLine)
        {
            if (currentMember == null)
            {
                return;
            }
            // First, look after Parameters
            foreach (var paramVar in currentMember.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(paramVar.getGlyphGroup(), paramVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Prototype, icon, null, Kind.Parameter,"")))
                    break;
            }
            // Then, look for Locals
            // line numbers in the range are 1 based. currentLine = 0 based !
            foreach (var localVar in currentMember.GetLocals(_buffer.CurrentSnapshot, currentLine, _dialect).Where(l => nameStartsWith(l.Name, startWith) && l.Range.StartLine-1 <= currentLine))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(localVar.getGlyphGroup(), localVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Prototype, icon, null, Kind.Local,"")))
                    break;
                
            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //

            if (currentMember.Kind.IsClassMember(_dialect))
            {
                var classElement = currentMember.Parent as XSourceTypeSymbol;
                foreach (var member in classElement.GetMembers(startWith).Where(m => m.Kind == Kind.Field ))
                {
                    ImageSource icon = _provider.GlyphService.GetGlyph(member.getGlyphGroup(), member.getGlyphItem());
                    if (!compList.Add(new XSCompletion(member.Name, member.Name, member.Prototype, icon, null, Kind.Field,"")))
                        break;
                }
            }
        }

        /// <summary>
        /// Fill the CompletionList by enumerating the members of the Parent
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="parent">The XSourceTypeSymbol element</param>
        /// <param name="minVisibility">The minimum Visibility</param>
        /// <param name="staticOnly">Static member only ?</param>
        /// <param name="startWith">The filter text</param>
        private void BuildCompletionList(CompletionList compList, XSymbol parent, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (parent == null)
            {
                return;
            }
            if (!(parent is XSourceTypeSymbol))
            {
                return;
            }
            //
            IXTypeSymbol Owner = parent as IXTypeSymbol;
            //
            bool hideAdvanced = XSettings.EditorHideAdvancedMembers;
            foreach (var elt in Owner.GetMembers(startWith))
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic != staticOnly)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                if (IsHiddenMemberName(elt.Name))
                {
                    continue;
                }
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                string toAdd = "";
                if (elt.Kind.HasParameters() && elt.Kind != Kind.Constructor)
                {
                    toAdd = "(";
                }
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Prototype, icon, null, elt.Kind,elt.Value)))
                    break;
            }
            // Hummm, we should call for Owner of the Owner.. Super !
            Owner.ForceComplete();
            if (Owner.Parent != null)
            {
                BuildCompletionList(compList, parent, Modifiers.Protected, staticOnly, startWith);
            }
        }

        private void BuildCompletionList(CompletionList compList, CompletionType cType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (cType == null)
            {
                return;
            }
            //
            if (cType.XTypeDef != null)
            {
                FillMembers(compList, cType.XTypeDef, minVisibility, staticOnly, startWith);
                // Hummm, we should call for Owner of the Owner.. Super !
                cType.XTypeDef.ForceComplete();
                if (cType.BaseType!= null)
                {
                    // Parent has just a Name, so one of the System Types
                    BuildCompletionList(compList, new CompletionType(cType.BaseType, _file, _file.Usings), Modifiers.Protected, staticOnly, startWith);
                }
            }
            else if (cType.XTypeRef != null)
            {
                // Now add Members for System types
                FillMembers(compList, cType.XTypeRef, minVisibility, staticOnly, startWith);
                //
            }
            FillExtensions(compList, cType, startWith);
        }


        private bool nameStartsWith(string name, string startWith)
        {
            // prevent crash for members without a name.
            if (startWith?.Length == 0)
                return true;
            if (name != null)
                return name.StartsWith(startWith, this._settingIgnoreCase, System.Globalization.CultureInfo.InvariantCulture);
            return false;
        }
        /// <summary>
        /// Add members to the completionlist
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="members"></param>
        /// <param name="minVisibility"></param>
        /// <param name="staticOnly"></param>
        private void FillMembers(CompletionList compList, IEnumerable<IXMemberSymbol> members, Modifiers minVisibility, bool staticOnly)
        {
            WriteOutputMessage($"FillMembers start, {members.Count()} members");
            foreach (var elt in members)
            {
                bool add = true;
                if (IsHiddenMemberName(elt.Name))
                {
                    continue;
                }
                switch (elt.Kind)
                {
                    case Kind.EnumMember:
                        add = true;
                        break;
                    case Kind.Constructor:
                    case Kind.Operator:
                        add = false;
                        break;
                    default:
                        if (elt.IsStatic != staticOnly)
                            add = false;
                        if (add && elt.Visibility < minVisibility)
                            add = false;
                        if (elt.Visibility == Modifiers.Internal)
                        {
                            if (elt is XSourceMemberSymbol)
                            {
                                var member = (XSourceMemberSymbol)elt;
                                if (member.File == null )
                                {
                                    add = false;
                                }
                                else
                                {
                                    if (compList.File.Project.FindXFile(member.File.FullPath) == null)
                                    {
                                        add = false;
                                    }
                                }
                            }
                        }
                        break;
                }
                if (!add)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                string toAdd = "";
                if (elt.Kind.HasParameters() && elt.Kind != Kind.Constructor)
                {
                    toAdd = "(";
                }
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Prototype, icon, null, elt.Kind, elt.Value)))
                    break;
            }
            WriteOutputMessage($"FillMembers stop");
        }
        /// <summary>
        /// Add Members for our Types to the completionlist
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="xType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, IXTypeSymbol xType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            FillMembers(compList, xType.GetMembers(startWith), minVisibility, staticOnly);
        }


        private void FillExtensions(CompletionList compList, CompletionType cType, string startWith)
        {
            //WriteOutputMessage($"FillExtensions for type {sType.FullName}");
            if (cType.Type != null)
            {
                var extensions = _file.Project.GetExtensions(cType.Type.FullName);
                IEnumerable<IXMemberSymbol> selection = extensions;
                if (! string.IsNullOrEmpty(startWith))
                {
                    selection = extensions.Where(x => nameStartsWith(x.Name, startWith));
                }
                FillMembers(compList, selection, Modifiers.Public, true);
                foreach (var ifname in cType.Type.Interfaces)
                {
                    var lifname = ifname;
                    var lookupproject = _file.Project;
                    if (cType.XTypeDef != null)
                    {
                        var typedef = cType.XTypeDef;
                        var origfile = XSolution.FindFullPath(typedef.File.FullPath);
                        lookupproject = origfile.Project;
                        var reftype = SystemTypeController.FindType(lifname, typedef.FileUsings, lookupproject.AssemblyReferences);
                        if (reftype != null)
                        {
                            lifname = reftype.FullName;
                        }
                    }
                    extensions = lookupproject.GetExtensions(lifname);
                    selection = extensions;
                    if (!string.IsNullOrEmpty(startWith))
                    {
                        selection = extensions.Where(x => nameStartsWith(x.Name, startWith));
                    }
                    FillMembers(compList, selection, Modifiers.Public, true);
                }
            }
            //WriteOutputMessage($"FillExtensions complete for type {sType.FullName}");
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

        /// <summary>
        /// Flatten the TokenList as a String
        /// </summary>
        /// <param name="tokenList"></param>
        /// <returns></returns>
        private string TokenListAsString(List<XSharpToken> tokenList, int less)
        {
            string retValue = "";
            for (int pos = 0; pos < tokenList.Count - less; pos++)
            {
                string tk = tokenList[pos].Text;
                retValue += tk;
            }
            return retValue;
        }
    }
  

    /// <summary>
    /// Process a TypeInfo in order to provide usable informations (TypeName, Glyph, ... )
    /// </summary>
 
    /// <summary>
    /// XSharp CompletionList
    /// </summary>
    internal class CompletionList : SortedDictionary<string, XSCompletion>
    {
        internal bool HasMethods { get; private set; }
        internal bool HasProperties { get; private set; }
        internal bool HasFields { get; private set; }
        internal bool HasEvents { get; private set; }
        internal bool HasEnumMembers { get; private set; }
        internal bool HasTypes { get; private set; }
        internal bool HasNamespaces { get; private set; }
        internal XFile _file;
        internal CompletionList(XFile startLocation) : base(StringComparer.OrdinalIgnoreCase)
        {
            _file = startLocation;
        }
        internal XFile File => _file;
        public bool Add(XSCompletion item)
        {

            if (ContainsKey(item.DisplayText))
            {
                // only methods have overloads
                // we do not want to the overloads message for partial classes that appear in more than 1 file
                // and if a method in a subclass has the same params we also do not want to show that there are overloads
                var found = this[item.DisplayText];
                if (!string.Equals(found.Description, item.Description, StringComparison.OrdinalIgnoreCase))
                {
                    int overloads = 0;
                    // Already exists in the List !!
                    // First Overload ?
                    XSCompletion comp = this[item.DisplayText];
                    if (!comp.Properties.TryGetProperty(XsCompletionProperties.Overloads, out overloads))
                    {
                        comp.Properties.AddProperty(XsCompletionProperties.Overloads, overloads);
                    }
                    overloads += 1;
                    // Set the number of Overload(s)
                    comp.Properties[XsCompletionProperties.Overloads] = overloads;
                    // Now, hack the Description text
                }
                // Ok, Forget about the newly added Completion please
                return true;

            }
            if (!string.IsNullOrEmpty(item.DisplayText))
            {
                switch (item.Kind)
                {
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                    case Kind.Operator:
                    case Kind.VODLL:
                    case Kind.LocalFunc:
                    case Kind.LocalProc:
                        HasMethods = true;
                        break;
                    case Kind.Event:
                        HasEvents = true;
                        break;
                    case Kind.Property:
                    case Kind.Access:
                    case Kind.Assign:
                        HasProperties = true;
                        break;
                    case Kind.Namespace:
                        HasNamespaces = true;
                        break;
                    case Kind.EnumMember:
                        HasEnumMembers = true;
                        break;
                    case Kind.Constructor:
                        break;
                    case Kind.Destructor:
                        break;
                    case Kind.Local:
                        break;
                    case Kind.Parameter:
                        break;
                    case Kind.Delegate:
                        break;
                    case Kind.Using:
                        break;
                    case Kind.Keyword:
                        break;
                    default:
                        if (item.Kind.IsField())
                            HasFields = true;
                        else if (item.Kind.IsType())
                            HasTypes = true;
                        break;
                }
                base.Add(item.DisplayText, item);
            }
            return true;
        }
    }

    internal static class XsCompletionProperties
    {
        public const string Type = nameof(Type);
        public const string Char = nameof(Char);
        public const string IncludeKeywords = nameof(IncludeKeywords);
        public const string Command = nameof(Command);
        public const string Overloads = nameof(Overloads);

    }

    /// <summary>
    /// XSharp Completion class.
    /// Overload the Description property in order to add "overload" text at the end
    /// </summary>
    [DebuggerDisplay("{DisplayText,nq}")]
    public class XSCompletion : Completion
    {
 
        public XSharpModel.Kind Kind { get; set; }
        public string Value { get; set; }
        public XSCompletion(string displayText, string insertionText, string description, 
            ImageSource iconSource, string iconAutomationText, XSharpModel.Kind kind, string value)
            : base(displayText, insertionText, description, iconSource, iconAutomationText)
        {
            Kind = kind;
            Value = value;
        }

        public override string Description
        {
            get
            {
                string desc;
                int overloads = 0;
                this.Properties.TryGetProperty(XsCompletionProperties.Overloads, out overloads);
                if (overloads > 0)
                {
                    desc = base.Description;
                    desc += " (+" + overloads + " overload";
                    if (overloads > 1)
                        desc += "s";
                    desc += ")";
                }
                else
                {
                    desc = base.Description;
                }
                if (!string.IsNullOrEmpty(Value))
                {
                    desc += " := " + Value;
                }
                //
                return desc;
            }
            set
            {
                base.Description = value;
            }
        }

    }


 
    /// <summary>
    /// Static class Tools. Offer services to get Glyphs (Icons) to CompletionSource (at least) ...
    /// </summary>
    public static class XSharpGlyphTools
    {
        public static StandardGlyphGroup getGlyphGroup(this IXSymbol elt)
        {
            StandardGlyphGroup imgG = StandardGlyphGroup.GlyphGroupClass;
            switch (elt.Kind)
            {
                case Kind.Class:
                    imgG = StandardGlyphGroup.GlyphGroupClass;
                    break;
                case Kind.Constructor:
                case Kind.Destructor:
                case Kind.Method:
                case Kind.Function:
                case Kind.Procedure:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                    imgG = StandardGlyphGroup.GlyphGroupMethod;
                    break;
                case Kind.Structure:
                    imgG = StandardGlyphGroup.GlyphGroupStruct;
                    break;
                case Kind.Access:
                case Kind.Assign:
                case Kind.Property:
                    imgG = StandardGlyphGroup.GlyphGroupProperty;
                    break;
                case Kind.Local:
                case Kind.MemVar:
                case Kind.DbField:
                case Kind.Parameter:
                    imgG = StandardGlyphGroup.GlyphGroupVariable;
                    break;
                case Kind.Event:
                    imgG = StandardGlyphGroup.GlyphGroupEvent;
                    break;
                case Kind.Delegate:
                    imgG = StandardGlyphGroup.GlyphGroupDelegate;
                    break;
                case Kind.Enum:
                    imgG = StandardGlyphGroup.GlyphGroupEnum;
                    break;
                case Kind.EnumMember:
                    imgG = StandardGlyphGroup.GlyphGroupEnumMember;
                    break;
                case Kind.Operator:
                    imgG = StandardGlyphGroup.GlyphGroupOperator;
                    break;
                case Kind.Interface:
                    imgG = StandardGlyphGroup.GlyphGroupInterface;
                    break;
                case Kind.Namespace:
                    imgG = StandardGlyphGroup.GlyphGroupNamespace;
                    break;
                case Kind.Field:
                case Kind.VOGlobal:
                    imgG = StandardGlyphGroup.GlyphGroupField;
                    break;
                case Kind.Union:
                    imgG = StandardGlyphGroup.GlyphGroupUnion;
                    break;
                case Kind.VODefine:
                    imgG = StandardGlyphGroup.GlyphGroupConstant;
                    break;
                case Kind.VOStruct:
                    imgG = StandardGlyphGroup.GlyphGroupValueType;
                    break;
                case Kind.Keyword:
                    imgG = StandardGlyphGroup.GlyphKeyword;
                    break;
            }
            return imgG;
        }

        public static StandardGlyphItem getGlyphItem(this IXSymbol elt) 
        {
            StandardGlyphItem imgI = StandardGlyphItem.GlyphItemPublic;
            switch (elt.Visibility)
            {
                case Modifiers.Public:
                    imgI = StandardGlyphItem.GlyphItemPublic;
                    break;
                case Modifiers.Protected:
                    imgI = StandardGlyphItem.GlyphItemProtected;
                    break;
                case Modifiers.Private:
                    imgI = StandardGlyphItem.GlyphItemPrivate;
                    break;
                case Modifiers.Internal:
                    imgI = StandardGlyphItem.GlyphItemInternal;
                    break;
                case Modifiers.ProtectedInternal:
                    imgI = StandardGlyphItem.GlyphItemProtected;
                    break;
            }
            if (elt.IsStatic)
            {
                imgI = StandardGlyphItem.GlyphItemShortcut;
            }
            return imgI;
        }
    }


    /// <summary>
    /// Class that contains informations about the Code Element we have found during
    /// type searching.
    /// Used by Goto Definition, Parameter Info, ...
    /// </summary>
    [DebuggerDisplay("{Name,nq} {ReturnType.FullName,nq}")]
    public class CompletionElement
    {
        IXSymbol foundElement = null;
        public bool IsSourceElement => SearchLocation != null;
        string genTypeName;
        public XSourceSymbol SourceElement => foundElement as XSourceSymbol;
        public XSourceSymbol SearchLocation;
        public CompletionElement(IXSymbol element)
        {
            this.foundElement = element;
            if (element is XSourceSymbol)
            {
                this.SearchLocation = (XSourceSymbol)element;
            }
        }
        public CompletionElement(IXSymbol element, XFile file)
        {
            this.foundElement = element;
            if (element is XSourceSymbol)
            {
                this.SearchLocation = (XSourceSymbol)element;
            }
        }
        public void OpenSource()
        {
            if (IsSourceElement)
            {
                SourceElement.OpenEditor();
            }
        }

        public bool IsInitialized => this.foundElement != null;

        public IXSymbol Result => foundElement ;

        public string Name => this.foundElement.Name;


        public string TypeName
        {
            get
            {
                var t = ReturnType;
                var name = t.FullName;
                if (t.IsArray)
                    name += "[]";
                return name;
            }
        }
        public CompletionType MemberOf
        {
            get
            {
                CompletionType cType = new CompletionType();
                //if (this.XSharpElement != null)
                //{
                //    if (this.XSharpElement.Parent != null)
                //        cType = new CompletionType(this.XSharpElement.Parent);
                //    else if (this.XSharpElement.ParentName != null)
                //    {
                //        var defaultNS = "";
                //        if (!String.IsNullOrEmpty(this.XSharpElement.ParentName))
                //        {
                //            defaultNS = this.XSharpElement.ParentName;
                //        }
                //        cType = new CompletionType(this.XSharpElement.ParentName, this.XSharpElement.File, defaultNS);
                //    }
                //    else
                //        cType = new CompletionType("System.Object", null, "");
                //}
                return cType;
            }
        }

        public CompletionType ReturnType
        {
            get
            {
                CompletionType cType;
                
                if (IsSourceElement)
                {
                    string searchTypeName = foundElement.TypeName;
                    var usings = new List<String>();
                    usings.AddRange(SearchLocation.FileUsings);
                    var sourceElement = this.SourceElement;
                    if (sourceElement is IXMemberSymbol)
                    {
                       var member = (IXMemberSymbol)sourceElement;
                        var parts = member.FullName.Split(new char[]{ '.'});
                        string ns = "";
                        foreach (var part in parts)
                        {
                            ns = ns + part;
                            if (!usings.Contains(ns))
                                usings.Add(ns);
                            ns += ".";
                        }
                    }
                    cType = new CompletionType(searchTypeName, SearchLocation.File, usings);
                }
                else
                {
                    if (foundElement is IXMemberSymbol)
                    {
                        cType = new CompletionType(foundElement as IXMemberSymbol);
                    }
                    else // if (foundElement is IXTypeSymbol)
                    {
                        cType = new CompletionType(foundElement as IXTypeSymbol);
                    }
                }
                return cType;
            }
        }

        public string GenericTypeName
        {
            get
            {
                string ret = "";
                if (this.IsGeneric)
                {
                    if (this.genTypeName == null)
                    {
                        string searchTypeName = "";
                        if ((this.foundElement is XSourceMemberSymbol) && (this.Result.Kind.HasReturnType()))
                        {
                            XSourceMemberSymbol xt = (XSourceMemberSymbol)this.foundElement;
                            searchTypeName = xt.TypeName;
                        }
                        else if (this.foundElement is XSourceVariableSymbol xv)
                        {
                            searchTypeName = xv.TypeName;
                        }
                        if (!string.IsNullOrEmpty(searchTypeName))
                        {
                            int genMarker = searchTypeName.IndexOf("<");
                            if (genMarker > -1)
                            {
                                searchTypeName = searchTypeName.Substring(genMarker + 1);
                                searchTypeName = searchTypeName.Substring(0, searchTypeName.Length - 1);
                                this.genTypeName = searchTypeName;
                                ret = this.genTypeName;
                            }
                        }
                    }
                    else
                    {
                        ret = this.genTypeName;
                    }
                }
                return ret; 
            }

            set
            {
                this.genTypeName = value;
            }
        }

        public bool IsArray
        {
            get
            {
                if (foundElement != null)
                    return foundElement.IsArray;
                return false;
            }
        }

        public bool IsGeneric
        {
            get
            {
                if (foundElement != null)
                {
                    var type = foundElement.TypeName;
                    if (type != null)
                        return type.EndsWith(">");
                }
                return false;
            }
        }

    }

    // Build a list of all Keywords
    internal static class XSharpTypes
    {
        static IList<IXSymbol> _keywords;
        static IList<IXSymbol> _types;

        static XSharpTypes()
        {
            // Dummy call to a Lexer; just to copy the Keywords, Types, ...
            // Pass default options so this will be the core dialect and no
            // 4 letter abbreviations will be in the list
            var lexer = XSharpLexer.Create("", "", XSharpParseOptions.Default);
            //

            var keywords = new List<IXSymbol>();
            var types = new List<IXSymbol>();
            //
            foreach (var keyword in lexer.KwIds)
            {
                keywords.Add(new XSourceSymbol(keyword.Key, Kind.Keyword, Modifiers.None));
                if (XSharpLexer.IsType(keyword.Value))
                {
                    types.Add(new XSourceSymbol(keyword.Key, Kind.Keyword, Modifiers.None));
                }
            }
            //
            _keywords = keywords.ToArray();
            _types = types.ToArray();
        }

        internal static IList<IXSymbol> Get()
        {
            return _keywords;
        }
        internal static IList<IXSymbol> GetTypes()
        {
            return _types;
        }
    }
    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }
    internal static class MemberExtensions
    {
        /// <summary>
        /// Retrieve the locals for a particular member
        /// </summary>
        /// <param name="member"></param>
        /// <param name="snapshot"></param>
        /// <param name="iCurrentLine"></param>
        /// <returns></returns>
        internal static IList<XSourceVariableSymbol> GetLocals(this XSourceMemberSymbol member, ITextSnapshot snapshot, int iCurrentLine, XSharpDialect dialect)
        {
            iCurrentLine = Math.Min(snapshot.LineCount - 1, iCurrentLine);
            // create a walker with just the contents of the current member
            // use a new file object so we will not destroy the types in the existing object
            var walker = new SourceWalker( new XFile ( member.File.FullPath, member.File.Project));
            var start = member.Interval.Start;
            var end = member.Interval.Width;
            if (start + end > snapshot.Length)
                end = snapshot.Length - start;
            var memberSource = snapshot.GetText(start, end);
            
            var locals = walker.ParseLocals(memberSource, member);
            // Add the normal locals for class members
            foreach(var local in locals)
            {
                // assign the current member so we will have the proper Parent as well
                local.Parent = member;
                if (local is XSourceSymbol )
                {
                    ((XSourceSymbol)local).File = member.File;
                }
            }
            if (member.Kind.IsClassMember(dialect) && !member.Modifiers.HasFlag(Modifiers.Static))
            {
                var XVar = new XSourceVariableSymbol(member, "SELF", member.Range, member.Interval, member.ParentName);
                XVar.File = walker.File;
                locals.Add(XVar);
                if (member.ParentType.BaseType != null)
                {
                    XVar = new XSourceVariableSymbol(member, "SUPER", member.Range, member.Interval, member.ParentType.BaseType);
                    XVar.File = walker.File;
                    locals.Add(XVar);
                }
            }
            return locals;
        }
    }

 }




