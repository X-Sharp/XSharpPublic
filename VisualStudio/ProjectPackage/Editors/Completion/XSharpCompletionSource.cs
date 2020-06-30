//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;
using System.Diagnostics;
using System.Collections.Immutable;
using XSharpColorizer;
using XSharp.Project.OptionsPages;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.Text.Tagging;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;
using XSharp.Project;

namespace XSharpLanguage
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("XSharp")]
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
        internal static IntellisenseOptionsPage _optionsPage;
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
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            _optionsPage = package.GetIntellisenseOptionsPage();
        }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (_optionsPage.EnableCodeCompletionLog && _optionsPage.EnableOutputPane)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage(strMessage);
            }
        }
        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            WriteOutputMessage("-->> AugmentCompletionSessions");
            try
            {
                if (_optionsPage.DisableCodeCompletion)
                    return;
                XSharpModel.ModelWalker.Suspend();
                if (_disposed)
                    throw new ObjectDisposedException("XSharpCompletionSource");
                _showTabs = _optionsPage.CompletionListTabs;
                _keywordsInAll = _optionsPage.KeywordsInAll;
                if (_dialect == XSharpDialect.FoxPro)
                {
                    _dotUniversal = true;
                }
                else if (_dialect == XSharpDialect.Core)
                {
                    _dotUniversal = _optionsPage.UseDotAsUniversalSelector;
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
                uint cmd = (uint)session.Properties["Command"];
                VSConstants.VSStd2KCmdID nCmdId = (VSConstants.VSStd2KCmdID)cmd;
                char typedChar = (char)session.Properties["Char"];
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
                //while (start > line.Start && !char.IsWhiteSpace((start - 1).GetChar()))
                //{
                //    start -= 1;
                //}
                //
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
                if (session.Properties.ContainsProperty("Type"))
                {
                    cType = (CompletionType)session.Properties["Type"];
                }
                // Start of Process
                string filterText = "";
                // Check if we can get the member where we are
                int currentLine = triggerPoint.GetContainingLine().LineNumber;
                XMemberDefinition member = XSharpTokenTools.FindMember(currentLine, this._file);
                XTypeDefinition currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, this._file);
                // Standard TokenList Creation (based on colon Selector )
                List<string> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, currentLine, _buffer.CurrentSnapshot, out _stopToken, false, _file, false, member);
                // We might be here due to a COMPLETEWORD command, so we have no typedChar
                // but we "may" have a incomplete word like System.String.To
                // Try to Guess what TypedChar could be
                if (typedChar == '\0')
                {
                    if (tokenList.Count > 0)
                    {
                        string extract = tokenList[tokenList.Count - 1];
                        typedChar = extract[extract.Length - 1];
                        if ((typedChar != '.') && (typedChar != ':'))
                        {
                            if (tokenList.Count == 1)
                            {
                                //
                                filterText = tokenList[0];
                                int dotPos = extract.LastIndexOf(".");
                                if (dotPos > -1)
                                {
                                    string startToken = extract.Substring(0, dotPos);
                                    filterText = extract.Substring(dotPos + 1);
                                    typedChar = '.';
                                    tokenList[0] = startToken + ".";
                                }
                            }
                            else
                            {
                                // So, we get the last Token as a Filter
                                filterText = tokenList[tokenList.Count - 1];
                            }
                            // but what could be the typedChar?
                            if (tokenList.Count > 1)
                            {
                                extract = tokenList[tokenList.Count - 2];
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
                List<string> altTokenList;
                if (dotSelector && _dotUniversal)
                    altTokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, currentLine, _buffer.CurrentSnapshot, out _stopToken, false, _file, true, member);
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
                cType = XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, _dialect);
                if (!cType.IsEmpty())
                {
                    session.Properties["Type"] = cType;
                }
                else
                {
                    if (dotSelector && _dotUniversal)
                    {
                        cType = XSharpTokenTools.RetrieveType(_file, altTokenList, member, currentNS, null, out foundElement, snapshot, currentLine, _dialect);
                        if (!cType.IsEmpty())
                        {
                            session.Properties["Type"] = cType;
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
                                    cType = new CompletionType(((XTypeDefinition) (member.Parent)).Clone);
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
                XSharpProjectPackage.Instance.DisplayException(ex);
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
                if (XSharpTokenTools.isGenerated(type))
                   continue;

                string realTypeName = type.FullName;
                if (IsHiddenTypeName(realTypeName))
                {
                    continue;
                }
                TypeAnalysis typeAnalysis = new TypeAnalysis(type);
                // Nested Type ?
                if (realTypeName.Contains("+"))
                {
                    realTypeName = realTypeName.Replace('+', '.');
                }
                // remove the start
                if (startLen > 0 && realTypeName.Length > startLen && realTypeName.StartsWith(startWith, StringComparison.OrdinalIgnoreCase))
                    realTypeName = realTypeName.Substring(startLen);
                // Do we have another part file
                dotPos = realTypeName.IndexOf('.');
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
            if (realTypeName.Length > 2 && realTypeName.StartsWith("__", StringComparison.Ordinal) && _optionsPage.HideAdvancemembers)
                return true;
            if (realTypeName.IndexOf('$') >= 0)
                return true;
            return false;
        }


        private bool IsHiddenMemberName(string realMemberName)
        {
            if (realMemberName.Length > 2 && realMemberName.StartsWith("__", StringComparison.Ordinal) && _optionsPage.HideAdvancemembers)
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

        private void BuildCompletionList(CompletionList compList, XMemberDefinition currentMember, string startWith, int currentLine)
        {
            if (currentMember == null)
            {
                return;
            }
            // First, look after Parameters
            foreach (XVariable paramVar in currentMember.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(paramVar.getGlyphGroup(), paramVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Prototype, icon, null, Kind.Parameter,"")))
                    break;
            }
            // Then, look for Locals
            foreach (XVariable localVar in currentMember.GetLocals(_buffer.CurrentSnapshot, currentLine, _dialect).Where(l => nameStartsWith(l.Name, startWith)))
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
                var classElement = currentMember.Parent as XTypeDefinition;
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
        /// <param name="parent">The XTypeDefinition element</param>
        /// <param name="minVisibility">The minimum Visibility</param>
        /// <param name="staticOnly">Static member only ?</param>
        /// <param name="startWith">The filter text</param>
        private void BuildCompletionList(CompletionList compList, XElement parent, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (parent == null)
            {
                return;
            }
            if (!(parent is XTypeDefinition))
            {
                return;
            }
            //
            IXType Owner = parent as IXType;
            //
            bool hideAdvanced = _optionsPage.HideAdvancemembers;
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
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
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
        private void FillMembers(CompletionList compList, IEnumerable<IXMember> members, Modifiers minVisibility, bool staticOnly)
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
                            if (elt is XMemberDefinition)
                            {
                                var member = (XMemberDefinition)elt;
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
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
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
        private void FillMembers(CompletionList compList, IXType xType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            FillMembers(compList, xType.GetMembers(startWith), minVisibility, staticOnly);
        }


        private void FillExtensions(CompletionList compList, CompletionType cType, string startWith)
        {
            //WriteOutputMessage($"FillExtensions for type {sType.FullName}");
            if (cType.Type != null)
            {
                var extensions = _file.Project.GetExtensions(cType.Type.FullName);
                IEnumerable<IXMember> selection = extensions;
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
        private string TokenListAsString(List<string> tokenList, int less)
        {
            string retValue = "";
            for (int pos = 0; pos < tokenList.Count - less; pos++)
            {
                string tk = tokenList[pos];
                retValue += tk;
            }
            return retValue;
        }
    }
    public class MemberAnalysis
    {
        IntellisenseOptionsPage _optionsPage => XSharp.Project.XSharpProjectPackage.Instance.GetIntellisenseOptionsPage();
        public class ParamInfo
        {
            public string Name;
            public string TypeName;
            public string Direction;
            public bool Optional;

            internal ParamInfo(string n, string t, string dir)
            {
                this.Name = n;
                this.TypeName = t;
                this.Direction = dir;
            }
            internal ParamInfo(System.Reflection.ParameterInfo p)
            {
                this.Name = p.Name;
                if (p.IsIn)
                {
                    Direction = "IN";
                }
                else if (p.IsOut)
                {
                    Direction = "OUT";
                }
                else
                {
                    Direction = "AS";
                    string type = p.ParameterType.FullName;
                    if (type == null)
                        type = p.ParameterType.Name;
                    if (type != null && type.EndsWith("&"))
                        Direction = "REF";
                }

                this.TypeName = p.ParameterType.GetXSharpTypeName();
                Optional = p.IsOptional;
            }
        }


        public string Name { get; private set; }
        public Modifiers Modifiers  { get; private set; }
        public Modifiers Visibility { get; private set; }
        public Kind Kind { get; private set; }
        public bool IsStatic { get; private set; }
        public string TypeName { get; private set; }
        public IList<IXVariable> Parameters { get; private set; }
        public string Value { get; private set; }

        /// <summary>
        /// Process a MemberInfo in order to provide usable informations ( TypeName, Glyph, ... )
        /// </summary>
        internal MemberAnalysis(IXMember member)
        {
            this.Name = member.Name;
            this.Kind = member.Kind;
            this.Modifiers = member.Modifiers;
            this.Visibility = member.Visibility;
            this.TypeName = "";
            this.Parameters = member.Parameters;
            this.Value = member.Value;
        }
      
    }

    /// <summary>
    /// Process a TypeInfo in order to provide usable informations (TypeName, Glyph, ... )
    /// </summary>
    public class TypeAnalysis
    {
        public string Name { get; private set; }
        public Modifiers Modifiers { get; private set; }
        public Modifiers Visibility { get; private set; }
        public Kind Kind { get; private set; }
        public bool IsStatic { get; private set; }
        IntellisenseOptionsPage _optionsPage => XSharp.Project.XSharpProjectPackage.Instance.GetIntellisenseOptionsPage();

        internal TypeAnalysis(IXType typeInfo)
        {
            //
            if (typeInfo == null)
                return;
            this.Name = typeInfo.FullName;
            this.Kind = typeInfo.Kind;
            this.Modifiers = typeInfo.Modifiers;
            this.Visibility = typeInfo.Visibility;
            if (Visibility == Modifiers.None)
            {
                Visibility = Modifiers.Public;
            }
            this.IsStatic = typeInfo.IsStatic;
            //
           
            //
            if (typeInfo.IsGeneric)
            {
                string genName = typeInfo.FullName;
                int index = genName.IndexOf('`');
                if (index != -1)
                {
                    genName = genName.Substring(0, index);
                }
                genName += "<";
                int count = 0;
                int max = typeInfo.TypeParameters.Count;
                foreach (var genType in typeInfo.TypeParameters)
                {
                    genName += genType;
                    count++;
                    if ((count < max))
                        genName += ", ";
                }
                genName += ">";
                //
                this.Name = genName;
            }
   
        }

        public string Prototype
        {
            get
            {
                return this.Name;
                //
            }
        }

        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG;
                //
                switch (this.Kind)
                {
                    case Kind.Class:
                    default:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
                        break;
                    case Kind.Interface:
                        imgG = StandardGlyphGroup.GlyphGroupInterface;
                        break;
                    case Kind.Enum:
                        imgG = StandardGlyphGroup.GlyphGroupEnum;
                        break;
                    case Kind.Delegate:
                        imgG = StandardGlyphGroup.GlyphGroupDelegate;
                        break;

                }
                return imgG;
            }
        }

        /// <summary>
        /// Glyph Item used by CompletionList in CompletionSource
        /// - See also GlyphGroup
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphItem GlyphItem
        {
            get
            {
                StandardGlyphItem imgI;
                //
                switch (this.Visibility)
                {
                    case Modifiers.Public:
                    default:
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
                        imgI = StandardGlyphItem.GlyphItemFriend;
                        break;

                }
                //
                return imgI;
            }
        }

     
    }
    /// <summary>
    /// XSharp CompletionList
    /// Overload the Add() Method to support "overloads"
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
                if (item.Kind == Kind.Method && !string.Equals(found.Description, item.Description, StringComparison.OrdinalIgnoreCase))
                {
                    int overloads = 0;
                    // Already exists in the List !!
                    // First Overload ?
                    var comp = this[item.DisplayText];
                    if (comp.Properties.ContainsProperty("overloads"))
                    {
                        // No ...
                        overloads = (int)comp.Properties.GetProperty("overloads");
                    }
                    else
                    {
                        comp.Properties.AddProperty("overloads", overloads);
                    }
                    overloads += 1;
                    // Set the number of Overload(s)
                    comp.Properties["overloads"] = overloads;
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
                if (this.Properties.ContainsProperty("overloads"))
                {
                    // No ...
                    overloads = (int)this.Properties.GetProperty("overloads");
                }
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
    /// Static class Tools. Offer services to get TokenList, Search members, ...
    /// </summary>
    public static class XSharpTokenTools
    {
        public static bool isGenerated(IXType type)
        {
            return type.Name.IndexOf("$", StringComparison.Ordinal) > -1;
        }
        public static bool isGenerated(IXMember m)
        {
            return m.Name.IndexOf("$") > -1 || m.Name.StartsWith("<");

        }

        public static bool StringEquals(string lhs, string rhs)
        {
            return XSharpCompletionSource.StringEquals(lhs, rhs);
        }

        /// <summary>
        /// Retrieve a List of Token, based on a position in buffer.
        /// Moving back in the buffer, all tokens are retrieved and stored.
        /// </summary>
        /// <param name="triggerPointPosition">The position in the buffer </param>
        /// <param name="triggerPointLineNumber"></param>
        /// <param name="bufferText"></param>
        /// <param name="stopToken">The IToken that stops the move backwards</param>
        /// <param name="fromGotoDefn">Indicate if the call is due to Goto Definition</param>
        /// <param name="file">XFile object to use for the context</param>
        /// <param name="dotAsSelector">dot is used as 'standard' selector, like colon </param>
        /// <param name="fromMember">The Member containing the position</param>
        /// <returns></returns>
        public static List<string> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            ITextSnapshot snapshot, out IToken stopToken, bool fromGotoDefn, XFile file, bool dotAsSelector, XMemberDefinition fromMember)
        {
            var bufferText = snapshot.GetText();
            return GetTokenList(triggerPointPosition, triggerPointLineNumber, bufferText, out stopToken, fromGotoDefn, file, dotAsSelector, fromMember);
        }

        public static List<string> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            string bufferText, out IToken stopToken, bool fromGotoDefn, XFile file, bool dotAsSelector, XMemberDefinition fromMember)
        {
            //////////////////////////////////////
            //////////////////////////////////////
            // Try to speedup the process, Tokenize only the Member source if possible (and not the FULL source text)
            if (fromMember != null && fromMember.Interval.Start < triggerPointPosition && fromMember.Kind.HasBody())
            {
                // if the triggerpoint is after the end of the member then the member information is old and
                // we should use that position to determine the end of the buffer to scan
                // And make sure we also add some more because that does not hurt.
                int nWidth;
                if (triggerPointPosition > fromMember.Interval.Stop)
                {
                    nWidth = triggerPointPosition - fromMember.Interval.Start + 500;
                }
                else
                {
                    nWidth = fromMember.Interval.Width + 500;
                }
                nWidth = Math.Min(nWidth, bufferText.Length - fromMember.Interval.Start);
                bufferText = bufferText.Substring(fromMember.Interval.Start, nWidth);
                // Adapt the positions.
                triggerPointPosition = triggerPointPosition - fromMember.Interval.Start;
                triggerPointLineNumber = triggerPointLineNumber - (fromMember.Range.StartLine - 1);
            }
            else
            {
                // no need to parse the whole buffer. It could be huge
                int maxLen = triggerPointPosition + 500;
                if (maxLen > bufferText.Length)
                    maxLen = bufferText.Length;
                bufferText = bufferText.Substring(0, maxLen);
                // no need to adjust the line and position since we are working from the start of the buffer
            }


            ITokenStream tokenStream;
            var reporter = new ErrorIgnorer();
            // Get compiler options
            XSharpParseOptions parseoptions;
            string fileName;
            if (file != null)
            {
                var prj = file.Project.ProjectNode;
                parseoptions = prj.ParseOptions;
                fileName = file.FullPath;
            }
            else
            {
                parseoptions = XSharpParseOptions.Default;
                fileName = "MissingFile.prg";
            }


            bool ok = Lex(bufferText, fileName, parseoptions, reporter, out tokenStream);
            var stream = tokenStream as BufferedTokenStream;
            return GetTokenList(triggerPointPosition, triggerPointLineNumber, stream, out stopToken, fromGotoDefn, file, dotAsSelector, fromMember);
        }


        public static List<string> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            BufferedTokenStream tokens, out IToken stopToken, bool fromGotoDefn, XFile file, bool dotAsSelector, XMemberDefinition fromMember)
        {
            List<string> tokenList = new List<string>();
            string token;
            IToken lastToken = null;
            //
            stopToken = null;
            if (tokens == null)
                return tokenList;

            // locate the last token before the trigger point
            // Use binary search in stead of linear search
            var list = tokens.GetTokens();
            int current = 0;
            int bottom = 0;
            int top = list.Count;
            while (top - bottom > 1)
            {
                // determine middle
                current = (bottom + top) / 2;
                var check = list[current];
                if (check.StartIndex > triggerPointPosition)
                {
                    top = current;
                }
                else if (check.StartIndex < triggerPointPosition)
                {
                    bottom = current;
                }
                else
                {
                    break;
                }
            }
            if (current > list.Count - 1 || current < 0)
                return tokenList;
            // on the right row now. Find the first token on the row
            for (int iToken = current; iToken >= 0; iToken--)
            {
                // Out tokens line numbers are 1 based
                if (list[iToken].Line != triggerPointLineNumber + 1)
                    break;
                current = iToken;
            }
            // now look forward and find the first token that is on or after the triggerpoint
            IToken nextToken = list[current];
            while (true)
            {
                current++;
                if (list[current].StartIndex < triggerPointPosition)
                    nextToken = list[current];
                else
                    break;
            }
            // nextToken now points to the first token that has its startindex >= triggerPointPosition

            //////////////////////////////////////
            // We are looking at line
            // Out tokens line numbers are 1 based
            int lineTP = triggerPointLineNumber + 1;
            // And we are on line
            int lineNT = nextToken.Line;
            //
            if (lineNT != lineTP)
            {
                // should not happen.
                //return tokenList;
            }
            // Not a CRLF ? Ok, should be the next one...except if the next one is CRLF !!
            if (nextToken.Type != XSharpLexer.EOS)
            {
                //if ( list[((XSharpToken)nextToken).OriginalTokenIndex + 1].Type != XSharpLexer.EOS )
                nextToken = list[((XSharpToken)nextToken).OriginalTokenIndex + 1];
                while (nextToken.Channel != XSharpLexer.DefaultTokenChannel && nextToken.Type != XSharpLexer.EOS)
                {
                    nextToken = list[((XSharpToken)nextToken).OriginalTokenIndex + 1];
                }
            }
            if (stopToken == null)
            {
                stopToken = nextToken;
            }
            // Now, let's build the Token chain, so we can guess what to add in the CompletionList
            IToken triggerToken = null;
            token = "";
            switch (nextToken.Type)
            {
                case XSharpLexer.LPAREN:
                    token = "()";
                    break;
                case XSharpLexer.LCURLY:
                    token = "{}";
                    break;
                case XSharpLexer.LBRKT:
                    token = "[]";
                    break;
            }
            if (!string.IsNullOrEmpty(token))
                tokenList.Add(token);
            triggerToken = GetPreviousToken(tokens, nextToken);
            //
            bool inCtor = false;
            while (triggerToken != null)
            {
                lastToken = triggerToken;
                token = triggerToken.Text;
                if (triggerToken.Channel != XSharpLexer.DefaultTokenChannel)
                {
                    triggerToken = GetPreviousToken(tokens, triggerToken);
                    continue;
                }
                switch (triggerToken.Type)
                {
                    // For ) ] }, we will search the counter part, and remove all stuff in between
                    case XSharpLexer.RPAREN:
                        // Search for the Left Parenthesis
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LPAREN, XSharpLexer.RPAREN);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "()";
                        break;
                    case XSharpLexer.RBRKT:
                        // Search for the Left Bracket
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LBRKT, XSharpLexer.RBRKT);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "[]";
                        break;
                    case XSharpLexer.RCURLY:
                        // Search for the Left Curly
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LCURLY, XSharpLexer.RCURLY);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "{}";
                        inCtor = true;
                        break;
                    case XSharpLexer.ASSIGN_OP:
                    //case XSharpLexer.COMMA:
                    case XSharpLexer.USING:
                    case XSharpLexer.AS:
                    case XSharpLexer.IS:
                    case XSharpLexer.REF:
                    case XSharpLexer.IMPLEMENTS:
                    case XSharpLexer.INHERIT:
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.SL_COMMENT:
                    case XSharpLexer.ML_COMMENT:
                    case XSharpLexer.DOC_COMMENT:
                        //case XSharpLexer.VAR:
                        //case XSharpLexer.IMPLIED:
                        // Stop here
                        stopToken = triggerToken;
                        token = null;
                        if (fromGotoDefn)
                        {
                            switch (triggerToken.Type)
                            {
                                case XSharpLexer.LPAREN:
                                    token = "()";
                                    break;
                                case XSharpLexer.LCURLY:
                                    token = "{}";
                                    inCtor = true;
                                    break;
                                case XSharpLexer.LBRKT:
                                    token = "[]";
                                    break;
                                default:
                                    token = null;
                                    triggerToken = null;
                                    break;
                            }
                            // One shot
                            fromGotoDefn = false;
                        }
                        else
                            triggerToken = null;
                        break;
                    case XSharpLexer.DOT:
                    case XSharpLexer.COLON:
                    case XSharpLexer.COLONCOLON:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        break;
                    default:
                        // allow FLoat{} or String{}
                        if (XSharpLexer.IsType(triggerToken.Type))
                            break;
                        else if (XSharpLexer.IsKeyword(triggerToken.Type))
                        {
                            token = null;
                            triggerToken = null;
                            break;
                        }
                        else if (XSharpLexer.IsOperator(triggerToken.Type) && !inCtor)
                        {
                            token = null;
                            triggerToken = null;
                        }
                        // Simple EQUAL in FoxPro could mean ASSIGN_OP
                        else if ((triggerToken.Type == XSharpLexer.EQ) && (file.Project.Dialect == XSharpDialect.FoxPro))
                        {
                            // Stop here
                            stopToken = triggerToken;
                            token = null;
                            triggerToken = null;
                        }
                        break;
                }
                //
                if (token != null)
                    tokenList.Add(token);
                //
                if (triggerToken != null)
                {
                    var last = triggerToken;
                    triggerToken = GetPreviousToken(tokens, triggerToken);
                    // BOF reached
                    if (last == triggerToken)
                        break;
                }

            }
            // Hack to hanlde :: / COLONCOLON with something in front
            if ((tokenList.Count > 1) && (tokenList[0] == "::"))
            {
                tokenList[0] = ":";
            }
            //
            tokenList.Reverse();
            if (tokenList.Count > 0)
            {
                // First token
                token = tokenList[0];
                // Could be a COLON or a DOT, it will could be the case in a WITH...END construct
                if ((token.CompareTo(":") == 0) || (token.CompareTo(".") == 0))
                {
                    bool inWith = false;
                    IToken lastID = null;
                    XSharpToken temp;
                    int withLine = -1;
                    int indexWith;
                    // We stopped there
                    triggerToken = lastToken;
                    // search for WITH....
                    while (triggerToken != null)
                    {
                        triggerToken = GetPreviousToken(tokens, triggerToken, false);
                        // We should check that we are not getting out of the current member.....
                        if (triggerToken.StartIndex < fromMember.Interval.Start)
                        {
                            triggerToken = null;
                            break;
                        }
                        switch (triggerToken.Type)
                        {
                            case XSharpLexer.WITH:
                                inWith = true;
                                withLine = triggerToken.Line;
                                temp = (XSharpToken)triggerToken;
                                indexWith = temp.OriginalTokenIndex;
                                triggerToken = null;
                                break;
                            case XSharpLexer.ID:
                                lastID = triggerToken;
                                break;
                        }
                    }
                    //
                    if (inWith)
                    {
                        // So now, get the ID associated with WITH
                        if (lastID != null)
                        {
                            // Guess that the ID is on the same line as the WITH
                            if (lastID.Line == withLine)
                            {
                                temp = (XSharpToken)lastID;
                                //indexWith = temp.OriginalTokenIndex;
                                // Ok, let's guess that the LastID is the one used by WITH
                                // SO, inject it in the beginning of the list
                                tokenList.Insert(0, lastID.Text);
                            }
                        }
                    }
                }
            }
            // Now, we may have some post-treatment
            List<string> returnList = new List<string>();
            int i = 0;
            bool prevWasDot = false;
            while (i < tokenList.Count)
            {
                token = tokenList[i];
                if ((token.CompareTo("()") == 0) || (token.CompareTo("{}") == 0) || (token.CompareTo("[]") == 0))
                {
                    if (returnList.Count > 0)
                    {
                        string prevToken = returnList[returnList.Count - 1];
                        if (prevToken == ">")
                        {
                            // Do we have a "<" ?
                            int leftGen = 0;
                            while (leftGen < returnList.Count)
                            {
                                if (returnList[leftGen] == "<")
                                {
                                    break;
                                }
                                leftGen++;
                            }
                            if ((leftGen < returnList.Count) && (leftGen > 0))
                            {
                                List<string> dummyList = new List<string>();
                                for (int j = 0; j < leftGen; j++)
                                    dummyList.Add(returnList[j]);
                                // Now, concat up to ">"
                                string genType = "";
                                for (int j = leftGen; j <= returnList.Count - 1; j++)
                                {
                                    genType += returnList[j];
                                }
                                //
                                genType += token;
                                dummyList[dummyList.Count - 1] = dummyList[dummyList.Count - 1] + genType;
                                returnList = dummyList;
                            }
                        }
                        else
                        {
                            prevToken = prevToken + token;
                            returnList[returnList.Count - 1] = prevToken;
                        }
                    }
                }
                else if ((file.Project.Dialect == XSharpDialect.XPP) && (token.CompareTo("::") == 0))
                {
                    returnList.Add("SELF");
                    returnList.Add(":");
                }
                else if (file.Project.Dialect == XSharpDialect.FoxPro)
                {
                    string lowerToken = token.ToLower();
                    switch (lowerToken)
                    {
                        case "this":
                            // THIS will turn to a SELF
                            returnList.Add("SELF");
                            break;
                        case ".":
                            // dot in first place will mean SELF.
                            if (returnList.Count == 0)
                            {
                                returnList.Add("SELF");
                            }
                            returnList.Add(token);
                            break;
                        default:
                            returnList.Add(token);
                            break;
                    }
                }
                else if ((token.CompareTo(".") == 0) && !dotAsSelector)
                {
                    if (returnList.Count > 0)
                    {
                        string prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                        prevWasDot = true;
                    }
                }
                else
                {
                    if (prevWasDot)
                    {
                        string prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                        prevWasDot = false;
                    }
                    else
                        returnList.Add(token);
                }
                i++;
            }
            //
            return returnList;
        }
        public static XTypeDefinition FindNamespace(int position, XFile file)
        {
            if (file == null)
            {
                return null;
            }
            if (file.TypeList == null)
                return null;
            //
            XTypeDefinition found = null;
            foreach (XTypeDefinition eltType in file.TypeList.Values)
            {

                if (eltType.Interval.ContainsInclusive(position))
                {
                    switch (eltType.Kind)
                    {
                        case Kind.Namespace:
                            return eltType;
                        case Kind.Class:
                        case Kind.Interface:
                        case Kind.Structure:
                        case Kind.Enum:
                            found = eltType;
                            break;
                    }
                }
            }
            //
            if (found != null)
            {
                string name = found.Name;
                if (found.Namespace?.Length > 0)
                    name = found.Namespace + "." + name;
                var pos = name.LastIndexOf('.');
                if (pos > 0)
                {
                    name = name.Substring(0, pos);
                }
                XTypeDefinition nSpace = new XTypeDefinition(name, Kind.Namespace, Modifiers.Public, found.Range, found.Interval, file);
                return nSpace;
            }
#if TRACE
                // a source file without a namespace is really not a problem
                //Support.Debug(String.Format("Cannot find namespace at position {0} in file {0} .", position, fileName));
#endif
            return null;
        }
        public static XMemberDefinition FindMemberAtPosition(int nPosition, XFile file)
        {
            if (file == null || file.EntityList == null)
            {
                return null;
            }
            var member = file.FindMemberAtPosition(nPosition);
            if (member is XMemberDefinition)
            {
                return member as XMemberDefinition;
            }
            // if we can't find a member then look for the global type in the file
            // and return its last member
            var xType = file.TypeList.FirstOrDefault();
            if (xType.Value != null)
            {
                return xType.Value.XMembers.LastOrDefault();
            }
            WriteOutputMessage(string.Format("Cannot find member at 0 based position {0} in file {0} .", nPosition, file.FullPath));
            return null;

        }

        public static XMemberDefinition FindMember(int nLine, XFile file)
        {
            if (file == null || file.EntityList == null)
            {
                return null;
            }
            var member = file.FindMemberAtRow(nLine);
            if (member is XMemberDefinition)
            {
                return member as XMemberDefinition;
            }
            if (member is XTypeDefinition)
            {
                var xtype = member as XTypeDefinition;
                if (xtype.Members.Count > 0)
                {
                    return xtype.Members.LastOrDefault() as XMemberDefinition;
                }
            }
            // try a few rows before
            member = file.FindMemberAtRow(Math.Max(nLine - 10, 1));
            if (member is XMemberDefinition)
            {
                return member as XMemberDefinition;
            }
            if (member is XTypeDefinition)
            {
                var xtype = member as XTypeDefinition;
                if (xtype.XMembers.Count > 0)
                {
                    return xtype.XMembers.LastOrDefault();
                }
            }

            // if we can't find a member then look for the global type in the file
            // and return its last member
            var ent = file.EntityList.LastOrDefault();
            if (ent is XMemberDefinition)
                return ent as XMemberDefinition;
            if (ent is XTypeDefinition)
                return ((XTypeDefinition)ent).XMembers.LastOrDefault();


#if DEBUG
            WriteOutputMessage(string.Format("Cannot find member at 0 based line {0} in file {0} .", nLine, file.FullPath));
#endif
            return null;
        }

        /// <summary>
        /// Retrieve the CompletionType based on :
        ///  The Token list returned by GetTokenList()
        ///  The Token that stops the building of the Token List.
        /// </summary>
        /// <param name="file"></param>
        /// <param name="tokenList"></param>
        /// <param name="currentMember"></param>
        /// <param name="currentNS"></param>
        /// <param name="stopToken"></param>
        /// <param name="foundElement"></param>
        /// <returns></returns>
        public static CompletionType RetrieveType(XFile file, List<string> tokenList, XMemberDefinition currentMember, string currentNS,
            IToken stopToken, out CompletionElement foundElement, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect)
        {
            //
#if TRACE
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
            foundElement = null;
            CompletionType cType = null;
            int currentPos = 0;
            var startOfExpression = true;
            var findConstructor = true;
            string currentToken = "";
            var currentScopes = new List<string>();
            IXEntity scope;
            scope = currentMember;
            while (scope != null)
            {
                string ns = "";
                if (scope is XTypeDefinition && !XTypeDefinition.IsGlobalType( scope))
                {
                     ns = scope.FullName;
                }
                if (scope.Kind == Kind.Namespace)
                {
                    ns = scope.FullName;
                }
                if (ns?.Length > 0)
                {
                    var elements = ns.Split(".".ToCharArray());
                    ns = "";
                    for (int i = 0; i < elements.Length; i++)
                    {
                        if (i > 0)
                            ns += "." + elements[i];
                        else
                            ns = elements[0];
                        if (!currentScopes.Contains(ns))
                        {
                            currentScopes.Add(ns);
                        }
                    }
                }
                scope = scope.Parent;

            }

            //
            if (currentMember == null)
            {
                // try to find the first member in the file
                if (file != null)
                {
                    XElement elt = file.FindMemberAtRow(currentLine);
                    if (elt is XMemberDefinition)
                    {
                        currentMember = (XMemberDefinition)elt;
                    }
                    else if (elt is XTypeDefinition)
                    {
                        // We might be in the Class Declaration !?
                        switch (stopToken.Type)
                        {
                            case XSharpLexer.IMPLEMENTS:
                            case XSharpLexer.INHERIT:
                                if (tokenList.Count == 1)
                                {
                                    currentToken = tokenList[currentPos];
                                    cType = new CompletionType(currentToken, file, ((XTypeDefinition)(elt)).Namespace);
                                }
                                break;
                            default:
                                cType = new CompletionType(elt.Name,file,file.Usings);
                                break;
                        }
                        if (!cType.IsEmpty())
                        {
                            SearchConstructorIn(cType, Modifiers.Private, out foundElement);
                            if (foundElement.Result == null && cType.XTypeDef  != null)
                            {
                                foundElement = new CompletionElement(cType.XTypeDef);
                            }
                            return cType;
                        }
                    }
                }
                //
                if (currentMember == null)
                {
#if TRACE
                    stopWatch.Stop();
                    WriteOutputMessage(string.Format("Retrieve current Type : Member cannot be null."));
#endif
                    return null;
                }
            }
            else
            {
                file = currentMember.File;
            }
            //
            // we have to walk the tokenList, searching for the current Type
            // As we have separators every even token, we will walk by step 2
            CompletionType cTemp = null;
            if (tokenList.Count == 0)
                return null;
            // Context Type....
            cType = new CompletionType((currentMember.ParentType as XTypeDefinition).Clone);
            Modifiers visibility = Modifiers.Private;
            //
            while (currentPos < tokenList.Count)
            {
                currentToken = tokenList[currentPos];
                // Remove the @@ marker
                if (currentToken.StartsWith("@@",StringComparison.Ordinal))
                    currentToken = currentToken.Substring(2);
                //
                var lastToken = currentToken;
                //
                int dotPos = currentToken.LastIndexOf(".");
                if (dotPos > -1)
                {
                    string startToken = currentToken.Substring(0, dotPos);
                    if (string.IsNullOrEmpty(startToken))
                    {
                        currentPos += 1;
                        continue;
                    }
                    cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.Namespace);
                    if (cType.IsEmpty())
                    {
                        // could be namespace.Type
                        // so now try with right side of the string
                        startToken = currentToken.Substring(dotPos + 1);
                        if (!string.IsNullOrEmpty(startToken))
                        {
                            cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.Namespace);
                        }
                    }
                    if (!cType.IsEmpty())
                    {
                        currentToken = currentToken.Substring(dotPos + 1);
                        startOfExpression = false;
                        if (string.IsNullOrEmpty(currentToken))
                        {
                            currentPos += 2;
                            continue;
                        }
                    }
                }
                //
                if (stopToken != null)
                {
                    switch (stopToken.Type)
                    {
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.OUT:
                        case XSharpLexer.ASTYPE:
                        case XSharpLexer.IMPLEMENTS:
                        case XSharpLexer.INHERIT:
                        case XSharpLexer.OF:
                        case XSharpLexer.SHARING:
                        case XSharpLexer.FROM:
                            if (tokenList.Count == 1)
                            {
                                // One Token, after such keyword, this is a type
                                // So we are looking for a Type, and we must end with a {}
                                if (!currentToken.EndsWith("{}"))
                                {
                                    currentToken += "{}";
                                    findConstructor = false;
                                }
                            }
                            break;
                        default:
                            break;
                    }
                }
                //
                if (currentToken.EndsWith("{}"))
                {
                    // Look for a type
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);

                    SearchType(currentMember.File, currentToken, out foundElement, currentMember.Parent.Namespace);


                    cType = new CompletionType(currentToken, currentMember.File, currentMember.Parent.Namespace);
                    if (!cType.IsEmpty())
                    {
                         foundElement = new CompletionElement(cType.Type);
                        if (findConstructor)
                        {
                            SearchConstructorIn(cType, visibility, out foundElement);
                        }
                        if (foundElement == null)
                        {
                            foundElement = new CompletionElement(cType.XTypeDef);
                        }
                        else if (foundElement.Result == null && cType.XTypeDef != null)
                        {
                            foundElement = new CompletionElement(cType.XTypeDef);
                        }

                        if ((foundElement != null) && (foundElement.IsGeneric))
                        {
                            if (string.IsNullOrEmpty(foundElement.GenericTypeName))
                            {
                                if (currentToken.EndsWith(">"))
                                {
                                    string genName = currentToken;
                                    int index = genName.IndexOf('<');
                                    if (index != -1)
                                    {
                                        // Extract the Generic params
                                        genName = genName.Substring(index + 1);
                                        genName = genName.Substring(0, genName.Length - 1);
                                        foundElement.GenericTypeName = genName;
                                    }
                                }
                            }
                        }
                    }
                }
                else if (currentToken.EndsWith("()"))
                {
                    // this a Method call
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    // Do we already know in which Type we are ?
                    if (currentToken.ToLower() == "self")
                    {
                        SearchConstructorIn(cType, visibility, out foundElement);
                    }
                    else if (currentToken.ToLower() == "super")
                    {
                        SearchConstructorIn(cType.ParentType, visibility, out foundElement);
                    }
                    // The first token in the list can be a Function or a Procedure
                    // Except if we already have a Type
                    if ((currentPos == 0) && (startOfExpression))
                    {
                        var globType = SearchFunctionIn(currentMember.File, currentToken, out foundElement);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            return globType;
                        }
                    }
                    if (!cType.IsEmpty())
                    {
                        // Now, search for a Method
                        cTemp = SearchMethodTypeIn(cType, currentToken, visibility, false, out foundElement, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = cTemp;
                        }
                        else
                        {
                            cType = new CompletionType();
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        // check to see if this is a method from the Object Type, such as ToString().
                        cTemp = SearchMethodTypeIn(new CompletionType("System.Object",file,file.Usings), currentToken, visibility, false, out foundElement, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = cTemp;
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        // Could it be Static Method with "Using Static"
                        // Now, search for a Method
                        cType = SearchMethodStaticIn(currentMember.File, currentToken, out foundElement, dialect);
                    }
                    if (cType.IsEmpty())
                    {
                        cType = null;
                    }
                }
                else
                {
                    bool hasBracket = false;
                    if (currentToken.EndsWith("[]"))
                    {
                        currentToken = currentToken.Substring(0, currentToken.Length - 2);
                        hasBracket = true;
                    }
                    else if (currentToken.EndsWith("."))
                    {
                        currentToken = currentToken.Substring(0, currentToken.Length - 1);
                    }
                    // First token, so it could be a parameter or a local var
                    if (startOfExpression)
                    {
                        // Search in Parameters, Locals, Field and Properties
                        foundElement = FindIdentifier(currentMember, currentToken, ref cType, Modifiers.Private, currentNS, snapshot, currentLine, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = foundElement.ReturnType;
                        }
                    }
                    else
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (!cType.IsEmpty())
                        {
                            cType = SearchPropertyOrFieldIn(cType, currentToken, visibility, out foundElement);
                        }
                    }
                    if (foundElement == null)
                    {
                        cType = SearchType(file, currentToken, out foundElement, currentNS);
                    }
                    // We have it
                    if ((foundElement != null) && (foundElement.IsInitialized))
                    {
                        // and we are in an Array, so we need the "other" type
                        if (hasBracket)
                        {
                            if (foundElement.IsGeneric)
                            {
                                // Retrieve the inner Type
                                if (foundElement.Result != null)
                                {
                                    if (!string.IsNullOrEmpty(foundElement.GenericTypeName))
                                    {
                                        var usings = new List<string>(currentScopes);
                                        usings.AddRange(file.Usings);
                                        if ( !string.IsNullOrEmpty(currentNS) && !usings.Contains(currentNS))
                                        {
                                            usings.Add(currentNS);
                                        }
                                        if (foundElement.GenericTypeName.Contains(','))
                                        {
                                            // Ok, this is might be wrong, but...
                                            string[] items = foundElement.GenericTypeName.Split(',');
                                            if (items.Length > 1)
                                                cType = new CompletionType(items[1], file, usings);
                                        }
                                        else
                                            cType = new CompletionType(foundElement.GenericTypeName, file, usings);
                                    }
                                }
                            }
                            else if (foundElement.IsArray)
                            {
                                // Retrieve the inner Type
                                if (foundElement.Result != null)
                                {
                                    cType = foundElement.ReturnType;
                                }
                            }
                        }
                        else if (foundElement.IsArray)
                        {
                            cType = new CompletionType("System.Array", file, "");
                        }
                    }
                }
                //
                if (cType.IsEmpty())
                {

                    cType = null;
                }
                // Next Token
                currentPos += 1;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                currentToken = tokenList[currentPos];
                if (currentToken == "." || currentToken == ":" || currentToken == "::")
                {
                    currentPos += 1;
                    if (currentPos >= tokenList.Count || currentToken == "::")
                    {
                        break;
                    }
                }
                if (lastToken.EndsWith("(") || lastToken.EndsWith(")") || lastToken.EndsWith("{") || lastToken.EndsWith("}"))
                {
                    startOfExpression = true;
                }
                else
                {
                    startOfExpression = false;
                }
                //
                visibility = Modifiers.Public;
                if ((foundElement != null) && (foundElement.IsInitialized))
                {
                    if (string.Compare(foundElement.Name, "self", true) == 0)
                    {
                        visibility = Modifiers.Private;
                    }
                    else if (string.Compare(foundElement.Name, "super", true) == 0)
                    {
                        visibility = Modifiers.Protected;
                    }
                }
            }
            if (cType != null && foundElement == null && cType.XTypeDef != null)
            {
                foundElement = new CompletionElement(cType.XTypeDef);
            }
#if TRACE
                //
                stopWatch.Stop();
                // Get the elapsed time as a TimeSpan value.
                TimeSpan ts = stopWatch.Elapsed;
                // Format and display the TimeSpan value.
                string elapsedTime = string.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
            //
            WriteOutputMessage("XSharpTokenTools::RetrieveType : Done in " + elapsedTime);
#endif
            return cType;
        }

        static public CompletionType SearchType(XFile xFile, string currentToken, out CompletionElement foundElement, string currentNs)
        {
            foundElement = null;
            CompletionType cType = null;
            WriteOutputMessage($"SearchType in file {xFile.SourcePath} {currentToken}");
            // check for a system type
            var usings = xFile.Usings.ToList();
            if (!string.IsNullOrEmpty(currentNs))
                usings.Add(currentNs);
            // Try to check Check System Types
            cType = new CompletionType(currentToken, xFile, currentNs);
            if (cType.XTypeRef != null)
            {
                foundElement = new CompletionElement(cType.XTypeRef);
                if (foundElement.IsGeneric)
                {
                    // We may need to adapt...
                    string searchTypeName = currentToken;
                    int genMarker = searchTypeName.IndexOf("<");
                    if (genMarker > -1)
                    {
                        searchTypeName = searchTypeName.Substring(genMarker + 1);
                        searchTypeName = searchTypeName.Substring(0, searchTypeName.Length - 1);
                        foundElement.GenericTypeName = searchTypeName;
                    }
                }
            }
            else if (cType.XTypeDef != null)
            {
                foundElement = new CompletionElement(cType.XTypeDef);
            }
            if (cType.IsEmpty())
            {
                usings.Add(currentNs);
                var type = xFile.Project.Lookup(currentToken, usings);
                if (type != null)
                {
                    cType = new CompletionType(type);
                    foundElement = new CompletionElement(type);
                }
            }
            return cType;
        }

        static List<string> nestedSearches = new List<string>();
        static public CompletionElement FindIdentifier(XMemberDefinition member, string name, ref CompletionType cType,
            Modifiers visibility, string currentNS, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect)
        {
            IXElement element;
            CompletionElement foundElement = null;
            if (nestedSearches.Contains(name,StringComparer.OrdinalIgnoreCase))
            {
                return null;
            }
            int nestedLevel = nestedSearches.Count();
            try
            {
                nestedSearches.Add(name);
                if (cType.IsEmpty())
                {
                    cType = new CompletionType(member.ParentType,null);
                }
                WriteOutputMessage($"--> FindIdentifier in {cType.FullName}, {name} ");
                element = member.Parameters.Where(x => StringEquals(x.Name, name)).FirstOrDefault();
                if (element == null)
                {
                    // then Locals
                    element = member.GetLocals(snapshot, currentLine, dialect).Where(x => StringEquals(x.Name, name)).LastOrDefault();
                    if (element == null)
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (!cType.IsEmpty())
                        {
                            cType = SearchPropertyOrFieldIn(cType, name, visibility, out foundElement);
                            if (foundElement != null)
                            {
                                element = foundElement.Result;
                            }

                        }
                        // Find Defines and globals in this file
                        if (element == null && cType.IsEmpty() && member.File.GlobalType != null)
                        {
                            element = member.File.GlobalType.GetMembers(name, true).FirstOrDefault();
                        }
                        if (element == null)
                        {
                            var type = member.File.Project.Lookup(XSharpModel.XLiterals.GlobalName);
                            if (type != null)
                            {
                                element = type.GetMembers(name,true).FirstOrDefault();
                            }
                        }
                        if (element == null)
                        {
                            // Now, search for a Global in external Assemblies
                            //
                            cType = SearchGlobalFieldIn(member.File, name, out foundElement);
                        }
                    }
                }
                if (element != null)
                {
                    if (element is XVariable)
                    {
                        XVariable xVar = element as XVariable;
                        if (xVar.TypeName == XLiterals.VarType)
                        {
                            resolveVarType(xVar, member, ref cType,visibility,currentNS,snapshot, currentLine,dialect);
                        }
                        cType = new CompletionType((XVariable)element, currentNS);
                        foundElement = new CompletionElement(xVar);
                    }
                    else if (element is IXMember)
                    {
                        var xMember = (IXMember)element;
                        cType = new CompletionType(xMember);
                        foundElement = new CompletionElement(xMember);
                    }
                    else if (element is IXType)
                    {
                        var xType = (IXType)element;
                        cType = new CompletionType(xType);
                        foundElement = new CompletionElement(xType);
                    }
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("FindIdentifier failed: ");
                XSharpProjectPackage.Instance.DisplayException(ex);

            }
            finally
            {
                while (nestedSearches.Count > nestedLevel)
                {
                    nestedSearches.Remove(nestedSearches.Last());
                }
            }
            return foundElement;
        }

        private static void resolveVarType(XVariable xVar, XMemberDefinition member, ref CompletionType cType,
            Modifiers visibility, string currentNS, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect)
        {
            var tokens = xVar.Expression;
            List<string> tokenList;
            string expr;
            CompletionElement foundElement = null;
            bool resolved = false;
            if (tokens == null || tokens.Count == 0)
            {
                xVar.TypeName = "OBJECT";
                return;
            }
            if (tokens.Count == 1)
            {
                resolved = true;
                switch (tokens[0].Type)
                {
                    case XSharpLexer.STRING_CONST:
                    case XSharpLexer.NULL_STRING:
                        xVar.TypeName = "STRING";
                        break;
                    case XSharpLexer.INT_CONST:
                        xVar.TypeName = "INT";
                        break;
                    case XSharpLexer.CHAR_CONST:
                        xVar.TypeName = "CHAR";
                        break;
                    case XSharpLexer.REAL_CONST:
                        xVar.TypeName = "REAL8";
                        break;
                    case XSharpLexer.SYMBOL_CONST:
                        xVar.TypeName = "SYMBOL";
                        break;
                    case XSharpLexer.NULL_DATE:
                    case XSharpLexer.DATE_CONST:
                        xVar.TypeName = "DATE";
                        break;
                    case XSharpLexer.TRUE_CONST:
                    case XSharpLexer.FALSE_CONST:
                        xVar.TypeName = "LOGIC";
                        break;
                    case XSharpLexer.NIL:
                        xVar.TypeName = "USUAL";
                        break;
                    case XSharpLexer.NULL:
                        xVar.TypeName = "OBJECT";
                        break;
                    case XSharpLexer.NULL_PTR:
                        xVar.TypeName = "IntPtr";
                        break;
                    default:
                        resolved = false;
                        break;

                }
                if (resolved)
                    return;
            }
            if (tokens[0].Type == XSharpLexer.IN && tokens.Count > 1)
            {
                // foreach loop with IN <somevariable> or IN <list>
                if (tokens.Count == 2)
                {
                    string collection = tokens[1].Text;
                    foundElement = FindIdentifier(member, collection, ref cType,
                                visibility, currentNS, snapshot, currentLine, dialect);
                    if (foundElement != null)
                    {
                        xVar.TypeName = GetTypeFromFoundElement(foundElement);
                        return;
                    }
                }

                expr = BuildTokenString(tokens, 1);
                if (!string.IsNullOrEmpty(expr))
                {
                    tokenList = new List<string> { expr };
                    cType = RetrieveType(xVar.File, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, dialect);
                    if (foundElement != null)
                    {
                        xVar.TypeName = GetTypeFromFoundElement(foundElement);
                    }
                }
                return;
            }
            expr = BuildTokenString(tokens, 0);
            tokenList = new List<string> { expr };
            cType = RetrieveType(xVar.File, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, dialect);
            if (foundElement != null)
            {
                xVar.TypeName = cType.FullName;
            }
            return;
        }

        private static string GetTypeFromFoundElement(CompletionElement foundElement)
        {
            // Let's set the Std Type for this VAR
            if (foundElement.IsGeneric)
            {
                return foundElement.GenericTypeName;
            }
            else if (foundElement.IsArray)
            {
                return foundElement.ReturnType.FullName;
            }
            var type = foundElement.ReturnType;
            if (type.Type != null)
            {
                foreach (var prop in type.Type.GetProperties())
                {
                    var pars = prop.Parameters;
                    if (pars.Count == 1)
                    {
                        switch (pars[0].TypeName.ToLower())
                        {
                            case "long":
                            case "int":
                            case "system.int32":
                                return prop.TypeName;
                        }
                    }

                }
            }

            return "object";
        }

        private static string BuildTokenString(IList<IToken> tokens, int start = 0)
        {
            var res = getDelimitedTokens(tokens, start, XSharpLexer.LPAREN, XSharpLexer.RPAREN);
            if (res.Length > 0)
            {
                return res + "()";
            }
            res = getDelimitedTokens(tokens, start, XSharpLexer.LCURLY, XSharpLexer.RCURLY);
            if (res.Length > 0)
            {
                return res + "{}";
            }
            res = getDelimitedTokens(tokens, start, XSharpLexer.LBRKT, XSharpLexer.RBRKT);
            if (res.Length > 0)
            {
                return res + "[]";
            }
            return "";
        }

        static string getDelimitedTokens(IList<IToken> tokens, int start, int leftToken, int rightToken)
        {
            var sb = new StringBuilder();
            bool left = false, right = false;
            int nested = 0;
            bool done = false;
            for (int i = start; i < tokens.Count && !done; i++)
            {
                var t = tokens[i];
                if (t.Type == leftToken)
                {
                    left = true;
                    nested++;
                }
                else if (t.Type == rightToken)
                {
                    right = true;
                    nested--;
                    if (nested == 0)
                        done = true;
                }
                else
                {
                    if (!left)
                    {
                        sb.Append(t.Text);
                    }
                }
            }
            if (left && right)
            {
                return sb.ToString();
            }
            return "";
        }


        /// <summary>
        /// Search for the Constructor in the corresponding Type,
        /// no return value, the constructor is returned by foundElement
        /// </summary>
        /// <param name="cType"></param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        private static void SearchConstructorIn(CompletionType cType, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($"--> SearchConstructorIn {cType?.FullName}");
            foundElement = null;
            if (cType.Type != null)
            {
                //
                var xMethod = cType.Type.Members.Where(x => x.Kind == Kind.Constructor).FirstOrDefault();
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod != null)
                {
                    foundElement = new CompletionElement(xMethod);
                    return;
                }
            }
            return;
        }


        /// <summary>
        /// Search for a Property or a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XTypeDefinition (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        internal static CompletionType SearchPropertyOrFieldIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            CompletionType result = SearchFieldTypeIn(cType, currentToken, minVisibility, out foundElement);
            if (result.IsEmpty())
            {
                result = SearchPropertyTypeIn(cType, currentToken, minVisibility, out foundElement);
            }
            return result;
        }

        private static CompletionType SearchExternalGlobalFieldIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($"<-- SearchExternalGlobalFieldIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<string> emptyUsings = new List<string>();
            foreach (string staticUsing in xFile.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                CompletionType tempType = new CompletionType(staticUsing, xFile, emptyUsings);
                //
                cType = SearchPropertyOrFieldIn(tempType, currentToken, Modifiers.Public, out foundElement);
                if (!cType.IsEmpty())
                    break;
            }
            //
            return cType;
        }


        /// <summary>
        /// Search for a Property, in a CompletionType, based on the Visibility.
        /// A Completion can have a XTypeDefinition (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchPropertyTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchPropertyTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {
                
                IXMember property = cType.Type.GetProperty(currentToken).FirstOrDefault();
                //
                if ((property != null) && (property.Visibility < minVisibility))
                {
                    property = null;
                }
                //
                if (property == null )
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchPropertyTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(property);
                    return foundElement.ReturnType;
                }
            }
            // Sorry, not found
            return new CompletionType();

        }

        /// <summary>
        /// Search for a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XTypeDefinition (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Field we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Field (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchFieldTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFieldTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {
                IXMember field = cType.Type.GetField(currentToken).FirstOrDefault();
                if ((field != null) && (field.Visibility < minVisibility))
                {
                    field = null;
                }
                if (field == null )
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        // Search in base type
                        return SearchFieldTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(field);
                    return foundElement.ReturnType;
                }
            }
            
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Method, in a CompletionType, based on the Visibility.
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Method we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType that the Method will return (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// and FoundElement is null
        /// </returns>
        internal static CompletionType SearchMethodTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, bool staticOnly, out CompletionElement foundElement, XSharpDialect dialect)
        {
            WriteOutputMessage($" SearchMethodTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {
                IXMember xMethod = cType.Type.GetMembers(currentToken,true).Where(x => x.Kind.IsClassMethod(dialect) ).FirstOrDefault();
                if ((xMethod != null) && staticOnly && !xMethod.IsStatic)
                {
                    xMethod = null;
                }
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod == null )
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        cType.Type.ForceComplete();
                        //
                        if (minVisibility == Modifiers.Private)
                            minVisibility = Modifiers.Protected;
                        return SearchMethodTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, minVisibility, staticOnly, out foundElement, dialect);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(xMethod);
                    return foundElement.ReturnType;
                }
            }
           
            // Sorry, not found
            return new CompletionType();
        }


        /// <summary>
        /// Search for a static Method in a File
        /// </summary>
        /// <param name="xFile">The File to search in</param>
        /// <param name="currentToken">The Toekn to look after</param>
        /// <param name="foundElement">The Found Element</param>
        /// <returns>The CompletionType that contains the Element</returns>
        private static CompletionType SearchMethodStaticIn(XFile xFile, string currentToken, out CompletionElement foundElement, XSharpDialect dialect)
        {
            WriteOutputMessage($" SearchMethodStaticIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<string> emptyUsings = new List<string>();
            foreach (string staticUsing in xFile.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                CompletionType tempType = new CompletionType(staticUsing, xFile, emptyUsings);
                //
                cType = SearchMethodTypeIn(tempType, currentToken, Modifiers.Public, true, out foundElement, dialect);
                if (foundElement != null)
                {
                }
                if (!cType.IsEmpty())
                    break;
            }
            //
            return cType;
        }

        private static CompletionType SearchGlobalFieldIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchGlobalFieldIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            if (xFile.Project.AssemblyReferences == null)
            {
                return null;
            }
            //
            var global = xFile.Project.FindGlobalOrDefine(currentToken);
            if (global != null)
            {
                foundElement = new CompletionElement(global);
                return new CompletionType(global.ParentType);
            }
            CompletionType cType = null;
            List<string> emptyUsings = new List<string>();
            var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where(m => m.Kind.IsField()).ToArray();
            string declType = null;
            IXMember field = null;
            foreach (var member in found)
            {
                if (StringEquals(member.Name, currentToken))
                {
                    field = member;
                    declType = field.TypeName;
                    break;
                }
            }
            if (declType != null)
            {
                foundElement = new CompletionElement(field);
                return new CompletionType(declType, xFile, xFile.Usings);
            }
            return cType;
        }

        private static CompletionType SearchFunctionIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFunctionIn {xFile.SourcePath}, {currentToken} ");

            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            //
            IXMember xMethod = xFile.Project.FindFunction(currentToken);
            //
            if (xMethod == null)
            {
                var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where ( m=> m.Kind.IsMethod() ).ToArray();
                if (found.Length > 1)
                {
                    foreach (var m in found)
                    {
                        // Find an exact match first
                        if (string.Compare(m.Name, currentToken, true) == 0)
                        {
                            foundElement = new CompletionElement(m);
                            return new CompletionType(m.ParentType);

                        }
                    }
                }
                if (found.Length > 0)
                {
                    // no exact match, return the first one
                    foundElement = new CompletionElement(found[0]);
                    return new CompletionType(found[0].ParentType);
                }
            }

            foundElement = new CompletionElement(xMethod);
            if (xMethod?.Parent != null)
            {
                // Parent is a XElement, so one of our Types
                return new CompletionType((xMethod.Parent as XTypeDefinition).Clone);
            }
            //
            return cType;
        }

        private static CompletionType SearchGlobalOrDefineIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchGlobalOrDefineIn {xFile.SourcePath}, {currentToken} ");

            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            //
            IXMember xMethod = xFile.Project.FindGlobalOrDefine(currentToken);
            //
            if (xMethod == null)
            {
                if (xMethod == null)
                {
                    var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where(m => m.Kind.IsField()).ToArray();
                    if (found.Length > 1)
                    {
                        foreach (var m in found)
                        {
                            // Find an exact match first
                            if (string.Compare(m.Name, currentToken, true) == 0)
                            {
                                foundElement = new CompletionElement(m);
                                return new CompletionType(m.ParentType);

                            }
                        }
                    }
                    if (found.Length > 0)
                    {
                        // no exact match, return the first one
                        foundElement = new CompletionElement(found[0]);
                        return new CompletionType(found[0].ParentType);
                    }
                }
            }
            foundElement = new CompletionElement(xMethod);
            if (xMethod?.Parent != null)
            {
                // Parent is a XElement, so one of our Types
                return new CompletionType((xMethod.Parent as XTypeDefinition).Clone);
            }
            //
            return cType;
        }
        internal static IToken ProcessBounds(ITokenStream tokens, IToken currentToken, int LeftElement, int RightElement)
        {
            // Count the elements
            int rightElt = 1;
            while ((currentToken != null) && (rightElt != 0))
            {
                //currentToken = tokens.Lt(-1);
                currentToken = GetPreviousToken(tokens, currentToken);
                if (currentToken != null)
                {
                    // Bump the Left ?
                    if (currentToken.Column == 0)
                    {
                        if (rightElt != 0)
                        {
                            // If the counter is not null, we have a open element
                            currentToken = null;
                        }
                    }
                    // A Left Element ?
                    else if (currentToken.Type == LeftElement)
                    {
                        rightElt--;
                    }
                    // Another Right Parenthesis
                    else if (currentToken.Type == RightElement)
                    {
                        rightElt++;
                    }
                }
            }
            //
            return currentToken;
        }

        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken, bool checkLine)
        {
            XSharpToken prev = null;
            if (currentToken != null)
            {
                prev = (XSharpToken)currentToken;
                int Line = prev.Line;
                do
                {
                    if (prev.OriginalTokenIndex == 0)
                        return null;
                    prev = (XSharpToken)tokens.Get(prev.OriginalTokenIndex - 1);
                    if (checkLine && (prev.Line != Line))
                    {
                        prev = null;
                    }
                } while ((prev != null) && (string.IsNullOrWhiteSpace(prev.Text)));
            }
            return prev;
        }

        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken)
        {
            return GetPreviousToken(tokens, currentToken, true);
        }


        static void WriteOutputMessage(string message)
        {
            if (XSharpCompletionSource._optionsPage == null)
            {
                var package = XSharpProjectPackage.Instance;
                XSharpCompletionSource._optionsPage = package.GetIntellisenseOptionsPage();
            }
            if (XSharpCompletionSource._optionsPage.EnableCodeCompletionLog && XSharpCompletionSource._optionsPage.EnableOutputPane)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharp.Codecompletion :" + message);
            }
        }



    }


    /// <summary>
    /// Static class Tools. Offer services to get Glyphs (Icons) to CompletionSource (at least) ...
    /// </summary>
    public static class XSharpGlyphTools
    {
        public static StandardGlyphGroup getGlyphGroup(this IXElement elt)
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

        public static StandardGlyphItem getGlyphItem(this IXEntity elt)
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
        IXEntity foundElement = null;
        public bool IsSourceElement => SearchLocation != null;
        string genTypeName;
        public XSourceElement SourceElement => foundElement as XSourceElement;
        public XSourceElement SearchLocation;
        public CompletionElement(IXEntity element)
        {
            this.foundElement = element;
            if (element is XSourceElement)
            {
                this.SearchLocation = (XSourceElement)element;
            }
        }
        public CompletionElement(IXEntity element, XFile file)
        {
            this.foundElement = element;
            if (element is XSourceElement)
            {
                this.SearchLocation = (XSourceElement)element;
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

        public IXEntity Result => foundElement ;

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
                    cType = new CompletionType(searchTypeName, SearchLocation.File, SearchLocation.FileUsings);
                }
                else
                {
                    if (foundElement is IXMember)
                    {
                        cType = new CompletionType(foundElement as IXMember);
                    }
                    else // if (foundElement is IXType)
                    {
                        cType = new CompletionType(foundElement as IXType);
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
                        if ((this.foundElement is XMemberDefinition) && (this.Result.Kind.HasReturnType()))
                        {
                            XMemberDefinition xt = (XMemberDefinition)this.foundElement;
                            searchTypeName = xt.TypeName;
                        }
                        else if (this.foundElement is XVariable)
                        {
                            XVariable xv = (XVariable)this.foundElement;
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
                return foundElement.IsArray;
            }
        }

        public bool IsGeneric
        {
            get
            {
                var type = foundElement.TypeName;
                if ( type!= null)
                    return type.EndsWith(">");
                return false;
            }
        }

    }

    // Build a list of all Keywords
    internal static class XSharpTypes
    {
        static IList<IXEntity> _keywords;
        static IList<IXEntity> _types;

        static XSharpTypes()
        {
            // Dummy call to a Lexer; just to copy the Keywords, Types, ...
            // Pass default options so this will be the core dialect and no
            // 4 letter abbreviations will be in the list
            var lexer = XSharpLexer.Create("", "", XSharpParseOptions.Default);
            //

            var keywords = new List<IXEntity>();
            var types = new List<IXEntity>();
            //
            foreach (var keyword in lexer.KwIds)
            {
                keywords.Add(new XEntityDefinition(keyword.Key, Kind.Keyword));
                if (XSharpLexer.IsType(keyword.Value))
                {
                    types.Add(new XEntityDefinition(keyword.Key, Kind.Keyword));
                }
            }
            //
            _keywords = keywords.ToArray();
            _types = types.ToArray();
        }

        internal static IList<IXEntity> Get()
        {
            return _keywords;
        }
        internal static IList<IXEntity> GetTypes()
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
        internal static IList<IXVariable> GetLocals(this XMemberDefinition member, ITextSnapshot snapshot, int iCurrentLine, XSharpDialect dialect)
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
                if (local is XSourceElement )
                {
                    ((XSourceElement)local).File = member.File;
                }

               
            }
            if (member.Kind.IsClassMember(dialect) && !member.Modifiers.HasFlag(Modifiers.Static))
            {
                var XVar = new XVariable(member, "SELF", member.Range, member.Interval, member.ParentName);
                XVar.File = walker.File;
                locals.Add(XVar);
                if (member.ParentType.BaseType != null)
                {
                    XVar = new XVariable(member, "SUPER", member.Range, member.Interval, member.ParentType.BaseType);
                    XVar.File = walker.File;
                    locals.Add(XVar);
                }
            }
            return locals;
        }
    }
}




