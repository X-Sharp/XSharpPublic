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
            var file = textBuffer.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return null;
            return new XSharpCompletionSource(this, textBuffer, aggregator, file);
        }
    }

    partial class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;
        private bool _settingIgnoreCase;
        // Keep a trace of the Context of the TokenList build
        // This will be AS, IS, REF, USING, STATIC (USING STATIC), INHERIT, IMPLEMENTS etc.
        private IToken _stopToken;

        private XFile _file;
        private bool _showTabs;
        private bool _keywordsInAll;
        private IBufferTagAggregatorFactoryService aggregator;
        private XSharpDialect _dialect;

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
                bool showInstanceMembers = (typedChar == ':') || _file.Project.ParseOptions.AllowDotForInstanceMembers;
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
                int currentLine = triggerPoint.GetContainingLine().LineNumber;
                var member = XSharpLookup.FindMember(currentLine, this._file);
                if (member == null)
                    return;
                // Standard TokenList Creation (based on colon Selector )
                var caretPos = triggerPoint.Position;
                bool includeKeywords;
                session.Properties.TryGetProperty(XsCompletionProperties.IncludeKeywords, out includeKeywords);
                CompletionState state;

                var currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, this._file);
                string currentNs = "";
                if (currentNamespace != null)
                {
                    currentNs =currentNamespace.Name;
                }
                var location = new XSharpSearchLocation(member, snapshot, currentLine, caretPos, currentNs);
                var tokenList = XSharpTokenTools.GetTokenList(location, out state, includeKeywords);


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
                bool dotSelector = (typedChar == '.');

                // TODO: Based on the Project.Settings, we should add the Vulcan.VO namespace
                int tokenType = XSharpLexer.UNRECOGNIZED;
                if (this._stopToken != null)
                {
                    tokenType = this._stopToken.Type;
                }
             
                var symbol  = XSharpLookup.RetrieveElement(location, tokenList, CompletionState.General).FirstOrDefault();
                var memberName = "";
                // Check for members, locals etc and convert the type of these to IXTypeSymbol
                if (symbol != null )
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
                        if (xvar is  XSourceVariableSymbol sourcevar)
                        {
                            typeName = sourcevar.TypeName;
                            type = sourcevar.File.FindType(typeName);

                        }
                        else if (xvar is XPEParameterSymbol par)
                        {
                            typeName = par.TypeName;
                            type = location.FindType(typeName);
                        }
                        memberName = xvar.Name;
                    }
                    session.Properties[XsCompletionProperties.Type] = type;
                }
                if (dotSelector || state != CompletionState.None)
                {
                    if (string.IsNullOrEmpty(filterText))
                    {
                        filterText = TokenListAsString(tokenList, _stopToken);
                        if (filterText.Length > 0 && !filterText.EndsWith("."))
                            filterText += ".";
                    }
                    if (state.HasFlag(CompletionState.Namespaces))
                    {
                        AddNamespaces(compList, _file.Project, filterText);
                    }
                    if (state.HasFlag(CompletionState.Types) || state.HasFlag(CompletionState.Interfaces))
                    {
                        AddTypeNames(compList, _file.Project, filterText, location.Usings);
                        AddXSharpKeywordTypeNames(kwdList, filterText);
                    }
                    if (state.HasFlag(CompletionState.StaticMembers))
                    {
                        if (type != null && symbol is IXTypeSymbol)
                        {
                            // First we need to keep only the text AFTER the last dot
                            int dotPos = filterText.LastIndexOf('.');
                            filterText = filterText.Substring(dotPos + 1, filterText.Length - dotPos - 1);
                            BuildCompletionList(compList, type, Modifiers.Public, true, filterText);
                        }
                    }
                    if (state.HasFlag(CompletionState.InstanceMembers) &&  ! (symbol is IXTypeSymbol))
                    {
                        showInstanceMembers = true;
                        filterText = "";

                    }
                    if (state.HasFlag(CompletionState.General))
                    {
                        AddNamespaces(compList, _file.Project, filterText);
                        AddTypeNames(compList, _file.Project, filterText, location.Usings);
                        AddXSharpKeywordTypeNames(kwdList, filterText);
                        // Todo Add Global members from this project and from all global namespaces ?
                    }
                }
                if (showInstanceMembers && ! (symbol is IXTypeSymbol))
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
                        BuildCompletionList(compList, type, visibleAs, false, filterText);
                    }
                }
                //
                if (!dotSelector && !showInstanceMembers)
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
                            AddTypeNames(compList, _file.Project, filterText, location.Usings);
                            //
                            AddXSharpKeywordTypeNames(kwdList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            AddTypeNames(compList, _file.Project, filterText, location.Usings,true);
                            break;
                        default:

                            if (member != null)
                            {
                                // Fill with the context ( Parameters and Locals )
                                BuildCompletionList(compList, member, filterText, location);
                                AddXSharpKeywords(compList, filterText);
                                // Context Type....
                                //cType = new CompletionType(((XSourceTypeSymbol) (member.Parent)).Clone);
                                //if (!cType.IsEmpty())
                                //{
                                //    // Get the members also
                                //    BuildCompletionList(compList, type, Modifiers.Private, false, filterText);
                                //}
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
                            AddTypeNames(compList, _file.Project, filterText, location.Usings);
                            //
                            AddXSharpKeywordTypeNames(kwdList, filterText);
                            //
                            AddUsingStaticMembers(compList, _file, filterText);
                            break;
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

        private void AddUsingStaticMembers(XCompletionList compList, XFile file, string filterText)
        {
            //
            //foreach (string staticType in file.AllUsingStatics)
            //{
            //    BuildCompletionList(compList, new CompletionType(staticType, file, ""), Modifiers.Public, true, filterText);
            //}
            // And what about Global Types in referenced Assemblies ?
            var found = file.Project.FindGlobalMembersInAssemblyReferences(filterText);
            FillMembers(compList, null, found, Modifiers.Public, true);
        }
        public static bool isGenerated(IXTypeSymbol type)
        {
            return type.Name.IndexOf("$", StringComparison.Ordinal) > -1;
        }

        private void AddTypeNames(XCompletionList compList, XProject project, string startWith, List<string> usings, bool onlyInterfaces = false)
        {
            if (startWith == null)
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


            foreach (var type in project.FindSystemTypesByName(startWith, usings.ToArray()))
            {
                if (isGenerated(type))
                   continue;
                if (onlyInterfaces && type.Kind != Kind.Interface)
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
            if (realMemberName.Length > 2 && XSettings.EditorHideAdvancedMembers
                && (realMemberName.StartsWith("__", StringComparison.Ordinal) || realMemberName.EndsWith("__", StringComparison.Ordinal)))
                return true;
            // suppress SELF properties
            if (string.Compare(realMemberName, "self", StringComparison.Ordinal) == 0)
                return true;
            if (realMemberName.IndexOf('$') >= 0)
                return true;

            if (realMemberName.Length > 4)
            {
                if (realMemberName == ".dtor")
                    return true;
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

        private void AddXSharpKeywords(XCompletionList compList, string startWith)
        {
            foreach (var kw in XSharpTypes.Get().Where(ti => nameStartsWith(ti.Name, startWith)))
            {
                ImageSource icon = _provider.GlyphService.GetGlyph(kw.getGlyphGroup(), kw.getGlyphItem());
                compList.Add(new XSCompletion(kw.Name, kw.Name, kw.Prototype, icon, null, Kind.Keyword,""));
            }
        }

        private void AddXSharpTypeNames(XCompletionList compList, XProject project, string startWith, IList<string> usings)
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

        private void AddXSharpKeywordTypeNames(XCompletionList compList, string startWith)
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


        private void AddNamespaces(XCompletionList compList, XProject project, string startWith)
        {
            // We are looking for NameSpaces, in References
            if (startWith == null)
                return;
            var namespaces = project.GetAssemblyNamespaces();
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            ImageSource icon = _provider.GlyphService.GetGlyph(StandardGlyphGroup.GlyphGroupNamespace, StandardGlyphItem.GlyphItemPublic);
            ImageSource iconClass = _provider.GlyphService.GetGlyph(StandardGlyphGroup.GlyphGroupClass, StandardGlyphItem.GlyphItemPublic);
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
                XSCompletion item;
                if (realNamespace.IndexOf("<") > 0)
                {
                    item = new XSCompletion(realNamespace, realNamespace, nameSpace, iconClass, null, Kind.Class, "");
                }
                else
                {
                    item = new XSCompletion(realNamespace, realNamespace, "Namespace " + nameSpace, icon, null, Kind.Namespace, "");
                }
                if (!compList.Add(item))
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

        private void AddXSharpNamespaces(XCompletionList compList, XProject project, string startWith, ImageSource icon)
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

        private void BuildCompletionList(XCompletionList compList, XSourceMemberSymbol currentMember, string startWith, XSharpSearchLocation location)
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
            foreach (var localVar in currentMember.GetLocals(location).Where(l => nameStartsWith(l.Name, startWith) && l.Range.StartLine-1 <= location.LineNumber))
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


        private void BuildCompletionList(XCompletionList compList, IXTypeSymbol type, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (type == null)
            {
                return;
            }
            //
            FillMembers(compList, type, minVisibility, staticOnly, startWith);
            if (type is XSourceTypeSymbol sourceType)
            {
                sourceType.ForceComplete();
                var baseType = sourceType.BaseType;
                if (string.IsNullOrWhiteSpace(baseType))
                {
                    if (type.Kind == Kind.Enum)
                        baseType = "System.Enum";
                    else if (type.Kind == Kind.Delegate)
                        baseType = "System.Delegate";
                    else
                        baseType = "System.Object";
                }
                var parentType = sourceType.File.FindType(baseType, sourceType.Namespace);
                if (baseType == "System.Enum" && staticOnly)
                {
                    ; // do nothing
                }
                else
                    BuildCompletionList(compList, parentType, Modifiers.Protected, staticOnly, startWith);
                foreach (var ifname in sourceType.Interfaces)
                {
                    var iftype = sourceType.File.FindType(ifname, sourceType.Namespace);
                    if (iftype != null)
                    {
                        BuildCompletionList(compList, iftype, Modifiers.Public, staticOnly, startWith);
                    }
                }
            }
            FillExtensions(compList, type, startWith);
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
        private void FillMembers(XCompletionList compList, IXTypeSymbol type, IEnumerable<IXMemberSymbol> members, Modifiers minVisibility, bool staticOnly)
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
                        add = staticOnly;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Operator:
                        add = false;
                        break;
                    default:
                        if (elt.IsStatic != staticOnly)
                            add = false;
                        if (add)
                        { 
                            add = elt.IsVisible(minVisibility);
                        }
                        break;
                }
                if (type != null && elt.IsStatic && type.Kind == Kind.Enum && elt.DeclaringType != type.FullName && elt.Name != "HasFlag" )
                    add = false;
                if (!add)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                string toAdd = "";
                if (elt.Kind.HasParameters() && elt.Kind != Kind.Constructor && !elt.Kind.IsProperty())
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
        private void FillMembers(XCompletionList compList, IXTypeSymbol xType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            FillMembers(compList, xType, xType.GetMembers(startWith), minVisibility, staticOnly);
        }


        private void FillExtensions(XCompletionList compList, IXTypeSymbol type, string startWith)
        {
            //WriteOutputMessage($"FillExtensions for type {type?.FullName}");
            if (type != null)
            {
                var extensions = _file.Project.GetExtensions(type.FullName);
                IEnumerable<IXMemberSymbol> selection = extensions;
                if (! string.IsNullOrEmpty(startWith))
                {
                    selection = extensions.Where(x => nameStartsWith(x.Name, startWith));
                }
                FillMembers(compList, null, selection, Modifiers.Public, true);
                foreach (var ifname in type.Interfaces)
                {
                    var lifname = ifname;
                    var lookupproject = _file.Project;
                    if (type is XSourceTypeSymbol sourceType)
                    {
                        var typedef = sourceType;
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
                    FillMembers(compList, null, selection, Modifiers.Public, true);
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
        private string TokenListAsString(List<XSharpToken> tokenList, IToken fromToken)
        {
            string retValue = "";
            bool include = false;
            if (fromToken == null)
                include = true;
            for (int pos = 0; pos < tokenList.Count; pos++)
            {
                var t = tokenList[pos];
                if (include)
                {
                    switch (t.Type)
                    {
                        case XSharpLexer.ID:
                        case XSharpLexer.DOT:
                        case XSharpLexer.COLON:
                        case XSharpLexer.COLONCOLON:
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.RPAREN:
                            retValue += t.Text; 
                            break;
                    }
                    
                }
                else
                {
                    include = (t == fromToken);
                }
            }
            return retValue;
        }
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
        internal static IList<XSourceVariableSymbol> GetLocals(this XSourceMemberSymbol member, XSharpSearchLocation location)
        {
            var iCurrentLine = Math.Min(location.Snapshot.LineCount - 1, location.LineNumber);
            // create a walker with just the contents of the current member
            // use a new file object so we will not destroy the types in the existing object
            var walker = new SourceWalker( new XFile ( member.File.FullPath, member.File.Project));
            var start = member.Interval.Start;
            var end = member.Interval.Width;
            if (start + end > location.Snapshot.Length)
                end = location.Snapshot.Length - start;
            var memberSource = location.Snapshot.GetText(start, end);
            
            var locals = walker.ParseLocals(memberSource, member);
            // Add the normal locals for class members
            foreach(var local in locals)
            {
                // assign the current member so we will have the proper Parent as well
                local.Parent = member;
                local.File = member.File;
            }
            if (member.Kind.IsClassMember(location.Dialect) && !member.Modifiers.HasFlag(Modifiers.Static))
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




