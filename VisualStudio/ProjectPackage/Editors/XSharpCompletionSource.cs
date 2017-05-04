//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;

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

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
#if CODEMODEL
            return new XSharpCompletionSource(this, textBuffer, GetFileName(textBuffer));
#else
            return null;
#endif
        }

        private string GetFileName(ITextBuffer buffer)
        {
            IVsTextBuffer bufferAdapter;
            buffer.Properties.TryGetProperty(typeof(IVsTextBuffer), out bufferAdapter);
            if (bufferAdapter != null)
            {
                var persistFileFormat = bufferAdapter as IPersistFileFormat;
                string ppzsFilename = null;
                uint iii;
                if (persistFileFormat != null)
                    persistFileFormat.GetCurFile(out ppzsFilename, out iii);
                return ppzsFilename;
            }
            return null;
        }
    }

    partial class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private String _fileName;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;
        private bool _settingIgnoreCase;
        // Keep a trace of the Context of the TokenList build
        private IToken _stopToken;
        internal static bool StringEquals(string lhs, string rhs)
        {
            if (String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XSharpCompletionSource(XSharpCompletionSourceProvider provider, ITextBuffer buffer, String fileName)
        {
            _provider = provider;
            _buffer = buffer;
            _fileName = fileName;
            // Currently, set as default, but should be VS Settings Based
            _settingIgnoreCase = true;
            _stopToken = null;
        }

        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            if (_disposed)
                throw new ObjectDisposedException("XSharpCompletionSource");
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
            SnapshotPoint start = triggerPoint;
            //while (start > line.Start && !char.IsWhiteSpace((start - 1).GetChar()))
            //{
            //    start -= 1;
            //}
            //
            var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
            //
            XFile file = XSharpModel.XSolution.FindFullPath(this._fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
                return;
            }
            // The Completion list we are building
            CompletionList compList = new CompletionList();
            // The CompletionType we will use to fill the CompletionList
            CompletionType cType = null;
            // Start of Process
            // Build a list with the tokens we have from the TriggerPoint to the start of the line
            //List<String> tokenList = this.GetTokenList(triggerPoint);
            //LanguageService.SyntaxTree.ITokenStream tokenStream;
            List<String> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, triggerPoint.GetContainingLine().LineNumber, _buffer.CurrentSnapshot.GetText(), out _stopToken, false, _fileName);
            // and make it a string
            //String tokenLine = TokenListAsString(tokenList, 0);
            String filterText = "";
            // We might be here due to a COMPLETEWORD command, so we have no typedChar
            // but we "may" have a incomplete word like System.String.To
            if (typedChar == '\0')
            {
                if (tokenList.Count > 0)
                {
                    String extract = tokenList[tokenList.Count - 1];
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
                                String startToken = extract.Substring(0, dotPos);
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
            // Check if we can get the member where we are
            XTypeMember member = XSharpTokenTools.FindMember(triggerPoint.Position, this._fileName);
            XType currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, this._fileName);
            HashSet<String> Usings = new HashSet<String>(file.Usings, StringComparer.OrdinalIgnoreCase);
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
            XElement foundElement;
            MemberInfo dummyElement;
            String currentNS = "";
            if (currentNamespace != null)
            {
                currentNS = currentNamespace.Name;
            }
            cType = XSharpTokenTools.RetrieveType(_fileName, tokenList, member, currentNS, null, out foundElement, out dummyElement);
            switch (typedChar)
            {
                case '.':
                    if (String.IsNullOrEmpty(filterText))
                    {
                        filterText = TokenListAsString(tokenList, 0);
                        if (!filterText.EndsWith("."))
                            filterText += ".";
                    }
                    switch (tokenType)
                    {
                        case XSharpLexer.USING:
                            // It can be a namespace 
                            AddNamespaces(compList, file.Project, filterText);
                            break;
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.INHERIT:
                            // It can be a namespace 
                            AddNamespaces(compList, file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, file.Project, filterText, Usings);
                            //
                            AddXSharpTypesTypeNames(compList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            // It can be a namespace 
                            AddNamespaces(compList, file.Project, filterText);
                            // TODO: add Interfaces only
                            break;
                        default:
                            // It can be a namespace 
                            AddNamespaces(compList, file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, file.Project, filterText, Usings);
                            //
                            AddXSharpTypesTypeNames(compList, filterText);
                            // it can be a static Method/Property/Enum
                            if (cType != null)
                            {
                                // First we need to keep only the text AFTER the last dot
                                int dotPos = filterText.LastIndexOf('.');
                                filterText = filterText.Substring(dotPos + 1, filterText.Length - dotPos - 1);
                                BuildCompletionList(compList, cType, Modifiers.Public, true, filterText);
                            }
                            break;
                    }
                    break;
                case ':':
                    // Member call
                    if (cType != null)
                    {
                        Modifiers visibleAs = Modifiers.Public;
                        if (foundElement != null)
                        {
                            if (String.Compare(foundElement.Name, "self", true) == 0)
                            {
                                visibleAs = Modifiers.Private;
                            }
                            else if (String.Compare(foundElement.Name, "super", true) == 0)
                            {
                                visibleAs = Modifiers.Protected;
                            }

                        }
                        else if (member.ParentName == cType.FullName)
                        {
                            visibleAs = Modifiers.Private;
                        }
                        // Now, Fill the CompletionList with the available members, from there
                        BuildCompletionList(compList, cType, visibleAs, false, filterText);
                    }
                    break;

                default:
                    // Empty line ?
                    // If we have only one Token, it can be the start of a Parameter/Local/Property/Method/Type/...
                    // .........
                    // .........
                    // We were able to determine the Type, so Get the Members
                    if (cType != null)
                    {
                        Modifiers visibleAs = Modifiers.Public;
                        if (member.ParentName == cType.FullName)
                        {
                            visibleAs = Modifiers.Private;
                        }
                        // Now, Fill the CompletionList with the available members, from there
                        BuildCompletionList(compList, cType, visibleAs, false, filterText);
                    }
                    else
                    {
                        switch (tokenType)
                        {
                            case XSharpLexer.USING:
                                // It can be a namespace 
                                AddNamespaces(compList, file.Project, filterText);
                                break;
                            case XSharpLexer.AS:
                            case XSharpLexer.IS:
                            case XSharpLexer.REF:
                            case XSharpLexer.INHERIT:
                                // It can be a namespace 
                                AddNamespaces(compList, file.Project, filterText);
                                // It can be Type, FullyQualified
                                // we should also walk all the USINGs, and the current Namespace if any, to search Types
                                AddTypeNames(compList, file.Project, filterText, Usings);
                                //
                                AddXSharpTypesTypeNames(compList, filterText);
                                break;
                            case XSharpLexer.IMPLEMENTS:
                                // It can be a namespace 
                                AddNamespaces(compList, file.Project, filterText);
                                // TODO: add Interfaces only
                                break;
                            default:
                                if (member != null) // Fill with the context ( Parameters and Locals )
                                {
                                    BuildCompletionList(compList, member, filterText);
                                }
                                // Now Add Functions and Procedures
                                BuildCompletionList(compList, file.Project.LookupFullName(XType.GlobalName, true), Modifiers.Public, false, filterText);
                                // and Add NameSpaces
                                AddNamespaces(compList, file.Project, filterText);
                                // and Types
                                AddTypeNames(compList, file.Project, filterText, Usings);
                                //
                                AddXSharpTypesTypeNames(compList, filterText);
                                break;
                        }
                    }
                    break;
            }

            // Sort in alphabetical order
            compList.Sort((comp1, comp2) => comp1.DisplayText.CompareTo(comp2.DisplayText));
            // and put in the SelectionList
            completionSets.Add(new CompletionSet("All", "All", applicableTo, compList, Enumerable.Empty<Completion>()));
        }

        private void AddTypeNames(CompletionList compList, XProject project, string startWith, HashSet<String> usings)
        {
            AddTypeNames(compList, project, startWith);
            foreach (String nspace in usings)
            {
                AddTypeNames(compList, project, nspace + "." + startWith);
            }
        }

        private void AddTypeNames(CompletionList compList, XProject project, string startWith)
        {
            // We are looking for NameSpaces, in References
            SystemTypeController stc = project.TypeController;
            //
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            foreach (AssemblyInfo assemblyInfo in stc.Assemblies)
            {
                foreach (KeyValuePair<string, System.Type> typeInfo in assemblyInfo.Types.Where(ti => nameStartsWith(ti.Key, startWith)))
                {
                    String realTypeName = typeInfo.Value.FullName;
                    TypeAnalysis typeAnalysis = new TypeAnalysis(typeInfo.Value.GetTypeInfo());
                    // remove the start
                    if (startLen > 0)
                        realTypeName = realTypeName.Substring(startLen);
                    // Do we have another part file
                    dotPos = realTypeName.IndexOf('.');
                    // Then remove it
                    if (dotPos > 0)
                        realTypeName = realTypeName.Substring(0, dotPos);
                    if ((realTypeName.Length > 2) && (realTypeName.StartsWith("__")))
                        continue;
                    //
                    ImageSource icon = _provider.GlyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                    compList.Add(new XSCompletion(realTypeName, realTypeName, typeAnalysis.Description, icon, null));
                }
            }
            //
            // And our own Types
            AddXSharpTypeNames(compList, project, startWith, startLen, dotPos);
            // We should also add the external TypeNames
            IList<XProject> prjs = project.ReferencedProjects;
            foreach (var prj in prjs)
            {
                AddXSharpTypeNames(compList, prj, startWith, startLen, dotPos);
            }
            // And Stranger Projects
            IList<EnvDTE.Project> sprjs = project.StrangerProjects;
            foreach (var prj in sprjs)
            {
                AddStrangerTypeNames(compList, prj, startWith, startLen, dotPos);
            }
        }

        private void AddXSharpTypeNames(CompletionList compList, XProject project, string startWith, int startLen, int dotPos )
        {
            foreach (XFile file in project.Files)
            {
                foreach (XType typeInfo in file.TypeList.Values.Where(ti => nameStartsWith(ti.FullName, startWith)))
                {
                    String realTypeName = typeInfo.FullName;
                    // remove the start
                    if (startLen > 0)
                        realTypeName = realTypeName.Substring(startLen);
                    // Do we have another part 
                    dotPos = realTypeName.IndexOf('.');
                    // Then remove it
                    if (dotPos > 0)
                        realTypeName = realTypeName.Substring(0, dotPos);
                    ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.GlyphGroup, typeInfo.GlyphItem);
                    compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Description, icon, null));
                }
            }
        }

        private void AddStrangerTypeNames(CompletionList compList, EnvDTE.Project project, string startWith, int startLen, int dotPos)
        {

        }

        private void AddXSharpTypesTypeNames(CompletionList compList, string startWith)
        {
            //
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            // And our own Types
            List<XType> xsharpTypes = XSharpTypes.Get();
            foreach (XType typeInfo in xsharpTypes.Where(ti => nameStartsWith(ti.FullName, startWith)))
            {
                String realTypeName = typeInfo.FullName;
                // remove the start
                if (startLen > 0)
                    realTypeName = realTypeName.Substring(startLen);
                // Do we have another part 
                dotPos = realTypeName.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realTypeName = realTypeName.Substring(0, dotPos);
                ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.GlyphGroup, typeInfo.GlyphItem);
                compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Description, icon, null));
            }
        }

        private void AddNamespaces(CompletionList compList, XProject project, String startWith)
        {
            // We are looking for NameSpaces, in References
            SystemTypeController stc = project.TypeController;
            List<String> namespaces = stc.Namespaces;
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            XType fakeNS = new XType("fake", Kind.Namespace, Modifiers.None, Modifiers.Public, TextRange.Empty, TextInterval.Empty);
            ImageSource icon = _provider.GlyphService.GetGlyph(fakeNS.GlyphGroup, fakeNS.GlyphItem);
            foreach (String nameSpace in namespaces.Where(ns => nameStartsWith(ns, startWith)))
            {
                String realNamespace = nameSpace;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part 
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                //
                compList.Add(new XSCompletion(realNamespace, realNamespace, "Namespace " + nameSpace, icon, null));
            }
            //
            // And our own Namespaces
            AddXSharpNamespaces(compList, project, startWith, startLen, icon);
            // We should also add the external NameSpaces
            IList<XProject> prjs = project.ReferencedProjects;
            foreach (var prj in prjs)
            {
                AddXSharpNamespaces(compList, prj, startWith, startLen, icon);
            }
            // And Stranger Projects
            IList<EnvDTE.Project> sprjs = project.StrangerProjects;
            foreach (var prj in sprjs)
            {
                AddStrangerNamespaces(compList, prj, startWith, startLen, icon);
            }
        }

        private void AddXSharpNamespaces(CompletionList compList, XProject project, String startWith, int startLen, ImageSource icon)
        {
            int dotPos;
            // And our own Namespaces
            List<XType> xsNamespaces = project.Namespaces;
            foreach (XType nameSpace in xsNamespaces.Where(ns => nameStartsWith(ns.Name, startWith)))
            {
                String realNamespace = nameSpace.Name;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part 
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                compList.Add(new XSCompletion(realNamespace, realNamespace, nameSpace.Description, icon, null));
            }
        }

        private void AddStrangerNamespaces(CompletionList compList, EnvDTE.Project project, String startWith, int startLen, ImageSource icon)
        {
            int dotPos;
            // get all the items in each project
            foreach (EnvDTE.ProjectItem item in project.ProjectItems)
            {
                EnvDTE.FileCodeModel filecodemodel = item.FileCodeModel;
                if (filecodemodel == null)
                    continue;
                foreach (EnvDTE.CodeElement codeElement in filecodemodel.CodeElements)
                {
                    //
                    if (codeElement.Kind == EnvDTE.vsCMElement.vsCMElementNamespace)
                    {
                        String realNamespace = codeElement.FullName;
                        if (nameStartsWith(realNamespace, startWith))
                        {
                            // remove the start
                            if (startLen > 0)
                                realNamespace = realNamespace.Substring(startLen);
                            // Do we have another part 
                            dotPos = realNamespace.IndexOf('.');
                            // Then remove it
                            if (dotPos > 0)
                                realNamespace = realNamespace.Substring(0, dotPos);
                            compList.Add(new XSCompletion(realNamespace, realNamespace, "Namespace " + codeElement.FullName, icon, null));
                        }
                    }
                }
            }
        }


        private void BuildCompletionList(CompletionList compList, XTypeMember currentMember, String startWith)
        {
            if (currentMember == null)
            {
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find Member.", this._fileName));
#endif
                return;
            }
            // First, look after Parameters
            foreach (XVariable paramVar in currentMember.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(paramVar.GlyphGroup, paramVar.GlyphItem);
                compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Description, icon, null));
            }
            // Then, look for Locals
            // First, look after Parameters
            foreach (XVariable localVar in currentMember.Locals.Where(l => nameStartsWith(l.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(localVar.GlyphGroup, localVar.GlyphItem);
                compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Description, icon, null));
            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //BuildCompletionList(compList, currentMember.Parent, Modifiers.Private);
            //
        }

        /// <summary>
        /// Fill the CompletionList by enumerating the members of the Parent
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="parent">The XType element</param>
        /// <param name="minVisibility">The minimum Visibility</param>
        /// <param name="staticOnly">Static member only ?</param>
        /// <param name="startWith">The filter text</param>
        private void BuildCompletionList(CompletionList compList, XElement parent, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            if (parent == null)
            {
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find Parent.", this._fileName));
#endif
                return;
            }
            if (!(parent is XType))
            {
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Parent {1} is NOT XType.", this._fileName, parent.Name));
#endif
                return;
            }
            //
            XType Owner = parent as XType;
            //
            foreach (XTypeMember elt in Owner.Members.Where(e => nameStartsWith(e.Name, startWith)))
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic != staticOnly)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null));
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
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find CompletionType.", this._fileName));
#endif
                return;
            }
            //
            if (cType.XType != null)
            {
                FillMembers(compList, cType.XType, minVisibility, staticOnly, startWith);
                // Hummm, we should call for Owner of the Owner.. Super !
                cType.XType.ForceComplete();
                if (cType.XType.Parent != null)
                {
                    // Parent is a XElement, so one of our Types
                    BuildCompletionList(compList, new CompletionType((XType)cType.XType.Parent), Modifiers.Protected, staticOnly, startWith);
                }
                else if (cType.XType.ParentName != null)
                {
                    // Parent has just a Name, so one of the System Types
                    BuildCompletionList(compList, new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), Modifiers.Protected, staticOnly, startWith);
                }
            }
            else if (cType.SType != null)
            {
                // Now add Members for System types
                FillMembers(compList, cType.SType, minVisibility, staticOnly, startWith);
            }
        }


        private bool nameStartsWith(string name, string startWith)
        {
            return name.StartsWith(startWith, this._settingIgnoreCase, System.Globalization.CultureInfo.InvariantCulture);
        }
        /// <summary>
        /// Add Members for our Project Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="xType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, XType xType, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            // Add Members for our Project Types
            foreach (XTypeMember elt in xType.Members.Where(x => nameStartsWith(x.Name, startWith)))
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic != staticOnly)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null));
            }
        }

        /// <summary>
        /// Add Members from System Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="sType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, System.Type sType, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            MemberInfo[] members;
            //
            if (minVisibility < Modifiers.Public)
            {
                // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance
                    | BindingFlags.Static | BindingFlags.DeclaredOnly);
            }
            else
            {
                //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers();
            }
            //
            foreach (var member in members.Where(x => nameStartsWith(x.Name, startWith)))
            {
                MemberAnalysis analysis = new MemberAnalysis(member);
                if ((analysis.IsInitialized) && (minVisibility <= analysis.Visibility))
                {
                    if (analysis.Kind == Kind.Constructor)
                        continue;
                    if (analysis.IsStatic != staticOnly)
                    {
                        continue;
                    }
                    String toAdd = "";
                    if ((analysis.Kind == Kind.Method))
                    {
                        toAdd = "(";
                    }
                    //
                    ImageSource icon = _provider.GlyphService.GetGlyph(analysis.GlyphGroup, analysis.GlyphItem);
                    compList.Add(new XSCompletion(analysis.Name, analysis.Name + toAdd, analysis.Description, icon, null));
                }
            }
            // fill members of parent class
            if (sType.BaseType != null)
            {
                if (minVisibility == Modifiers.Private)
                    minVisibility = Modifiers.Protected;
                FillMembers(compList, sType.BaseType, minVisibility, staticOnly, startWith);
            }
            // We will miss the System.Object members
            if (sType.IsInterface)
            {
                System.Type obj = typeof(object);
                FillMembers(compList, obj, minVisibility, staticOnly, startWith);
            }
            if (sType.IsEnum)
            {
                // todo:
                // Fill enum members
                var names = sType.GetEnumNames();
                var scope = StandardGlyphItem.GlyphItemPublic;
                // must be public or nested, otherwise we would not see it.
                if (sType.IsNested)
                {
                    scope = StandardGlyphItem.GlyphItemFriend;
                }
                ImageSource icon = _provider.GlyphService.GetGlyph(StandardGlyphGroup.GlyphGroupEnumMember, scope);
                foreach (string name in names)
                {
                    compList.Add(new XSCompletion(name, name, "", icon, null));
                }

            }
        }

        public void Dispose()
        {
            _disposed = true;
        }

        /// <summary>
        /// Flatten the TokenList as a String
        /// </summary>
        /// <param name="tokenList"></param>
        /// <returns></returns>
        private String TokenListAsString(List<String> tokenList, int less)
        {
            String retValue = "";
            for (int pos = 0; pos < tokenList.Count - less; pos++)
            {
                String tk = tokenList[pos];
                retValue += tk;
            }
            return retValue;
        }

    }

    /// <summary>
    /// Process a MemberInfo in order to provide usable informations ( TypeName, Glyph, ... )
    /// </summary>
    internal class MemberAnalysis
    {
        class ParamInfo
        {
            public String Name;
            public String TypeName;

            internal ParamInfo(String n, String t)
            {
                this.Name = n;
                this.TypeName = t;
            }
        }

        private String _name;
        private Modifiers _modifiers;
        private Modifiers _visibility;
        private Kind _kind;
        private bool _isStatic;
        private String _typeName;
        private List<ParamInfo> _parameters;


        internal MemberAnalysis(MemberInfo member)
        {
            Type declType;
            //
            this._name = member.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            this._typeName = "";
            this._parameters = new List<ParamInfo>();
            //
            switch (member.MemberType)
            {
                case MemberTypes.Constructor:
                    this._kind = Kind.Constructor;
                    ConstructorInfo constInfo = member as ConstructorInfo;
                    this._isStatic = constInfo.IsStatic;
                    //
                    if (constInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (constInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (constInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (constInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = constInfo.DeclaringType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Event:
                    this._kind = Kind.Event;
                    EventInfo evt = member as EventInfo;
                    MethodInfo methodInfo = evt.GetAddMethod(true);
                    if (methodInfo == null)
                    {
                        methodInfo = evt.GetRemoveMethod(true);
                    }
                    //
                    this._isStatic = methodInfo.IsStatic;
                    //
                    if (methodInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (methodInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (methodInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (methodInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = evt.EventHandlerType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Field:
                    this._kind = Kind.ClassVar;
                    if (member.DeclaringType.IsEnum)
                        this._kind = Kind.Enum;
                    FieldInfo field = member as FieldInfo;
                    //
                    this._isStatic = field.IsStatic;
                    //
                    if (field.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (field.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (field.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = field.FieldType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Method:
                    this.Kind = Kind.Method;
                    MethodInfo method = member as MethodInfo;
                    if (method.IsSpecialName)
                    {
                        // The SpecialName bit is set to flag members that are treated in a special way by some compilers (such as property accessors and operator overloading methods).
                        this._name = null;
                        break;
                    }
                    //
                    this._isStatic = method.IsStatic;
                    //
                    if (method.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (method.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (method.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    ParameterInfo[] pars = method.GetParameters();
                    foreach (ParameterInfo p in pars)
                    {
                        this._parameters.Add(new ParamInfo(p.Name, p.ParameterType.FullName));
                    }
                    //
                    declType = method.ReturnType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Property:
                    this.Kind = Kind.Property;
                    PropertyInfo prop = member as PropertyInfo;
                    MethodInfo propInfo = prop.GetGetMethod(true);
                    if (propInfo == null)
                    {
                        propInfo = prop.GetSetMethod(true);
                    }
                    //
                    this._isStatic = propInfo.IsStatic;
                    //
                    if (propInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (propInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (propInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = prop.PropertyType;
                    this._typeName = declType.FullName;
                    break;
                default:
                    // Mark as Not-Initialized
                    this._name = null;
                    break;
            }
        }

        public bool IsInitialized
        {
            get
            {
                return (this.Name != null);
            }
        }

        public string Name
        {
            get
            {
                return _name;
            }
        }

        public String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if (this.Kind != Kind.ClassVar)
                    desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                if ((this.Kind == Kind.Method) || (this.Kind == Kind.Function) || (this.Kind == Kind.Property) || (this.Kind == Kind.ClassVar) || (this.Kind == Kind.Event))
                {
                    desc += " as " + this.TypeName;
                }
                //
                return desc;
            }
        }

        public String Prototype
        {
            get
            {
                if ((this.Kind == Kind.Property) || (this.Kind == Kind.Access) || (this.Kind == Kind.ClassVar) || (this.Kind == Kind.Enum) || (this.Kind == Kind.Event))
                    return this.Name;
                //
                String vars = "";
                foreach (var var in this.Parameters)
                {
                    if (vars.Length > 0)
                        vars += ", ";
                    vars += var.Name + " as " + var.TypeName;
                }
                //
                String desc = "";
                desc += this.Name;
                desc += "(";
                desc += vars;
                desc += ")";
                //
                return desc;
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
                    case Kind.Namespace:
                        imgG = StandardGlyphGroup.GlyphGroupNamespace;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                        imgG = StandardGlyphGroup.GlyphGroupMethod;
                        break;
                    case Kind.Structure:
                    case Kind.Union:
                        imgG = StandardGlyphGroup.GlyphGroupStruct;
                        break;
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        imgG = StandardGlyphGroup.GlyphGroupProperty;
                        break;
                    case Kind.Local:
                        imgG = StandardGlyphGroup.GlyphGroupVariable;
                        break;
                    case Kind.Enum:
                        imgG = StandardGlyphGroup.GlyphGroupEnumMember;
                        break;
                    case Kind.VOGlobal:
                    case Kind.ClassVar:
                        imgG = StandardGlyphGroup.GlyphGroupField;
                        break;
                    case Kind.Delegate:
                        imgG = StandardGlyphGroup.GlyphGroupDelegate;
                        break;
                    case Kind.Event:
                        imgG = StandardGlyphGroup.GlyphGroupEvent;
                        break;
                    case Kind.Interface:
                        imgG = StandardGlyphGroup.GlyphGroupInterface;
                        break;
                    case Kind.VODefine:
                        imgG = StandardGlyphGroup.GlyphGroupConstant;
                        break;
                    case Kind.Class:
                    default:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
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
                    case Modifiers.Public:
                    default:
                        imgI = StandardGlyphItem.GlyphItemPublic;
                        break;
                }
                //
                return imgI;
            }
        }

        public Kind Kind
        {
            get
            {
                return _kind;
            }

            set
            {
                _kind = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _modifiers;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _visibility;
            }
        }

        public string TypeName
        {
            get
            {
                return _typeName;
            }
        }

        private List<ParamInfo> Parameters
        {
            get
            {
                return _parameters;
            }

            set
            {
                _parameters = value;
            }
        }

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }
    }

    /// <summary>
    /// Process a TypeInfo in order to provide usable informations (TypeName, Glyph, ... )
    /// </summary>
    internal class TypeAnalysis
    {
        private String _name;
        private Modifiers _modifiers;
        private Modifiers _visibility;
        private Kind _kind;
        private bool _isStatic;


        internal TypeAnalysis(TypeInfo typeInfo)
        {
            //
            this._name = typeInfo.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            //
            if (typeInfo.IsClass)
            {
                this._kind = Kind.Class;
            }
            else if (typeInfo.IsEnum)
            {
                this._kind = Kind.Enum;
            }
            else if (typeInfo.IsInterface)
            {
                this._kind = Kind.Interface;
            }
            else if (typeInfo.IsValueType && !typeInfo.IsPrimitive)
            {
                this._kind = Kind.Structure;
            }
            //
            this._isStatic = (typeInfo.IsAbstract && typeInfo.IsSealed);
            //
            if (!this.IsStatic)
            {
                if (typeInfo.IsAbstract)
                {
                    this._modifiers = Modifiers.Abstract;
                }
            }
            ////
            //if (typeInfo.IsPrivate)
            //{
            //    this._visibility = Modifiers.Private;
            //}
            //if (typeInfo.IsAssembly)
            //{
            //    this._visibility = Modifiers.Internal;
            //}
            //if (typeInfo.IsFamily)
            //{
            //    this._visibility = Modifiers.Protected;
            //}
            //
        }

        public bool IsInitialized
        {
            get
            {
                return (this.Name != null);
            }
        }

        public string Name
        {
            get
            {
                return _name;
            }
        }

        public String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if (this.Kind != Kind.ClassVar)
                    desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                return desc;
            }
        }

        public String Prototype
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

        public Kind Kind
        {
            get
            {
                return _kind;
            }

            set
            {
                _kind = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _modifiers;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _visibility;
            }
        }

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }
    }
    /// <summary>
    /// XSharp CompletionList
    /// Overload the Add() Method to support "overloads"
    /// </summary>
    internal class CompletionList : List<XSCompletion>
    {
        public new void Add(XSCompletion item)
        {
            int overloads = 0;
            //
            foreach (XSCompletion comp in this)
            {
                // Search for the same Name
                if (comp.DisplayText == item.DisplayText)
                {
                    // Already exists in the List !!
                    // First Overload ?
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

                    // Ok, Forget about the newly added Completion please
                    return;
                }
            }
            // Unknown, Standard behaviour
            base.Add(item);
        }
    }

    /// <summary>
    /// XSharp Completion class.
    /// Overload the Description property in order to add "overload" text at the end
    /// </summary>
    public class XSCompletion : Completion
    {
        public XSCompletion(string displayText, string insertionText, string description, ImageSource iconSource, string iconAutomationText)
            : base(displayText, insertionText, description, iconSource, iconAutomationText)
        {
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
        /// <returns></returns>
        public static List<String> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            string bufferText, out IToken stopToken, bool fromGotoDefn, string fileName)
        {
            List<String> tokenList = new List<string>();
            String token;
            //
            stopToken = null;
            //tokenStream = null;
            // lex the entire document
            // Get compiler options
            XFile file = XSolution.FindFile(fileName);
            XSharpParseOptions parseoptions = null;
            if (file != null)
            {
                var prj = file.Project.ProjectNode;
                parseoptions = prj.ParseOptions;

            }
            var lexer = XSharpLexer.Create(bufferText, fileName, parseoptions);
            var tokens = lexer.GetTokenStream();

            // locate the last token before the trigger point
            IToken nextToken;
            while (true)
            {
                nextToken = tokens.Lt(1);
                if (nextToken.Type == XSharpLexer.Eof) // End Of File
                    break;
                // Move after the TriggerPoint
                if (nextToken.StartIndex >= triggerPointPosition)
                    break;

                tokens.Consume();
            }
            if (nextToken == null)
            {
                return tokenList;
            }
            // We are looking at line
            int lineTP = triggerPointLineNumber + 1;
            // And we are on line
            int lineNT = nextToken.Line;
            //
            if (lineTP != lineNT)
            {
                // ???
                return tokenList;
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
            if (!String.IsNullOrEmpty(token))
                tokenList.Add(token);
            triggerToken = GetPreviousToken(tokens, nextToken);
            //
            while (triggerToken != null)
            {
                token = triggerToken.Text;
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
                        if (XSharpLexer.IsKeyword(triggerToken.Type) ||
                            XSharpLexer.IsOperator(triggerToken.Type)
                            )
                        {
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
                    triggerToken = GetPreviousToken(tokens, triggerToken);
            }
            // 
            tokenList.Reverse();
            // Now, we may have some post-treatment
            List<String> returnList = new List<string>();
            int i = 0;
            bool prevWasDot = false;
            while (i < tokenList.Count)
            {
                token = tokenList[i];
                if ((token.CompareTo("()") == 0) || (token.CompareTo("{}") == 0) || (token.CompareTo("[]") == 0))
                {
                    if (returnList.Count > 0)
                    {
                        String prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                    }
                }
                else if (token.CompareTo(".") == 0)
                {
                    if (returnList.Count > 0)
                    {
                        String prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                        prevWasDot = true;
                    }
                }
                else
                {
                    if (prevWasDot)
                    {
                        String prevToken = returnList[returnList.Count - 1];
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

        public static XType FindNamespace(int position, String fileName)
        {
            XFile file = XSharpModel.XSolution.FindFullPath(fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Cannot find file {0} .", fileName));
#endif
                return null;
            }
            //
            XType found = null;
            foreach (XType eltType in file.TypeList.Values)
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
                if (found.NameSpace?.Length > 0)
                    name = found.NameSpace + "." + name;
                var pos = name.LastIndexOf('.');
                if (pos > 0)
                {
                    name = name.Substring(0, pos);
                }
                XType nSpace = new XType(name, Kind.Namespace, Modifiers.None,
                    Modifiers.Public, found.Range, found.Interval);
                return nSpace;
            }
#if TRACE
            // a source file without a namespace is really not a problem
            //System.Diagnostics.Debug.WriteLine(String.Format("Cannot find namespace at position {0} in file {0} .", position, fileName));
#endif
            return null;
        }

        public static XTypeMember FindMember(int position, String fileName)
        {
            XFile file = XSharpModel.XSolution.FindFullPath(fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
#if TRACE
                System.Diagnostics.Debug.WriteLine(String.Format("Cannot find file {0} .", fileName));
#endif
                return null;
            }
            // First, Check for Function/Procedure
            XType gbl = file.GlobalType;
            int maxPosition = 0;
            XTypeMember lastElt = null;
            //
            foreach (XTypeMember elt in gbl.Members)
            {
                if (elt.Interval.Start < position)
                {
                    if (elt.Interval.Stop > maxPosition)
                    {
                        // Keep track of the last position, in the corresponding XType
                        maxPosition = elt.Interval.Stop;
                        lastElt = elt;
                    }
                }
                if (elt.Interval.ContainsInclusive(position))
                {
                    return elt;
                }
            }
            // If we are here, we found nothing
            // but we might be after the code of the last Function in the file, so the parser don't know where we are
            // Keep it in lastElt, and check Members
            XTypeMember lastElt2 = null;
            foreach (XType eltType in file.TypeList.Values)
            {
                if (eltType.Interval.ContainsInclusive(position))
                {
                    //
                    foreach (XTypeMember elt in eltType.Members)
                    {
                        if (elt.Interval.Start < position)
                        {
                            if (elt.Interval.Stop > maxPosition)
                            {
                                // Keep track of the last position, in the corresponding XType
                                maxPosition = elt.Interval.Stop;
                                lastElt2 = elt;
                            }
                        }
                        if (elt.Interval.ContainsInclusive(position))
                        {
                            return elt;
                        }
                    }
                    // We are in the right Type, but found no Member ?
                    // So it must the Last Member, before the Closing-Keyword
                    if ((lastElt != null) && (lastElt2 == null))
                        return lastElt;
                    //
                    return lastElt2;
                }
            }
            //
            if (lastElt != null)
                return lastElt;
            //
#if DEBUG
            System.Diagnostics.Debug.WriteLine(String.Format("Cannot find member as position {0} in file {0} .", position, file.FullPath));
#endif
            return null;
        }

        /// <summary>
        /// Retrieve the CompletionType based on :
        ///  The Token list returned by GetTokenList()
        ///  The Token that stops the building of the Token List.
        /// </summary>
        /// <param name="tokenList"></param>
        /// <param name="currentMember"></param>
        /// <param name="stopToken"></param>
        /// <param name="foundElement"></param>
        /// <param name="sysFoundElement"></param>
        /// <returns></returns>
        public static CompletionType RetrieveType(string fileName, List<string> tokenList, XTypeMember currentMember, String currentNS, IToken stopToken, out XElement foundElement, out MemberInfo sysFoundElement)
        {
            foundElement = null;
            sysFoundElement = null;
            if (currentMember == null)
            {
                // try to find the first member in the file
                var file = XSharpModel.XSolution.FindFile(fileName);
                if (file != null)
                    currentMember = file.FirstMember();
                if (currentMember == null)
                {
#if TRACE
                    System.Diagnostics.Debug.WriteLine(String.Format("Retrieve current Type : Member cannot be null."));
#endif
                    return null;
                }

            }
            // we have to walk the tokenList, searching for the current Type
            // As we have separators every even token, we will walk by step 2
            int currentPos = 0;
            String currentToken = "";
            XElement element = null;
            CompletionType cType = null;
            CompletionType cTemp = null;
            if (tokenList.Count == 0)
                return null;
            // Context Type....
            cType = new CompletionType(currentMember.Parent.Clone);
            Modifiers visibility = Modifiers.Private;
            //
            while (currentPos < tokenList.Count)
            {
                currentToken = tokenList[currentPos];
                //
                int dotPos = currentToken.LastIndexOf(".");
                if (dotPos > -1)
                {
                    String startToken = currentToken.Substring(0, dotPos);
                    cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.NameSpace);
                    if (cType.IsEmpty())
                    {
                        // could be namespace.Type
                        // so now try with right side of the string
                        startToken = currentToken.Substring(dotPos + 1);
                        cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.NameSpace);
                        if (!cType.IsEmpty())
                            return cType;
                    }
                    currentToken = currentToken.Substring(dotPos + 1);
                    currentPos++;
                    if (String.IsNullOrEmpty(currentToken))
                    {
                        continue;
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
                        case XSharpLexer.IMPLEMENTS:
                        case XSharpLexer.INHERIT:
                            if (tokenList.Count == 1)
                            {
                                // One Token, after such keyword, this is a type
                                // So we are looking for a Type, and we must end with a {}
                                if (!currentToken.EndsWith("{}"))
                                {
                                    currentToken += "{}";
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
                    // this a Constructor call
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    cType = new CompletionType(currentToken, currentMember.File, currentMember.Parent.NameSpace);
                    if (!cType.IsEmpty())
                    {
                        if (cType.XType != null)
                        {
                            foundElement = cType.XType;
                        }
                        else
                        {
                            sysFoundElement = cType.SType;
                        }
                    }
                }
                else if (currentToken.EndsWith("()"))
                {
                    // this a Method call
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    // Do we already know in which Type we are ?
                    if (!cType.IsEmpty())
                    {
                        // Now, search for a Method
                        cType = SearchMethodTypeIn(cType, currentToken, visibility, out foundElement, out sysFoundElement);
                    }
                    if (cType.IsEmpty())
                    {
                        // check to see if this is a method from the Object Type, such as ToString().
                        cTemp = SearchMethodTypeIn(new CompletionType(typeof(object)), currentToken, visibility, out foundElement, out sysFoundElement);
                        if (!cTemp.IsEmpty())
                        {
                            cType = cTemp;
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        cType = null;
                    }
                }
                //else if (currentToken.EndsWith("[]"))
                //{
                //}
                else
                {
                    // First token, so it could be a parameter or a local var
                    if (currentPos == 0)
                    {
                        // Search in Parameters
                        element = currentMember.Parameters.Find(x => StringEquals(x.Name, currentToken));
                        if (element == null)
                        {
                            // then Locals
                            element = currentMember.Locals.Find(x => StringEquals(x.Name, currentToken));
                            if (element == null)
                            {
                                // We can have a Property/Field of the current CompletionType
                                if (!cType.IsEmpty())
                                {
                                    cType = SearchPropertyOrFieldIn(cType, currentToken, visibility, out foundElement);
                                }
                            }
                        }
                        if (element != null)
                        {
                            cType = new CompletionType((XVariable)element, currentNS);
                            foundElement = element;
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
                }
                //
                if (cType.IsEmpty())
                {
                    cType = null;
                }
                // Next Token
                currentPos += 2;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                //
                visibility = Modifiers.Public;
                if (foundElement != null)
                {
                    if (String.Compare(foundElement.Name, "self", true) == 0)
                    {
                        visibility = Modifiers.Private;
                    }
                    else if (String.Compare(foundElement.Name, "super", true) == 0)
                    {
                        visibility = Modifiers.Protected;
                    }
                }
            }
            return cType;
        }


        /// <summary>
        /// Search for a Property or a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchPropertyOrFieldIn(CompletionType cType, string currentToken, Modifiers minVisibility, out XElement foundElement)
        {
            CompletionType result = SearchPropertyTypeIn(cType, currentToken, minVisibility, out foundElement);
            if (result.IsEmpty())
            {
                result = SearchFieldTypeIn(cType, currentToken, minVisibility, out foundElement);
            }
            return result;
        }


        /// <summary>
        /// Search for a Property, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchPropertyTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out XElement foundElement)
        {
            foundElement = null;
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.Property) || (x.Kind == Kind.Access) || (x.Kind == Kind.Assign))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                });
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public, out foundElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                    foundElement = element;
                    return cType;
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                foreach (var member in members)
                {
                    if (StringEquals(member.Name, currentToken))
                    {
                        PropertyInfo prop = member as PropertyInfo;
                        declType = prop.PropertyType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchPropertyTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, out foundElement);
                    }
                    // not needed: no properties in object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchPropertyTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, out foundElement);
                    //}
                }
                else
                {
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Field we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Field (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchFieldTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out XElement foundElement)
        {
            foundElement = null;
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.ClassVar))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                });
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Protected, out foundElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                    foundElement = element;
                    return cType;
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                foreach (var member in members)
                {
                    if (StringEquals(member.Name, currentToken))
                    {
                        FieldInfo field = member as FieldInfo;
                        declType = field.FieldType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchFieldTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, out foundElement);
                    }
                    // not needed: no fields in object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchFieldTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, out foundElement);
                    //}
                }
                else
                {
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Method, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Method we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType that the Method will return (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchMethodTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out XElement foundElement, out MemberInfo systemElement)
        {
            foundElement = null;
            systemElement = null;
            if (cType.XType != null)
            {
                // 
                XTypeMember xMethod = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.Method))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                });
                //if (elt.IsStatic)
                //    continue;
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                //
                if (xMethod == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public, out foundElement, out systemElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Public, out foundElement, out systemElement);
                    }
                }
                else
                {
                    foundElement = xMethod;
                    if (xMethod.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return new CompletionType(xMethod.Parent.Clone);
                    }
                    else if (xMethod.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return new CompletionType(xMethod.ParentName, xMethod.FileUsings);
                    }
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                MethodInfo method = null;
                foreach (var member in members)
                {
                    if (member.MemberType == MemberTypes.Method)
                    {
                        if (StringEquals(member.Name, currentToken))
                        {
                            method = member as MethodInfo;
                            declType = method.ReturnType;
                            break;
                        }
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchMethodTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, out foundElement, out systemElement);
                    }
                    else if (cType.SType.IsInterface)
                    {
                        return SearchMethodTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, out foundElement, out systemElement);
                    }
                }
                else
                {
                    systemElement = method;
                    return new CompletionType(declType);
                }
            }
            else if (cType.CodeElement != null)
            {

            }
            // Sorry, not found
            return new CompletionType();
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

        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken)
        {
            IToken prev = null;
            if (currentToken != null)
            {
                prev = currentToken;
                int Line = prev.Line;
                do
                {
                    if (prev.TokenIndex == 0)
                        break;
                    prev = tokens.Get(prev.TokenIndex - 1);
                    if (prev.Line != Line)
                    {
                        prev = null;
                    }
                } while ((prev != null) && (String.IsNullOrWhiteSpace(prev.Text)));
            }
            return prev;
        }

    }


    // Build a list of all Keywords
    internal static class XSharpTypes
    {
        static List<XType> _xTypes;

        static XSharpTypes()
        {
            // Dummy call to a Lexer; just to copy the Keywords, Types, ...
            var lexer = XSharpLexer.Create("", "");
            //

            _xTypes = new List<XType>();
            //
            foreach (var keyword in lexer.KwIds)
            {
                _xTypes.Add(new XType(keyword.Key, Kind.Keyword, Modifiers.None, Modifiers.Public, TextRange.Empty, TextInterval.Empty));
            }
            //
        }

        internal static List<XType> Get()
        {
            List<XType> retTypes = new List<XType>();
            retTypes.AddRange(_xTypes);
            return retTypes;
        }
    }
}


