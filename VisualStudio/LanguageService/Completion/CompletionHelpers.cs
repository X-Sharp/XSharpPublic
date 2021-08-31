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
    internal class CompletionHelpers
    {
        internal IGlyphService _glyphService = null;
        private XSharpDialect _dialect;
        private XFile _file;
        private bool _settingIgnoreCase;
        internal CompletionHelpers(XSharpDialect dialect, IGlyphService glyphService, XFile file, bool ignoreCase)
        {
            _dialect = dialect;
            _glyphService = glyphService;
            _file = file;
            _settingIgnoreCase = ignoreCase;
        }
        internal void AddUsingStaticMembers(XCompletionList compList, XFile file, string filterText)
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
        internal void AddTypeNames(XCompletionList compList, XProject project, string startWith, List<string> usings, bool onlyInterfaces = false)
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
                    realTypeName = realTypeName.Substring(dotPos + 1);
                if (IsHiddenTypeName(realTypeName))
                    continue;

                //
                ImageSource icon = _glyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeAnalysis.Prototype, icon, null, Kind.Class, "")))
                    break;
            }
            //
            // And our own Types
            AddXSharpTypeNames(compList, project, startWith, usings.ToArray());

        }

        internal bool IsHiddenTypeName(string realTypeName)
        {
            if (realTypeName.Length > 2 && realTypeName.StartsWith("__", StringComparison.Ordinal) && XSettings.EditorHideAdvancedMembers)
                return true;
            if (realTypeName.IndexOf('$') >= 0)
                return true;
            return false;
        }


        internal bool IsHiddenMemberName(string realMemberName)
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
                if (realMemberName.StartsWith("add_", StringComparison.Ordinal))
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

        internal void AddXSharpKeywords(XCompletionList compList, string startWith)
        {
            foreach (var kw in XSharpTypes.Get().Where(ti => nameStartsWith(ti.Name, startWith)))
            {
                ImageSource icon = _glyphService.GetGlyph(kw.getGlyphGroup(), kw.getGlyphItem());
                compList.Add(new XSCompletion(kw.Name, kw.Name, kw.Prototype, icon, null, Kind.Keyword, ""));
            }
        }

        internal void AddXSharpTypeNames(XCompletionList compList, XProject project, string startWith, IList<string> usings)
        {
            var list = project.GetTypes(startWith, usings);
            foreach (var typeInfo in list)
            {

                // Then remove it
                ImageSource icon = _glyphService.GetGlyph(typeInfo.getGlyphGroup(), typeInfo.getGlyphItem());
                if (!compList.Add(new XSCompletion(typeInfo.Name, typeInfo.Name, typeInfo.FullName, icon, null, typeInfo.Kind, "")))
                    break;
            }
        }

        internal void AddXSharpKeywordTypeNames(XCompletionList compList, string startWith)
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
                ImageSource icon = _glyphService.GetGlyph(typeInfo.getGlyphGroup(), typeInfo.getGlyphItem());
                if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Prototype, icon, null, Kind.Class, "")))
                    break;
            }
        }


        internal void AddNamespaces(XCompletionList compList, XProject project, string startWith)
        {
            // We are looking for NameSpaces, in References
            if (startWith == null)
                return;
            var namespaces = project.AllNamespaces;
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            ImageSource icon = _glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupNamespace, StandardGlyphItem.GlyphItemPublic);
            ImageSource iconClass = _glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupClass, StandardGlyphItem.GlyphItemPublic);
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

        internal void AddXSharpNamespaces(XCompletionList compList, XProject project, string startWith, ImageSource icon)
        {
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            // And our own Namespaces
            var xsNamespaces = project.ProjectNamespaces;
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
                if (!compList.Add(new XSCompletion(realNamespace, realNamespace, nameSpace, icon, null, Kind.Namespace, "")))
                    break;
            }
        }
        internal void BuildCompletionList(XCompletionList compList, XSourceMemberSymbol currentMember, string startWith, XSharpSearchLocation location)
        {
            if (currentMember == null)
            {
                return;
            }
            // First, look after Parameters
            foreach (var paramVar in currentMember.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                //
                ImageSource icon = _glyphService.GetGlyph(paramVar.getGlyphGroup(), paramVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Prototype, icon, null, Kind.Parameter, "")))
                    break;
            }
            // Then, look for Locals
            // line numbers in the range are 1 based. currentLine = 0 based !
            foreach (var localVar in currentMember.GetLocals(location).Where(l => nameStartsWith(l.Name, startWith) && l.Range.StartLine - 1 <= location.LineNumber))
            {
                //
                ImageSource icon = _glyphService.GetGlyph(localVar.getGlyphGroup(), localVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Prototype, icon, null, Kind.Local, "")))
                    break;

            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //

            if (currentMember.Kind.IsClassMember(_dialect))
            {
                var classElement = currentMember.Parent as XSourceTypeSymbol;
                foreach (var member in classElement.GetMembers(startWith).Where(m => m.Kind == Kind.Field))
                {
                    ImageSource icon = _glyphService.GetGlyph(member.getGlyphGroup(), member.getGlyphItem());
                    if (!compList.Add(new XSCompletion(member.Name, member.Name, member.Prototype, icon, null, Kind.Field, "")))
                        break;
                }
            }
        }


        internal void BuildCompletionList(XCompletionList compList, IXTypeSymbol type, Modifiers minVisibility, bool staticOnly, string startWith)
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
                var baseType = sourceType.BaseTypeName;
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
            if (!staticOnly)
            {
                FillExtensions(compList, type, startWith);
            }
        }
        internal bool nameStartsWith(string name, string startWith)
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
        internal void FillMembers(XCompletionList compList, IXTypeSymbol type, IEnumerable<IXMemberSymbol> members, Modifiers minVisibility, bool staticOnly)
        {
            if (members.Count() == 0)
                return;
            WriteOutputMessage($"FillMembers {type?.FullName}: {members.Count()} members");
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
                        if (staticOnly && elt.IsStatic && type != null)
                        {
                            if (elt.Parent.FullName == "System.Object" && type.FullName != "System.Object")
                                add = false;
                        }
                        break;
                }
                if (type != null && elt.IsStatic && type.Kind == Kind.Enum && elt.DeclaringType != type.FullName && elt.Name != "HasFlag")
                    add = false;
                if (!add)
                    continue;
                //
                ImageSource icon = _glyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                string toAdd = "";
                if (elt.Kind.HasParameters() && elt.Kind != Kind.Constructor && !elt.Kind.IsProperty())
                {
                    toAdd = "(";
                }
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Prototype, icon, null, elt.Kind, elt.Value)))
                    break;
            }
        }
        /// <summary>
        /// Add Members for our Types to the completionlist
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="xType"></param>
        /// <param name="minVisibility"></param>
        internal void FillMembers(XCompletionList compList, IXTypeSymbol xType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            FillMembers(compList, xType, xType.GetMembers(startWith), minVisibility, staticOnly);
        }


        internal void FillExtensions(XCompletionList compList, IXTypeSymbol type, string startWith)
        {
            WriteOutputMessage($"FillExtensions for type {type?.FullName}");
            if (type != null)
            {
                var extensions = _file.Project.GetExtensions(type.FullName);
                IEnumerable<IXMemberSymbol> selection = extensions;
                if (!string.IsNullOrEmpty(startWith))
                {
                    selection = extensions.Where(x => nameStartsWith(x.Name, startWith));
                }
                if (selection.Count() > 0)
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
                    if (selection.Count() > 0)
                        FillMembers(compList, null, selection, Modifiers.Public, true);
                }
            }
            //WriteOutputMessage($"FillExtensions complete for type {sType.FullName}");
        }

        internal void BuildTabs(XCompletionList compList, IList<CompletionSet> completionSets, ITrackingSpan applicableTo)
        {
            var values = compList.Values;
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



        /// <summary>
        /// Flatten the TokenList as a String
        /// </summary>
        /// <param name="tokenList"></param>
        /// <returns></returns>
        internal string TokenListAsString(List<XSharpToken> tokenList, IToken fromToken)
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
        internal static void WriteOutputMessage(string strMessage)
        {
            XSharpCompletionSource.WriteOutputMessage(strMessage);
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
            var walker = new SourceWalker(new XFile(member.File.FullPath, member.File.Project), false);
            var start = member.Interval.Start;
            var end = member.Interval.Width;
            if (start + end > location.Snapshot.Length)
                end = location.Snapshot.Length - start;
            var memberSource = location.Snapshot.GetText(start, end);

            var locals = walker.ParseLocals(memberSource, member);
            // Add the normal locals for class members
            foreach (var local in locals)
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
                    XVar = new XSourceVariableSymbol(member, "SUPER", member.Range, member.Interval, member.ParentType.BaseTypeName);
                    XVar.File = walker.File;
                    locals.Add(XVar);
                }
            }
            return locals;
        }
    }
}

