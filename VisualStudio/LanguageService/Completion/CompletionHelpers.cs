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
using XSharpModel;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.CodeAnalysis.XSharp;

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

        internal void AddTypeNames(XCompletionList compList, XSharpSearchLocation location, string startWith,
            bool onlyInterfaces = false, bool afterDot = false)
        {

            if (startWith == null)
                return;
            // PE Types
            AddPETypeNames(compList, location, startWith, onlyInterfaces, afterDot);
            // Find Source Types
            AddSourceTypeNames(compList, location, startWith, onlyInterfaces, afterDot);

            // And our own Types
            AddXSharpTypeNames(compList, location, startWith);

        }


        bool isHiddenTypeSymbol(IXTypeSymbol type, out string displayName)
        {
            string fullName = type.FullName;
            displayName = fullName;
            if (IsHiddenTypeName(fullName))
            {
                return true;
            }
            if (fullName.Contains("+"))
            {
                fullName = fullName.Replace('+', '.');
            }
            displayName = fullName;
            // Do we have another part file
            var dotPos = fullName.LastIndexOf('.');
            // Then remove it
            if (dotPos > 0)
                displayName = displayName.Substring(dotPos + 1);
            if (IsHiddenTypeName(displayName))
                return true;
            return false;
        }

        internal void AddPETypeNames(XCompletionList compList, XSharpSearchLocation location, string startWith,
            bool onlyInterfaces = false, bool afterDot = false)
        {

            IList<XPETypeSymbol> types;
            if (afterDot)
                types = location.Project.GetAssemblyTypesInNamespace(startWith, location.Usings.ToArray());
            else
                types = location.Project.GetAssemblyTypes(startWith, location.Usings.ToArray());


            foreach (var type in types)
            {
                if (onlyInterfaces && type.Kind != Kind.Interface)
                    continue;
                if (isHiddenTypeSymbol(type, out var displayName))
                    continue;

                if (!afterDot && !displayName.StartsWith(startWith,StringComparison.OrdinalIgnoreCase) &&
                    ! type.FullName.StartsWith(startWith, StringComparison.OrdinalIgnoreCase))
                    continue;
                var typeAnalysis = new XTypeAnalysis(type);

                ImageSource icon = _glyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                if (!compList.Add(new XSCompletion(displayName, displayName, typeAnalysis.Prototype, icon, null, Kind.Class, "")))
                    break;
            }
        }
        internal void AddSourceTypeNames(XCompletionList compList, XSharpSearchLocation location, string startWith,
            bool onlyInterfaces = false, bool afterDot = false)
        {
            IList<XSourceTypeSymbol> types;
            if (afterDot)
                types = location.Project.GetProjectTypesInNamespace(startWith, location.Usings.ToArray());
            else
                types = location.Project.GetTypes(startWith, location.Usings.ToArray());

            foreach (var type in types)
            {
                if (onlyInterfaces && type.Kind != Kind.Interface)
                    continue;
                if (isHiddenTypeSymbol(type, out var displayName))
                    continue;

                if (!afterDot && !displayName.StartsWith(startWith) &&
                    !type.FullName.StartsWith(startWith, StringComparison.OrdinalIgnoreCase))
                    continue;
                var typeAnalysis = new XTypeAnalysis(type);

                ImageSource icon = _glyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                if (!compList.Add(new XSCompletion(displayName, displayName, typeAnalysis.Prototype, icon, null, Kind.Class, "")))
                    break;
            }
        }
        internal static bool IsHiddenTypeName(string realTypeName)
        {
            if (realTypeName.Length > 2 && realTypeName.StartsWith("__", StringComparison.Ordinal) && XEditorSettings.HideAdvancedMembers)
                return true;
            if (realTypeName.IndexOf('$') >= 0)
                return true;
            if (realTypeName.StartsWith("<") )
                return true;
            return false;
        }


        internal static bool IsHiddenMemberName(string realMemberName)
        {
            if (realMemberName.Length > 2 && XEditorSettings.HideAdvancedMembers
                && (realMemberName.StartsWith("__", StringComparison.Ordinal) || realMemberName.EndsWith("__", StringComparison.Ordinal)))
                return true;
            // suppress SELF properties
            if (string.Compare(realMemberName, "self", StringComparison.Ordinal) == 0)
                return true;
            if (realMemberName.IndexOf('$') >= 0)
                return true;
            if (realMemberName.IndexOf('<') >= 0)
                return true;

            if (realMemberName == XLiterals.DestructorName)
                return true;
            if (realMemberName.Length > 4)
            {
                // event add
                if (realMemberName.StartsWith("add_", StringComparison.Ordinal))
                    return true;
                // property get
                if (realMemberName.StartsWith("get_", StringComparison.Ordinal))
                    return true;
                // property set
                if (realMemberName.StartsWith("set_", StringComparison.Ordinal))
                    return true;
            }
            if (realMemberName.Length > 3)
            {
                // operator
                if (realMemberName.StartsWith("op_", StringComparison.Ordinal))
                    return true;
            }
            // event remove
            if (realMemberName.Length > 7 && realMemberName.StartsWith("remove_", StringComparison.Ordinal))
            {
                return true;
            }
            return false;
        }

        internal void AddXSharpKeywords(XCompletionList compList, string startWith)
        {
            foreach (var kw in XSharpSyntax.GetKeywords().Where(ti => nameStartsWith(ti.Name, startWith)))
            {
                ImageSource icon = _glyphService.GetGlyph(kw.getGlyphGroup(), kw.getGlyphItem());
                compList.Add(new XSCompletion(kw.Name, kw.Name, kw.Prototype, icon, null, Kind.Keyword, ""));
            }
        }

        internal void AddXSharpTypeNames(XCompletionList compList, XSharpSearchLocation location, string startWith, IList<string> usings = null, string NameToExclude = "")
        {
            if (usings == null)
                usings = location.Usings;
            var list = location.Project.GetTypes(startWith, usings);
            foreach (var typeInfo in list)
            {
                 if (String.Compare(typeInfo.FullName, NameToExclude) == 0)
                    continue;

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
            var xsharpTypes = XSharpSyntax.GetTypes();
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

        internal void AddGenericCompletion(XCompletionList compList, XSharpSearchLocation location, string startWith)
        {
            if (XEditorSettings.CompleteLocals && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericLocals(compList, location, startWith);
            }
            if (XEditorSettings.CompleteSelf && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericSelfMembers(compList, location, startWith);
            }
            if (XEditorSettings.CompleteParent && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericInheritedMembers(compList, location, startWith);
            }
            if (XEditorSettings.CompleteNamespaces && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddNamespaces(compList, location, startWith);
            }
            if (XEditorSettings.CompleteTypes && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddTypeNames(compList, location, startWith);
            }
            if (XEditorSettings.CompleteFunctions && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericFunctions(compList, location, startWith, true);
            }
            if (XEditorSettings.CompleteFunctionsP && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericFunctions(compList, location, startWith, false);
            }
            if (XEditorSettings.CompleteFunctionsA && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericFunctionsAssemblies(compList, location, startWith, false);
            }
            if (XEditorSettings.CompleteGlobals && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericGlobals(compList, location, startWith, true);
            }
            if (XEditorSettings.CompleteGlobalsP && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericGlobals(compList, location, startWith, false);
            }
            if (XEditorSettings.CompleteGlobalsA && compList.Count < XEditorSettings.MaxCompletionEntries)
            {
                AddGenericGlobalsAssemblies(compList, location, startWith, false);

            }
            if (XEditorSettings.CompleteSnippets)
            {
                // todo: Add Snippets
            }
            if (XEditorSettings.CompleteKeywords)
            {
                AddXSharpKeywords(compList, startWith);
            }
        }

        internal void AddGenericGlobals(XCompletionList compList, XSharpSearchLocation location, string startWith, bool onlyProject)
        {
            var found = location.Project.FindGlobalMembersLike(startWith, onlyProject);
            FillMembers(location, compList, null, found, Modifiers.Public, false);
        }
        internal void AddGenericGlobalsAssemblies(XCompletionList compList, XSharpSearchLocation location, string startWith, bool onlyProject)
        {
            var found = location.Project.FindGlobalsInAssemblyReferences(startWith);
            FillMembers(location, compList, null, found, Modifiers.Public, false);
        }

        internal void AddGenericFunctions(XCompletionList compList, XSharpSearchLocation location, string startWith, bool onlyProject)
        {
            var found = location.Project.FindFunctionsLike(startWith, onlyProject);
            FillMembers(location, compList, null, found, Modifiers.Public, false);

        }
        internal void AddGenericFunctionsAssemblies (XCompletionList compList, XSharpSearchLocation location, string startWith, bool onlyProject)
        {
            var found = location.Project.FindFunctionsInAssemblyReferences(startWith);
            FillMembers(location, compList, null, found, Modifiers.Public, false);
        }

        internal void AddNamespaces(XCompletionList compList, XSharpSearchLocation location, string startWith)
        {
            // We are looking for NameSpaces, in References
            if (startWith == null)
                return;
            var namespaces = location.Project.AllNamespaces;
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            ImageSource icon = _glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupNamespace, StandardGlyphItem.GlyphItemPublic);
            ImageSource iconClass = _glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupClass, StandardGlyphItem.GlyphItemPublic);
            foreach (string nameSpace in namespaces.Where(ns => nameStartsWith(ns, startWith)))
            {
                string displayName = nameSpace;
                // remove the start
                if (startLen > 0)
                    displayName = displayName.Substring(startLen);
                // Do we have another part
                dotPos = displayName.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    displayName = displayName.Substring(0, dotPos);
                //
                XSCompletion item;
                if (! compList.ContainsKey(displayName))
                {
                    if (displayName.IndexOf("<") > 0)
                    {
                        item = new XSCompletion(displayName, displayName, nameSpace, iconClass, null, Kind.Class, "");
                    }
                    else
                    {
                        item = new XSCompletion(displayName, displayName, "Namespace " + nameSpace, icon, null, Kind.Namespace, "");
                    }
                    if (!compList.Add(item))
                        break;
                }
            }
            //
            // And our own Namespaces
            AddXSharpNamespaces(compList, location, startWith, icon);
            // We should also add the external NameSpaces
            var prjs = location.Project.ReferencedProjects;
            foreach (var prj in prjs)
            {
                AddXSharpNamespaces(compList, location, startWith, icon);
            }
        }

        internal void AddXSharpNamespaces(XCompletionList compList, XSharpSearchLocation location, string startWith, ImageSource icon)
        {
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            // And our own Namespaces
            var xsNamespaces = location.Project.ProjectNamespaces;
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
        internal void AddGenericLocals(XCompletionList compList, XSharpSearchLocation location, string startWith)
        {
            if (location.Member == null)
            {
                return;
            }
            // We assume the calling code checks to see if this is allowed
            // First, look after Parameters
            foreach (var paramVar in location.Member.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                ImageSource icon = _glyphService.GetGlyph(paramVar.getGlyphGroup(), paramVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Prototype, icon, null, Kind.Parameter, "")))
                    break;
            }
            // Then, look for Locals
            // line numbers in the range are 1 based. currentLine = 0 based !
            foreach (var localVar in location.Member.GetLocals(location).Where(l => nameStartsWith(l.Name, startWith) && l.Range.StartLine <= location.LineNumber))
            {
                ImageSource icon = _glyphService.GetGlyph(localVar.getGlyphGroup(), localVar.getGlyphItem());
                if (!compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Prototype, icon, null, Kind.Local, "")))
                    break;

            }
        }

        internal void AddGenericSelfMembers(XCompletionList compList, XSharpSearchLocation location, string startWith )
        {
            if (location.Member == null)
            {
                return;
            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //
            // We assume the calling code checks to see if this is allowed

            if (location.Member.Kind.IsClassMember(_dialect))
            {
                var type = location.Member.Parent as XSourceTypeSymbol;
                if (type == null)
                    return;
                foreach (var member in type.GetMembers(startWith))
                {
                    ImageSource icon = _glyphService.GetGlyph(member.getGlyphGroup(), member.getGlyphItem());
                    if (!compList.Add(new XSCompletion(member.Name, member.Name, member.Prototype, icon, null, Kind.Field, "")))
                        break;
                }
            }
        }


        internal void AddGenericInheritedMembers(XCompletionList compList, XSharpSearchLocation location, string startWith)
        {
            if (location.Member == null)
            {
                return;
            }
            if (location.Member.Kind.IsClassMember(_dialect))
            {
                var type = location.Member.Parent as XSourceTypeSymbol;
                if (type == null)
                    return;
                type.ForceComplete();
                var baseType = type.BaseType;
                if (baseType == null)
                    return;
                foreach (var member in baseType.GetMembers(startWith))
                {
                    ImageSource icon = _glyphService.GetGlyph(member.getGlyphGroup(), member.getGlyphItem());
                    if (!compList.Add(new XSCompletion(member.Name, member.Name, member.Prototype, icon, null, Kind.Field, "")))
                        break;
                }
            }
        }

        internal void BuildCompletionListMembers(XSharpSearchLocation location, XCompletionList compList, IXTypeSymbol type, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (type == null)
            {
                return;
            }
            //
            FillMembers(location, compList, type, minVisibility, staticOnly, startWith);
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
               if (parentType != null && parentType.FullName == sourceType.FullName)
                {
                    ; // recursion !
                    WriteOutputMessage("*** Recursion detected *** " + sourceType.FullName + " inherits from " + parentType.FullName);
                }
                else if (baseType == "System.Enum" && staticOnly)
                {
                    ; // do nothing
                }
                else
                {
                    var nextVis = Modifiers.Protected;
                    if (minVisibility == Modifiers.Internal || minVisibility == Modifiers.Public)
                        nextVis = minVisibility;
                    BuildCompletionListMembers(location, compList, parentType, nextVis, staticOnly, startWith);
                }
                foreach (var ifname in sourceType.Interfaces)
                {
                    var iftype = sourceType.File.FindType(ifname, sourceType.Namespace);
                    if (iftype != null)
                    {
                        BuildCompletionListMembers(location, compList, iftype, Modifiers.Public, staticOnly, startWith);
                    }
                }
            }
            if (type is XPETypeSymbol && type.Children.Count > 0)
            {
                // Add nested types. They start with the typename +"."
                AddTypeNames(compList, location, type.FullName+".", false);
            }
            if (type is XSourceTypeSymbol)
            {
                var usings = location.Usings.ToList();
                usings.Add(type.FullName);
                AddXSharpTypeNames(compList, location, type.FullName, usings, type.FullName);
            }
            if (!staticOnly)
            {
                FillExtensions(location, compList, type, startWith);
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
        private string AddOpenParen(Kind kind)
        {
            if (!XEditorSettings.DisableAutoOpen)
            {
                switch (kind)
                {
                    case Kind.Constructor:
                    case Kind.Event:
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        return "";
                    default:
                        if (kind.HasParameters())
                        {
                            return "(";
                        }
                        break;
                }
            }
            return "";
        }

        internal void FillEnumMembers(XSharpSearchLocation location, XCompletionList compList, IXTypeSymbol type, IEnumerable<IXMemberSymbol> members, Modifiers minVisibility, bool staticOnly)
        {
            WriteOutputMessage($"FillEnumMembers {type?.FullName}: {members.Count()} members");
            foreach (var elt in members)
            {
                if (elt.IsStatic != staticOnly)
                    continue;
                if (elt.Parent != type && staticOnly)
                    continue;
                if (elt is XPESymbol peSym && peSym.IsSpecialName)
                    continue;
                ImageSource icon = _glyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name, elt.Prototype, icon, null, elt.Kind, elt.Value)))
                    break;
            }
        }

        /// <summary>
        /// Add members to the completionlist
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="members"></param>
        /// <param name="minVisibility"></param>
        /// <param name="staticOnly"></param>
        internal void FillMembers(XSharpSearchLocation location, XCompletionList compList, IXTypeSymbol type, IEnumerable<IXMemberSymbol> members, Modifiers minVisibility, bool staticOnly)
        {
            if (members.Count() == 0)
                return;
            bool isEnum = type != null && type.Kind == Kind.Enum;
            if (isEnum)
            {
                FillEnumMembers(location, compList, type, members, minVisibility, staticOnly);
                return;
            }
            WriteOutputMessage($"FillMembers {type?.FullName}: {members.Count()} members");
            foreach (var elt in members)
            {
                bool add = true;
                if (IsHiddenMemberName(elt.Name))
                {
                    continue;
                }
                if (elt is XPESymbol peSym && peSym.IsSpecialName)
                    continue;
                switch (elt.Kind)
                {
                    case Kind.EnumMember:
                        add = true;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Operator:
                        add = false;
                        break;
                    default:
                        if (!elt.Kind.IsGlobalTypeMember() && elt.IsStatic != staticOnly)
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
                if (!add)
                    continue;
                //
                ImageSource icon = _glyphService.GetGlyph(elt.getGlyphGroup(), elt.getGlyphItem());
                string toAdd = AddOpenParen(elt.Kind);
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
        internal void FillMembers(XSharpSearchLocation location, XCompletionList compList, IXTypeSymbol xType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            FillMembers(location, compList, xType, xType.GetMembers(startWith), minVisibility, staticOnly);
        }


        internal void FillExtensions(XSharpSearchLocation location, XCompletionList compList, IXTypeSymbol type, string startWith)
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
                {
                    FillMembers(location, compList, null, selection, Modifiers.Public, true);
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
        internal string TokenListAsString(IList<IToken> tokenList)
        {
            string retValue = "";
            for (int pos = 0; pos < tokenList.Count; pos++)
            {
                var t = tokenList[pos];
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
            string memberSource;
            if (start < location.Snapshot.Length && end > 0 && end <= location.Snapshot.Length)
                memberSource = location.Snapshot.GetText(start, end);
            else
                memberSource = location.Snapshot.GetText();

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
                if (member.ParentType != null && !String.IsNullOrEmpty(member.ParentType.BaseTypeName))
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

