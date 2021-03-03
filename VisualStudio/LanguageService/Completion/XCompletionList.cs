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

 

    /// <summary>
    /// XSharp CompletionList
    /// </summary>
    internal class XCompletionList : SortedDictionary<string, XSCompletion>
    {
        internal bool HasMethods { get; private set; }
        internal bool HasProperties { get; private set; }
        internal bool HasFields { get; private set; }
        internal bool HasEvents { get; private set; }
        internal bool HasEnumMembers { get; private set; }
        internal bool HasTypes { get; private set; }
        internal bool HasNamespaces { get; private set; }
        internal XFile _file;
        internal XCompletionList(XFile startLocation) : base(StringComparer.OrdinalIgnoreCase)
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

        public IXSymbol Result => foundElement;

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
                CompletionType cType = CompletionType.Empty;
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
                        var parts = member.FullName.Split(new char[] { '.' });
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


}
