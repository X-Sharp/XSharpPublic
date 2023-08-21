//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Windows.Media;
using XSharpModel;
using static Microsoft.VisualStudio.Shell.ThreadedWaitDialogHelper;

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
        public bool Add(XSCompletion item, bool overwrite = false)
        {
            if (item.KeyText.StartsWith("("))
                return false;

            if (overwrite && ContainsKey(item.KeyText))
            {
                Remove(item.KeyText);
            }
            if (ContainsKey(item.KeyText))
            {
                // only methods have overloads
                // we do not want to the overloads message for partial classes that appear in more than 1 file
                // and if a method in a subclass has the same params we also do not want to show that there are overloads
                var found = this[item.KeyText];
                if (!string.Equals(found.Description, item.Description, StringComparison.OrdinalIgnoreCase))
                {
                    // Already exists in the List !!
                    // First Overload ?
                    XSCompletion comp = this[item.KeyText];
                    var props = comp.GetCompletionProperties();
                    props.Overloads += 1;
                }
                // Ok, Forget about the newly added Completion please
                return true;

            }
            if (!string.IsNullOrEmpty(item.KeyText))
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
                base.Add(item.KeyText, item);
            }
            return true;
        }
    }

    internal class CompletionProperties
    {
        internal IXTypeSymbol Type { get; set; } = null;
        internal Char Char { get; set; } = '\0';
        internal bool IncludeKeywords { get; set; } = false;
        internal uint Command { get; set; } = 0;
        internal bool AutoType { get; set; } = false;
        internal string Filter { get; set; } = "";
        internal int Overloads { get; set; } = 0;
        internal int NumCharsToDelete { get; set; } = 0;
        internal SnapshotPoint Position { get; set; } = default;
        internal ICompletionSession Session { get; set; } = null;

    }

    internal static class CompletionPropertiesExt
    {
        internal static CompletionProperties GetCompletionProperties(this ICompletionSession session)
        {
            session.Properties.TryGetProperty(typeof(CompletionProperties), out CompletionProperties props);
            if (props == null)
            {
                props = new CompletionProperties();
                props.Session = session;
                session.Properties.AddProperty(typeof(CompletionProperties), props);
            }
            return props;
        }
        internal static CompletionProperties GetCompletionProperties(this XSCompletion comp)
        {
            comp.Properties.TryGetProperty(typeof(CompletionProperties), out CompletionProperties props);
            if (props == null)
            {
                props = new CompletionProperties();
                comp.Properties.AddProperty(typeof(CompletionProperties), props);
            }
            return props;
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

    public virtual string KeyText
    {
        get
        {
            var text = this.DisplayText;
            var pos = text.IndexOf("<");
            if (pos > 0)
            {
                text = text.Substring(0, pos - 1);
            }
            return text;
        }
    }

    public override string Description
    {
        get
        {
            string desc;
            var props = this.GetCompletionProperties();
            if (props.Overloads > 0)
            {
                desc = base.Description;
                desc += " (+" + props.Overloads + " overload";
                if (props.Overloads > 1)
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

// Build a list of all Keywords
internal static class XSharpSyntax
{
    static IList<IXSymbol> _keywords;
    static IList<IXSymbol> _types;
    static IDictionary<string, string> _keywordNames;
    static XSharpSyntax()
    {
        // Dummy call to a Lexer; just to copy the Keywords, Types, ...
        // Pass default options so this will be the core dialect and no
        // 4 letter abbreviations will be in the list
        var lexer = XSharpLexer.Create("", "", XSharpParseOptions.Default);
        //
        _keywordNames = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        var keywords = new List<IXSymbol>();
        var types = new List<IXSymbol>();
        //
        foreach (var keyword in lexer.KwIds)
        {
            _keywordNames.Add(keyword.Key, keyword.Key);
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

    internal static IDictionary<string, string> KeywordNames => _keywordNames;
    internal static IList<IXSymbol> GetKeywords()
    {
        return _keywords;
    }
    internal static IList<IXSymbol> GetTypes()
    {
        return _types;
    }
}


}
