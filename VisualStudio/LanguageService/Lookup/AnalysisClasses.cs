//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Adornments;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Remoting.Contexts;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    internal class XAnalysis
    {
        public string Name { get; protected set; }
        public IXSymbol symbol;
        internal XAnalysis(IXSymbol sym)
        {
            if (sym == null)
                return;
            symbol = sym;
            Name = sym.FullName;
        }
        internal ImageMoniker Image
        {
            get
            {
                switch (symbol.Kind)
                {
                    case Kind.Class:
                        return KnownMonikers.Class;
                    case Kind.Structure:
                        return KnownMonikers.Structure;
                    case Kind.Interface:
                        return KnownMonikers.Interface;
                    case Kind.Delegate:
                        return KnownMonikers.Delegate;
                    case Kind.Enum:
                        return KnownMonikers.Enumeration;
                    case Kind.VOStruct:
                        return KnownMonikers.Type;
                    case Kind.Union:
                        return KnownMonikers.Union;
                    case Kind.Namespace:
                        return KnownMonikers.Namespace;
                    case Kind.Keyword:
                        return KnownMonikers.IntellisenseKeyword;
                }
                return KnownMonikers.None;
            }
        }
        public string Prototype
        {
            get
            {
                return Name;
                //
            }
        }
        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG;
                //
                switch (symbol.Kind)
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
                switch (symbol.Visibility)
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
        public virtual ClassifiedTextRun[] WPFDescription
        {
            get
            {
                var content = new List<ClassifiedTextRun>();

                content.addKeyword(XLiterals.FormatKeyword(symbol.KindKeyword) + " ");
                //
                //
                content.addText(Prototype);

                //
                return content.ToArray();
            }

        }
    }
    internal class XTypeAnalysis : XAnalysis
    {
        public bool IsStatic => Type.IsStatic;
        public IXTypeSymbol Type { get; private set; }

        internal XTypeAnalysis(IXTypeSymbol typeInfo) : base(typeInfo)
        {
            //
            if (typeInfo == null)
                return;
            Type = typeInfo;
            Name = typeInfo.FullName;
        }

        public override ClassifiedTextRun[] WPFDescription
        {
            get
            {
                var content = new List<ClassifiedTextRun>();

                if (Type.Modifiers != Modifiers.None)
                {
                    content.addKeyword(XLiterals.FormatKeyword(Type.ModifiersKeyword));
                    content.addWs();
                }
                content.addKeyword(XLiterals.FormatKeyword(Type.VisibilityKeyword));
                content.addWs();
                //
                if (Type.Kind != Kind.Field)
                {
                    content.addKeyword(XLiterals.FormatKeyword(Type.KindKeyword));
                    content.addWs();
                }
                //
                if (Type.IsGeneric)
                {
                    // convert syntax with `2 to real type parameter names
                    string genName = Type.FullName;
                    if (Type is XPETypeSymbol petype)
                    {
                        genName = petype.OriginalTypeName;
                    }
                    int index = genName.IndexOfAny(new char[] { '`','<'});
                    if (index != -1)
                    {
                        genName = genName.Substring(0, index);
                    }
                    content.addText(genName);
                    bool first = true;
                    foreach (var str in Type.TypeParameters)
                    {
                        if (first)
                            content.addOperator("<");
                        else
                            content.addOperator(",");
                        content.addText(str);
                        first = false;
                    }
                    content.addOperator(">");
                }
                else
                {
                    content.addText(Prototype);
                }
                if (!string.IsNullOrWhiteSpace(Type.BaseTypeName))
                {
                    content.addText("\r\n");
                    content.addKeyword(XLiterals.FormatKeyword("INHERIT"));
                    content.addWs();
                    content.addText(Type.BaseTypeName);
                }
                if (Type.Interfaces?.Count > 0)
                {
                    content.addText("\r\n");
                    content.addKeyword(XLiterals.FormatKeyword("IMPLEMENTS"));
                    content.addWs();
                    bool first = true;
                    int count = 0;
                    foreach (var name in Type.Interfaces)
                    {
                        if (first)
                            first = false;
                        else
                        {
                            content.addOperator(", ");
                        }
                        content.addText(name);
                        if (++count > 4 && Type.Interfaces.Count > 5)
                        {
                            var rest = Type.Interfaces.Count - 5;
                            content.addText($", ... ( {rest} more )");
                            break;
                        }
                    }
                }
                if (Type.Kind == Kind.Delegate)
                {
                    var mem = Type.Members.First();
                    var memana = new XMemberAnalysis(mem);
                    var desc = memana.WPFPrototype;
                    var name = mem.Name;
                    // skip first element: Invoke
                    for (int i = 1; i < desc.Length; i++)
                    {
                        var element = desc[i];
                        content.Add(element);
                    }

                }
                //
                string returns;
                string remarks;
                var xmldesc = XSharpXMLDocMember.GetTypeSummary(Type, null, out returns, out remarks);
                content.addSummary(xmldesc);
                content.addReturns(returns);
                content.addRemarks(remarks);
                content.addLocation(Type.Location);
                return content.ToArray();
            }

        }

    }
    internal class XMemberAnalysis
    {
        public string Name { get; private set; }
        public string TypeName { get; private set; }
        public string Value { get; private set; }
        public IXMemberSymbol Member;



        internal XMemberAnalysis(IXMemberSymbol member)
        {
            TypeName = "";
            if (member == null)
                return;
            Member = member;
            Name = member.Name;
            Value = member.Value;
            if (member.Kind.HasReturnType())
            {
                TypeName = member.TypeName;
            }
        }

        public ClassifiedTextRun[] WPFDescription
        {
            get
            {
                var content = new List<ClassifiedTextRun>();

                if (Member.Modifiers != Modifiers.None)
                {
                    content.addKeyword(XLiterals.FormatKeyword(Member.ModifiersKeyword) + " ");
                }
                content.addKeyword(XLiterals.FormatKeyword(Member.VisibilityKeyword) + " ");
                //
                if ((Member.IsStatic) && ((Member.Kind != Kind.Function) && (Member.Kind != Kind.Procedure)))
                {
                    content.addKeyword(XLiterals.FormatKeyword("STATIC "));
                }
                //
                if ((Member.Kind != Kind.Field) && (Member.Kind != Kind.Constructor))
                {
                    content.addKeyword(XLiterals.FormatKeyword(Member.KindKeyword) + " ");
                }
                //
                content.AddRange(WPFPrototype);
                //
                return content.ToArray();
            }

        }

        public ClassifiedTextRun[] WPFPrototype
        {
            get
            {
                var content = new List<ClassifiedTextRun>();
                if (Member.Kind.HasParameters() /*&& Member.Kind.IsProperty()*/)
                {
                    content.addText(Name);
                    content.addOperator(Member.Kind == Kind.Constructor ? "{" : "(");
                    bool first = true;
                    foreach (IXParameterSymbol var in Member.Parameters)
                    {
                        if (!first)
                        {
                            content.addOperator(", ");
                        }
                        first = false;
                        content.addText(var.Name + " ");
                        content.addKeyword(var.ParamTypeDesc + " ");
                        content.addKeyword(var.TypeName);
                    }
                    content.addOperator(Member.Kind == Kind.Constructor ? "}" : ")");
                }
                //
                if (!string.IsNullOrEmpty(Value))
                {
                    content.addOperator(" := ");
                    content.addText(Value);
                }
                if (Member.Kind.HasReturnType())
                {
                    content.addReturnType(TypeName);
                }
                return content.ToArray();
            }
        }
    }

}
