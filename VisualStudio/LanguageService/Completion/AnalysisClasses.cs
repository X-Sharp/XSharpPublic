//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Windows.Documents;
using System.Windows.Media;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal class XTypeAnalysis
    {
        public string Name { get; private set; }
        public bool IsStatic => this.Type.IsStatic;
        public IXTypeSymbol Type { get; private set; }

        void setType(IXTypeSymbol typeInfo)
        {
            //
            if (typeInfo == null)
                return;
            this.Type = typeInfo;
            this.Name = typeInfo.FullName;
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

        internal XTypeAnalysis(IXTypeSymbol type)
        {
            setType(type);
        }

        internal XTypeAnalysis(IXTypeSymbol type, Brush fg, Brush txt)
        {
            QuickInfoHelpers.kwBrush = fg;
            QuickInfoHelpers.txtBrush = txt;
            setType(type);
        }
        public List<Inline> WPFDescription
        {
            get
            {
                List<Inline> content = new List<Inline>();

                if (this.Type.Modifiers != XSharpModel.Modifiers.None)
                {
                    content.addKeyword(XSettings.FormatKeyword(this.Type.ModifiersKeyword) + " ");
                }
                content.addKeyword(XSettings.FormatKeyword(this.Type.VisibilityKeyword) + " ");
                //
                if (this.IsStatic)
                {
                    content.addKeyword(XSettings.FormatKeyword("STATIC "));
                }
                //
                if (this.Type.Kind != XSharpModel.Kind.Field)
                {
                    content.addKeyword(XSettings.FormatKeyword(this.Type.KindKeyword) + " ");
                }
                //
                content.addText(this.Prototype);

                //
                string returns;
                string remarks;
                var xmldesc = XSharpXMLDocMember.GetTypeSummary(this.Type, null, out returns, out remarks);
                content.addSummary(xmldesc);
                content.addReturns(returns);
                content.addRemarks(remarks);
                content.addLocation(Type.Location);
                return content;
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
                switch (this.Type.Kind)
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
                switch (this.Type.Visibility)
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
    internal class XMemberAnalysis
    {
        public string Name { get; private set; }
        public string TypeName { get; private set; }
        public string Value { get; private set; }
        public IXMemberSymbol Member;

        internal XMemberAnalysis(IXMemberSymbol member, Brush kw, Brush txt)
        {
            this.Member = member;
            this.Name = member.Name;
            this.TypeName = "";
            this.Value = member.Value;

            QuickInfoHelpers.kwBrush = kw;
            QuickInfoHelpers.txtBrush = txt;
        }

        public List<Inline> WPFDescription
        {
            get
            {
                List<Inline> content = new List<Inline>();

                if (this.Member.Modifiers != XSharpModel.Modifiers.None)
                {
                    content.addKeyword(XSettings.FormatKeyword(this.Member.ModifiersKeyword) + " ");
                }
                content.addKeyword(XSettings.FormatKeyword(this.Member.VisibilityKeyword) + " ");
                //
                if ((this.Member.IsStatic) && ((this.Member.Kind != Kind.Function) && (this.Member.Kind != Kind.Procedure)))
                {
                    content.addKeyword(XSettings.FormatKeyword("STATIC "));
                }
                //
                if ((this.Member.Kind != XSharpModel.Kind.Field) && (this.Member.Kind != XSharpModel.Kind.Constructor))
                {
                    content.addKeyword(XSettings.FormatKeyword(this.Member.KindKeyword) + " ");
                }
                //
                content.AddRange(this.WPFPrototype);
                //
                return content;
            }

        }

        public List<Inline> WPFPrototype
        {
            get
            {
                List<Inline> content = new List<Inline>();
                if (this.Member.Kind.HasParameters())
                {
                    content.addText(this.Name);
                    content.addKeyword(this.Member.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                    bool first = true;
                    foreach (var var in this.Member.Parameters)
                    {
                        if (!first)
                        {
                            content.addText(", ");
                        }
                        first = false;
                        content.addText(var.Name + " ");
                        content.addKeyword(var.ParamTypeDesc + " ");
                        content.addKeyword(var.TypeName);
                    }
                    content.addKeyword(this.Member.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                }
                //
                if (!String.IsNullOrEmpty(this.Value))
                {
                    content.addText(" := " + this.Value);
                }
                if (this.Member.Kind.HasReturnType())
                {
                    content.addReturnType(this.TypeName);
                }
                return content;
            }
        }
    }

}
