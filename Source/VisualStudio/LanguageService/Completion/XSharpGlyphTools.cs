//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Language.Intellisense;
using XSharpModel;

namespace XSharp.LanguageService
{


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
                case Kind.VODLL:
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
                case Kind.Define:
                case Kind.Undefine:
                    imgG = StandardGlyphGroup.GlyphGroupConstant;
                    break;
                case Kind.VOStruct:
                    imgG = StandardGlyphGroup.GlyphGroupValueType;
                    break;
                case Kind.Keyword:
                    imgG = StandardGlyphGroup.GlyphKeyword;
                    break;
                case Kind.Using:
                    imgG = StandardGlyphGroup.GlyphReference;
                    break;
                case Kind.Attribute:
                    imgG = StandardGlyphGroup.GlyphGroupIntrinsic;

                    break;
                case Kind.Command:
                case Kind.XCommand:
                case Kind.Translate:
                case Kind.XTranslate:
                    imgG = StandardGlyphGroup.GlyphGroupMacro;
                    break;
                case Kind.TypeParameter:
                    imgG = StandardGlyphGroup.GlyphGroupType;
                    break;
                case Kind.Unknown:
                case Kind.Ignore:
                case Kind.Undeclared:
                    imgG = StandardGlyphGroup.GlyphGroupUnknown;
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


}
