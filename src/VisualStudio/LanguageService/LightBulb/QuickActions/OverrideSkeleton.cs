//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>The text of a member that overrides a base member and calls it with SUPER.</summary>
    internal static class OverrideSkeleton
    {
        /// <summary>
        /// The member text, starting with a blank line and ending with a line break. <paramref name="writeOverride"/>
        /// prefixes OVERRIDE (the caller decides from the base member and the project options). With
        /// <paramref name="className"/> the header gets a CLASS clause (VO-style classes without END CLASS); properties,
        /// which VO-style classes do not declare outside the class block, do not.
        /// </summary>
        internal static string Build(IXMemberSymbol member, string memberIndent, bool writeOverride, CodeStyle style, string className = null)
        {
            var clause = string.IsNullOrEmpty(className) ? "" : " " + style.Kw("CLASS") + " " + className;
            memberIndent = memberIndent ?? "";
            var body = memberIndent + style.IndentUnit;
            var nl = style.NewLine;
            var prefix = new StringBuilder(memberIndent);
            if (!member.IsPublic && !string.IsNullOrEmpty(member.VisibilityKeyword))
                prefix.Append(style.Kw(member.VisibilityKeyword.ToUpperInvariant())).Append(' ');
            if (writeOverride)
                prefix.Append(style.Kw("OVERRIDE")).Append(' ');
            var name = member.Name;
            // the code model reports USUAL for an untyped member: IsTyped tells whether the base wrote AS ...
            var returnType = member.IsTyped ? member.TypeName ?? "" : "";
            var asType = returnType.Length > 0 ? " " + style.Kw("AS") + " " + returnType : "";
            var parameters = member.Parameters ?? new List<IXParameterSymbol>();
            var isAbstract = (member.Modifiers & Modifiers.Abstract) != 0;
            switch (member.Kind)
            {
                case Kind.Access:
                    return nl + prefix + style.Kw("ACCESS") + " " + name + asType + clause + nl +
                           body + style.Kw("RETURN") + " " + style.Kw("SUPER") + ":" + name + nl;
                case Kind.Assign:
                    var value = parameters.Count > 0 ? parameters[0].Name : "uValue";
                    var valueType = parameters.Count > 0 && parameters[0].IsTyped
                        ? " " + style.Kw("AS") + " " + parameters[0].TypeName : "";
                    return nl + prefix + style.Kw("ASSIGN") + " " + name + "(" + value + valueType + ")" + clause + nl +
                           body + style.Kw("SUPER") + ":" + name + " := " + value + nl;
                case Kind.Property:
                    var text = nl + prefix + style.Kw("PROPERTY") + " " + name + asType;
                    if (HasAccessor(member, AccessorKind.Get))
                        text += " " + style.Kw("GET") + " " + (isAbstract ? Throw(style) : style.Kw("SUPER") + ":" + name);
                    if (HasAccessor(member, AccessorKind.Set))
                        text += " " + style.Kw("SET") + " " + (isAbstract ? Throw(style) : style.Kw("SUPER") + ":" + name + " := " + style.Kw("VALUE"));
                    return text + nl;
            }
            var call = style.Kw("SUPER") + ":" + name;
            if (IsClipper(member))
            {
                var names = string.Join(", ", parameters.Select(p => p.Name));
                return nl + prefix + style.Kw("METHOD") + " " + name + "(" + names + ")" + asType + " " + style.Kw("CLIPPER") + clause + nl +
                       body + style.Kw("RETURN") + " " + call + "(_Args())" + nl;
            }
            var declared = string.Join(", ", parameters.Select(p => Declaration(p, style)));
            var passed = string.Join(", ", parameters.Select(p => Argument(p, style)));
            var header = nl + prefix + style.Kw("METHOD") + " " + name + "(" + declared + ")" + asType + clause + nl;
            // an abstract base member has no body to call
            if (isAbstract)
                return header + body + Throw(style) + nl;
            if (string.Equals(returnType, "VOID", StringComparison.OrdinalIgnoreCase))
                return header + body + call + "(" + passed + ")" + nl + body + style.Kw("RETURN") + nl;
            return header + body + style.Kw("RETURN") + " " + call + "(" + passed + ")" + nl;
        }

        private static string Throw(CodeStyle style) => style.Kw("THROW") + " NotImplementedException{}";

        /// <summary>The accessors a DLL property has; a source property is taken to have both.</summary>
        private static bool HasAccessor(IXMemberSymbol member, AccessorKind kind)
        {
            return !(member is XPEPropertySymbol property) || property.Accessors.HasFlag(kind);
        }

        /// <summary>CLIPPER calling convention, or a source method whose parameters are all untyped.</summary>
        private static bool IsClipper(IXMemberSymbol member)
        {
            if (member.CallingConvention == CallingConvention.Clipper)
                return true;
            var parameters = member.Parameters;
            return parameters != null && parameters.Count > 0 && parameters.All(p => !p.IsTyped);
        }

        private static string Declaration(IXParameterSymbol p, CodeStyle style)
        {
            var type = p.IsTyped && !string.IsNullOrEmpty(p.TypeName) ? p.TypeName : style.UntypedType;
            switch (p.ParamType)
            {
                case ParamType.Ref: return p.Name + " " + style.Kw("REF") + " " + type;
                case ParamType.Out: return p.Name + " " + style.Kw("OUT") + " " + type;
                case ParamType.In: return p.Name + " " + style.Kw("IN") + " " + type;
                case ParamType.Params: return style.Kw("PARAMS") + " " + p.Name + " " + style.Kw("AS") + " " + type;
                default: return p.Name + " " + style.Kw("AS") + " " + type;
            }
        }

        private static string Argument(IXParameterSymbol p, CodeStyle style)
        {
            switch (p.ParamType)
            {
                case ParamType.Ref: return style.Kw("REF") + " " + p.Name;
                case ParamType.Out: return style.Kw("OUT") + " " + p.Name;
                default: return p.Name;
            }
        }
    }
}
