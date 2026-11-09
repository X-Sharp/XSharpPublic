//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharp.LanguageService.ForeignSource
{
    /// <summary>What to look for in a C# or VB compilation: a type, or a member of that type.</summary>
    internal sealed class ForeignSymbolKey
    {
        internal ForeignSymbolKey(string typeDocId, string memberDocId, string memberName, int parameterCount)
        {
            TypeDocId = typeDocId;
            MemberDocId = memberDocId;
            MemberName = memberName;
            ParameterCount = parameterCount;
        }

        /// <summary>The XML doc ID of the type (<c>T:Ns.Type</c>).</summary>
        internal string TypeDocId { get; }
        /// <summary>The XML doc ID of the member, or null when the type itself is wanted.</summary>
        internal string MemberDocId { get; }
        /// <summary>The member name as X# spells it (<c>.ctor</c> for a constructor); used when the doc ID does not resolve.</summary>
        internal string MemberName { get; }
        internal int ParameterCount { get; }
    }
}
