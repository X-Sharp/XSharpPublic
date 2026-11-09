//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    internal enum InlayHintKind
    {
        Parameter,
        Type
    }

    /// <summary>One hint: grey text at a buffer position that is not part of the file.</summary>
    internal sealed class InlayHint
    {
        internal InlayHintKind Kind { get; set; }
        /// <summary>Buffer offset: the start of the argument, or the end of the variable name.</summary>
        internal int Position { get; set; }
        /// <summary>The text shown: <c>name:</c> or <c> AS Type</c>.</summary>
        internal string Text { get; set; }
        internal IXParameterSymbol Parameter { get; set; }
        internal IXMemberSymbol Member { get; set; }
        internal IXTypeSymbol Type { get; set; }
        /// <summary>The X# name of the type as written into the code by a double-click.</summary>
        internal string TypeName { get; set; }
        /// <summary>The double-click edit of a type hint, null when there is none.</summary>
        internal TypeEdit Edit { get; set; }
    }

    /// <summary>Text changes on buffer offsets of the snapshot the hint was computed for; applied as one undo step.</summary>
    internal sealed class TypeEdit
    {
        internal IReadOnlyList<(int Start, int Length, string Text)> Changes { get; set; }
    }
}
