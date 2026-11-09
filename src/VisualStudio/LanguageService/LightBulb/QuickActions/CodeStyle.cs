//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>How generated code is written: dialect, keyword case, indent unit and line break.</summary>
    internal sealed class CodeStyle
    {
        private readonly Func<string, string> _keyword;

        internal CodeStyle(XDialect dialect, Func<string, string> keyword, string indentUnit, string newLine)
        {
            Dialect = dialect;
            _keyword = keyword ?? (s => s);
            IndentUnit = indentUnit ?? "    ";
            NewLine = newLine ?? "\r\n";
        }

        internal XDialect Dialect { get; }
        internal string IndentUnit { get; }
        internal string NewLine { get; }

        /// <summary>The dialect has the USUAL type (every dialect but Core).</summary>
        internal bool HasUsual => Dialect != XDialect.Core;

        /// <summary>The type written for a parameter whose type is unknown.</summary>
        internal string UntypedType => Kw(HasUsual ? "USUAL" : "OBJECT");

        /// <summary>A keyword (or several, separated by blanks) in the keyword case of the editor.</summary>
        internal string Kw(string keyword) => _keyword(keyword);
    }

    /// <summary>Text to insert at the start of a 0-based line.</summary>
    internal struct TextInsertion
    {
        internal TextInsertion(int line, string text, int caret = -1)
        {
            Line = line;
            Text = text;
            Caret = caret;
        }

        internal int Line { get; }
        internal string Text { get; }

        /// <summary>Where the caret goes after the insertion, as an offset in <see cref="Text"/>; -1 for no move.</summary>
        internal int Caret { get; }
    }
}
