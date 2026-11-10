//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Where "Generate overrides..." is offered: like C#, on the class header or on a blank line between members.</summary>
    internal static class OverrideTrigger
    {
        /// <summary>
        /// True on <paramref name="classHeaderLine"/>, or on a blank line whose next code line (comments skipped) is in
        /// <paramref name="declarationLines"/> – the lines where a member of the class or END CLASS starts – or that
        /// has no code after it. X# members have no END METHOD, so "between members" is told by what follows.
        /// </summary>
        internal static bool Applies(Func<int, string> lineText, int lineCount, int caretLine, int classHeaderLine, ISet<int> declarationLines)
        {
            if (caretLine == classHeaderLine)
                return true;
            if (caretLine < 0 || caretLine >= lineCount || lineText(caretLine).Trim().Length > 0)
                return false;
            for (int line = caretLine + 1; line < lineCount; line++)
            {
                var text = lineText(line).Trim();
                if (text.Length == 0 || IsComment(text))
                    continue;
                return declarationLines.Contains(line);
            }
            return true;
        }

        private static bool IsComment(string text)
        {
            return text.StartsWith("//") || text.StartsWith("*") || text.StartsWith("&&") || text.StartsWith("/*");
        }
    }
}
