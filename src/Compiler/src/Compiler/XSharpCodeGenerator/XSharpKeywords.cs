//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.CodeDom.Compiler;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace XSharp.CodeDom
{
    // That information is build from the file XSharpParser.cs
    // stored in XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Generated
    // The goal is to automate the generation of the FixedStringLookup array
    // We don't want to add a link here to the codeanalysis.dll
    public class XSharpKeywords
    {
        // Pasted from XSharpParser, and reworked a bit
        // look at the _SymbolicNames in the XsharpParser.cs
        // only the tokens in the form of FIRST_* and LAST_* have been deleted
        // and the names for operator, constants etc.

        private static HashSet<string> _keywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        public static bool Contains(string value)
        {
            return  _keywords.Contains(value.ToUpper());
        }

        static XSharpKeywords()
        {
            var kwds = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DefaultVocabulary;

            for (int i = 1; i < kwds.MaxTokenType; i++)
            {
                if (IsKeyword(i) )
                {
                    string name = kwds.GetSymbolicName(i);
                    if (!name.StartsWith("FIRST_") && !name.StartsWith("LAST_"))
                    {
                        _keywords.Add(name);
                    }
                }

            }
        }
        static bool IsKeyword (int iToken)
        {
            return iToken > XSharpLexer.FIRST_KEYWORD && iToken < XSharpLexer.LAST_KEYWORD;
        }
    }
    
}
