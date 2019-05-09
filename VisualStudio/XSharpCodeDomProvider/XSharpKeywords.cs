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
        private static readonly string[] _SymbolicNames = {
        "ACCESS", "ALIGN", "AS", "ASPEN", "ASSIGN", "BEGIN",
        "BREAK", "CALLBACK", "CASE", "CAST", "CLASS", "CLIPPER", "DECLARE", "DEFINE",
        "DIM", "DLL", "DLLEXPORT", "DO", "DOWNTO", "ELSE", "ELSEIF", "END", "ENDCASE",
        "ENDDO", "ENDIF", "EXIT", "EXPORT", "FASTCALL", "FIELD", "FIELD_", "FOR",
        "FUNC", "FUNCTION", "GLOBAL", "HIDDEN", "IF", "IIF", "INHERIT", "INIT1",
        "INIT2", "INIT3", "INSTANCE", "IS", "IN", "LOCAL", "LOOP", "MEMBER", "MEMVAR",
        "METHOD", "NAMEOF", "NEXT", "OTHERWISE", "PARAMETERS", "PASCAL", "PRIVATE",
        "PROC", "PROCEDURE", "PROTECTED", "PUBLIC", "RECOVER", "RETURN", "SELF",
        "SEQUENCE", "SIZEOF", "STATIC", "STEP", "STRICT", "SUPER", "THISCALL",
        "TO", "TYPEOF", "UNION", "UPTO", "USING", "WHILE", "WINCALL", "CATCH",
        "FINALLY", "THROW",  "ABSTRACT", "ANSI", "AUTO",
        "CONSTRUCTOR", "CONST", "DEFAULT", "DELEGATE", "DESTRUCTOR", "ENUM", "EVENT",
        "EXPLICIT", "FOREACH", "GET", "IMPLEMENTS", "IMPLICIT", "IMPLIED", "INITONLY",
        "INTERFACE", "INTERNAL", "LOCK", "NAMESPACE", "NEW", "OPERATOR", "OUT",
        "PARTIAL", "PROPERTY", "REPEAT", "SCOPE", "SEALED", "SET", "STRUCTURE",
        "TRY", "UNICODE", "UNTIL", "VALUE", "VIRTUAL", "VOSTRUCT", "ADD", "ARGLIST",
        "ASCENDING", "ASSEMBLY", "ASYNC", "ASTYPE", "AWAIT", "BY", "CHECKED",
        "DESCENDING", "EQUALS", "EXTERN", "FIXED", "FROM", "GROUP", "INTO", "JOIN",
        "LET", "NOP", "MODULE", "OF", "ON", "ORDERBY", "OVERRIDE", "PARAMS", "REMOVE",
        "SELECT", "SWITCH", "UNCHECKED", "UNSAFE", "VAR", "VOLATILE", "WHERE",
        "YIELD",  "ARRAY", "BYTE", "CODEBLOCK",
        "DATE", "DWORD", "FLOAT", "INT", "LOGIC", "LONGINT", "OBJECT", "PSZ",
        "PTR", "REAL4", "REAL8", "REF", "SHORTINT", "STRING", "SYMBOL", "USUAL",
        "VOID", "WORD", "CHAR", "INT64", "UINT64", "DYNAMIC", "DECIMAL", "DATETIME",
         "NIL", "NULL", "NULL_ARRAY", "NULL_CODEBLOCK", "NULL_DATE",
        "NULL_OBJECT", "NULL_PSZ", "NULL_PTR", "NULL_STRING", "NULL_SYMBOL"

        };

        private static HashSet<string> _keywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        public static bool Contains(string value)
        {
            return  _keywords.Contains(value.ToUpper());
        }

        static XSharpKeywords()
        {
            foreach( string kw in XSharpKeywords._SymbolicNames)
            {
                _keywords.Add(kw);
            }
        }
    }
}
