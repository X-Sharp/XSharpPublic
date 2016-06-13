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
    public class XSharpKeywords
    {
        // Pasted from XSharParser, and reworked a bit
        private static readonly string[] _SymbolicNames = {
        "ACCESS", "ALIGN", "AS", "ASSIGN", "BEGIN", "BREAK", "CASE", "CAST",
        "CLASS", "CLIPPER", "DEFINE", "DIM", "DLL", "DO", "DOWNTO", "ELSE", "ELSEIF",
        "END", "ENDCASE", "ENDDO", "ENDIF", "EXIT", "EXPORT", "FASTCALL", "FIELD",
        "FOR", "FUNCTION", "GLOBAL", "HIDDEN", "IF", "IIF", "INHERIT", "INIT1",
        "INIT2", "INIT3", "INSTANCE", "IS", "LOCAL", "LOOP", "MEMBER", "MEMVAR",
        "METHOD", "NEXT", "OTHERWISE", "PARAMETERS", "PASCAL", "PRIVATE", "PROCEDURE",
        "PROTECTED", "PUBLIC", "RECOVER", "RETURN", "SELF", "SEQUENCE", "SIZEOF",
        "STATIC", "STEP", "STRICT", "SUPER", "THISCALL", "TO", "TYPEOF", "UNION",
        "UPTO", "USING", "WHILE", "CATCH", "FINALLY", "THROW", "ABSTRACT", "AUTO",
        "CONSTRUCTOR", "CONST", "DEFAULT", "DELEGATE", "DESTRUCTOR", "ENUM", "EVENT",
        "EXPLICIT", "FOREACH", "GET", "IMPLEMENTS", "IMPLICIT", "IMPLIED", "IN",
        "INITONLY", "INTERFACE", "INTERNAL", "LOCK", "NAMESPACE", "NEW", "OFF",
        "ON", "OPERATOR", "OPTIONS", "OUT", "PARTIAL", "PROPERTY", "REPEAT", "SCOPE",
        "SEALED", "SET", "STRUCTURE", "TRY", "UNTIL", "VALUE", "VIRTUAL", "VOSTRUCT",
        "WARNINGS", "ASCENDING", "ASSEMBLY", "ASYNC", "AWAIT", "BY", "CHECKED",
        "DESCENDING", "EQUALS", "EXTERN", "FROM", "GROUP", "INTO", "JOIN", "LET",
        "MODULE", "ORDERBY", "SELECT", "SWITCH", "UNCHECKED", "UNSAFE", "VAR",
        "VOLATILE", "WHERE", "YIELD", "ARRAY", "BYTE", "CHAR", "CODEBLOCK", "DATE",
        "DWORD", "FLOAT", "INT", "LOGIC", "LONGINT", "OBJECT", "PSZ", "PTR", "REAL4",
        "REAL8", "REF", "SHORTINT", "STRING", "SYMBOL", "USUAL", "VOID", "WORD",
        "INT64", "UINT64", "DYNAMIC", "NIL", "NULL", "NULL_ARRAY", "NULL_CODEBLOCK",
        "NULL_DATE", "NULL_OBJECT", "NULL_PSZ", "NULL_PTR", "NULL_STRING", "NULL_SYMBOL"
        };

        private static HashSet<string> _keywords = new HashSet<string>();

        internal static bool Contains(string value)
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
