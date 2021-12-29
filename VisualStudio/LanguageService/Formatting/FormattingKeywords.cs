using Microsoft.VisualStudio.Text;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Classification;
using System.Linq;
using System;
using LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;

namespace XSharp.LanguageService.Formatting
{
    /// <summary>
    /// This enum is used in the Formatting code.
    /// It contains synonyms for the XSharpLexer token types
    /// The size is 16 bits, so a combination of 2 values can fit into an integer
    /// Not all the Lexer tokens are in here. We do not need them all.
    /// </summary>
    internal enum XKeyword : Int16
        {
        None = 0,
        // Types
        Class = XSharpLexer.CLASS,
        Interface = XSharpLexer.INTERFACE,
        Structure = XSharpLexer.STRUCTURE,
        VoStruct = XSharpLexer.VOSTRUCT,
        Union = XSharpLexer.UNION,
        Enum = XSharpLexer.ENUM,
        Define = XSharpLexer.DEFINE, // foxpro define class
        Enddefine = XSharpLexer.ENDDEFINE, // foxpro define class
        // other (single line) types are not needed for indenting the code
        // Members
        Method = XSharpLexer.METHOD,
        Access = XSharpLexer.ACCESS,
        Assign = XSharpLexer.ASSIGN,
        Constructor = XSharpLexer.CONSTRUCTOR,
        Destructor = XSharpLexer.DESTRUCTOR,
        Property = XSharpLexer.PROPERTY,
        Event = XSharpLexer.EVENT,
        Operator = XSharpLexer.OPERATOR,
        Function = XSharpLexer.FUNCTION,
        Procedure = XSharpLexer.PROCEDURE,
        Get = XSharpLexer.GET,
        Set = XSharpLexer.SET,
        Init = XSharpLexer.INIT,
        Add = XSharpLexer.ADD,
        Remove = XSharpLexer.REMOVE,
        // other (single line) members are not needed for indenting the code

        // Control structures
        Do = XSharpLexer.DO,
        While = XSharpLexer.WHILE,
        Enddo = XSharpLexer.ENDDO,
        Case = XSharpLexer.CASE,
        Otherwise = XSharpLexer.OTHERWISE,
        Endcase = XSharpLexer.ENDCASE,
        Switch = XSharpLexer.SWITCH,
        For = XSharpLexer.FOR,
        Foreach = XSharpLexer.FOREACH,
        Next = XSharpLexer.NEXT,
        If = XSharpLexer.IF,
        Else = XSharpLexer.ELSE,
        Elseif = XSharpLexer.ELSEIF,
        Endif = XSharpLexer.ENDIF,
        Repeat = XSharpLexer.REPEAT,
        Until = XSharpLexer.UNTIL,
        Try = XSharpLexer.TRY,
        Catch = XSharpLexer.CATCH,
        Finally = XSharpLexer.FINALLY,
        Sequence = XSharpLexer.SEQUENCE,
        Recover = XSharpLexer.RECOVER,
        Text = XSharpLexer.TEXT,
        Endtext = XSharpLexer.ENDTEXT,

        // preprocessor
        PP_else = XSharpLexer.PP_ELSE,
        PP_endif = XSharpLexer.PP_ENDIF,
        PP_endregion = XSharpLexer.PP_ENDREGION,
        PP_ifdef = XSharpLexer.PP_IFDEF,
        PP_ifndef = XSharpLexer.PP_IFNDEF,
        PP_region = XSharpLexer.PP_REGION,
        // other preprocessor keywords are not needed for indenting the code

        // Block structures
        Namespace = XSharpLexer.NAMESPACE,
        Begin = XSharpLexer.BEGIN,
        End = XSharpLexer.END,
        Scope = XSharpLexer.SCOPE,
        Fixed = XSharpLexer.FIXED,
        Unsafe = XSharpLexer.UNSAFE,
        Checked = XSharpLexer.CHECKED,
        Unchecked = XSharpLexer.UNCHECKED,
        Using = XSharpLexer.USING,
        Lock = XSharpLexer.LOCK,
        With = XSharpLexer.WITH,
        Local = XSharpLexer.LOCAL, // for LOCAL PROCEDURE


        Return = XSharpLexer.RETURN,
        Self = XSharpLexer.SELF,
        Throw = XSharpLexer.THROW,
        UDC = XSharpLexer.UDC_KEYWORD,
        PP_undef = XSharpLexer.PP_UNDEF,
        PP_translate = XSharpLexer.PP_TRANSLATE,
        PP_command = XSharpLexer.PP_COMMAND,
        PP_pragma = XSharpLexer.PRAGMA,
        Var = XSharpLexer.VAR,
        Nop = XSharpLexer.NOP,
        Decimal = XSharpLexer.DECIMAL,
        


        Eof = XSharpLexer.Eof,
        Eos = XSharpLexer.EOS,
        Id = XSharpLexer.ID,
    }
}
