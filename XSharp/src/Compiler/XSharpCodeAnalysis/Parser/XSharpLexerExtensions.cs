using System;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{

    public partial class XSharpLexer : Antlr4.Runtime.Lexer
    {
        // Several Help methods that can be used for colorizing in an editor
        public static bool IsKeyword(int iToken)
        {
            return iToken > XSharpLexer.FIRST_KEYWORD && iToken < XSharpLexer.LAST_KEYWORD;
        }
        public static bool IsOperator(int iToken)
        {
            return (iToken > XSharpLexer.FIRST_OPERATOR && iToken < XSharpLexer.LAST_OPERATOR)
                || iToken == XSharpLexer.PRAGMA || iToken == XSharpLexer.HASHUSING
                || iToken == XSharpLexer.PP_SYMBOLS || iToken == XSharpLexer.SEMI;
        }
        public static bool IsConstant(int iToken)
        {
            return (iToken > XSharpLexer.FIRST_CONSTANT && iToken < XSharpLexer.LAST_CONSTANT)
                || iToken == XSharpLexer.MACRO;
        }
        public static bool IsIdentifier(int iToken)
        {
            return iToken == XSharpLexer.ID || iToken == XSharpLexer.KWID;
        }
        public static bool IsComment(int iToken)
        {
            return iToken == XSharpLexer.SL_COMMENT || iToken == XSharpLexer.ML_COMMENT || iToken == XSharpLexer.DOC_COMMENT;
        }

    }
}