using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web;

namespace XSharp.MacroCompiler.Syntax
{
    internal enum Channel
    {
        Default,
        XmlDoc,
        PreProcessor,
        Hidden,
    };

    internal enum TokenType
    {
        // Keywords
        // Old (VO) Keywords can have 4 letter abbreviations. This can be enabled/disabled with the
        // AllowFourLetterAbbreviations property of the Lexer, which sets the protected field _Four.
        // New (Vulcan) Keywords only full names
        //
        FIRST_KEYWORD,

        // Entity keywords [entity]
        ACCESS, ALIGN, ASPEN, ASSIGN, CALLBACK, CLASS, CLIPPER, DEFINE, DLL, DLLEXPORT, EXPORT, FASTCALL, 
        FUNC, FUNCTION, GLOBAL, HIDDEN, INHERIT, INIT1, INIT2, INIT3, INSTANCE, MEMBER, METHOD, PARAMETERS, LPARAMETERS, PASCAL,
        PRIVATE, PROC, PROCEDURE, PROTECTED, PUBLIC, STRICT, THISCALL, UNION,
        USING, WINCALL,

        // Statement keywords [statement]
        BEGIN, BREAK, CASE, DIM, DO, DOWNTO, ELSE, ELSEIF, END, ENDCASE, ENDDO, ENDIF, EXIT, THEN,
        FOR, IN, LOCAL, LOOP, NAMEOF, NEXT, OTHERWISE,
        RECOVER, RETURN, SEQUENCE, STATIC, STEP, TO,
        UPTO, WHILE,
        DECLARE, DIMENSION,

        // Vulcan stmt keywords [statement]
        CATCH, FINALLY, THROW,

        // Expression keywords
        IIF, IF, AS, OF, SELF, SUPER, SIZEOF, TYPEOF, FIELD, CAST, IS, MEMVAR,

        FIRST_POSITIONAL_KEYWORD,

        // New Vulcan Keywords (no 4 letter abbreviations) [entity]
        ABSTRACT, ANSI, AUTO, CONSTRUCTOR, DELEGATE, DESTRUCTOR, ENUM, EVENT, EXPLICIT, GET, IMPLEMENTS, INITONLY, INTERFACE, INTERNAL,
        NAMESPACE, NEW, OPERATOR, PARTIAL, PROPERTY, SEALED, SET, STRUCTURE, UNICODE, UNTIL, VALUE, VIRTUAL, VOSTRUCT,

        // New Vulcan Keywords (no 4 letter abbreviations) [statement]
        CONST, EACH, FOREACH, IMPLICIT, IMPLIED, LOCK, OUT, REPEAT, SCOPE, TRY,

        // New Vulcan expr Keywords (no 4 letter abbreviations)
        DEFAULT,

        //// New XSharp Keywords (no 4 letter abbreviations) [entity]
        ADD, ASSEMBLY, EXTERN, FIXED, MODULE, OVERRIDE, PARAMS, REMOVE, UNSAFE,

        //// New XSharp Keywords (no 4 letter abbreviations) [statement]
        ARGLIST, ASCENDING, BY, DESCENDING, EQUALS, FROM, GROUP, INTO, JOIN, LET, NOP,
        ON, ORDERBY, SELECT, SWITCH, VAR, VOLATILE, WHERE, YIELD, WITH, WHEN,

        //// New XSharp expr Keywords (no 4 letter abbreviations)
        AWAIT, ASYNC, ASTYPE, CHECKED, UNCHECKED,

        // Fox kws
        M, 

        LAST_POSITIONAL_KEYWORD,

        // Predefined types
        FIRST_TYPE,
        ARRAY, BYTE, CODEBLOCK, DATE, DWORD, FLOAT, INT, LOGIC, LONGINT, OBJECT, PSZ, PTR, REAL4, REAL8, REF, SHORTINT, STRING, SYMBOL, USUAL, VOID, WORD,

        // Vulcan Types
        CHAR, INT64, UINT64,

        // XSharp Types
        DYNAMIC, DECIMAL, DATETIME, CURRENCY, BINARY, 
        LAST_TYPE,

        // UDC Tokens that should be shown in the keyword color [entity]
        UDC_KEYWORD,

        // Scripting directives (pseudo-preprocessor handling) [entity]
        SCRIPT_REF, SCRIPT_LOAD,

        LAST_KEYWORD,

        // Null values
        FIRST_NULL,

        NIL, NULL, NULL_ARRAY, NULL_CODEBLOCK, NULL_DATE, NULL_OBJECT, NULL_PSZ, NULL_PTR, NULL_STRING, NULL_SYMBOL, // NULL_FOX,

        LAST_NULL,

        // Relational operators
        FIRST_OPERATOR,
        LT, LTE, GT, GTE, EQ, EEQ, SUBSTR, NEQ, NEQ2,

        // Prefix and postfix Operators
        INC, DEC,

        // Unary & binary operators
        PLUS, MINUS, DIV, MOD, EXP, LSHIFT, RSHIFT, TILDE, MULT, QQMARK, QMARK,

        // Boolean operators
        AND, OR, NOT,

		// VO Bitwise operators
		BIT_NOT, BIT_AND, BIT_OR, BIT_XOR,

        // Assignments
        ASSIGN_OP, ASSIGN_ADD, ASSIGN_SUB, ASSIGN_EXP, ASSIGN_MUL, ASSIGN_DIV,
        ASSIGN_MOD, ASSIGN_BITAND, ASSIGN_BITOR, ASSIGN_LSHIFT, ASSIGN_RSHIFT,
        ASSIGN_XOR,ASSIGN_QQMARK,


        // Operators
        LOGIC_AND, LOGIC_OR, LOGIC_NOT, LOGIC_XOR, 

        // Symbols
        LPAREN, RPAREN, LCURLY, RCURLY, LBRKT, RBRKT, COLON, COMMA, PIPE, AMP, ADDROF, ALIAS, DOT, COLONCOLON, BACKSLASH, ELLIPSIS,

        LAST_OPERATOR,

        FIRST_CONSTANT,
        // Logics
        FALSE_CONST, TRUE_CONST,
        // Consts
        HEX_CONST, BIN_CONST, INT_CONST, DATE_CONST, DATETIME_CONST, REAL_CONST, REAL_CONST_EXP, INVALID_NUMBER,
        SYMBOL_CONST, CHAR_CONST, STRING_CONST, ESCAPED_STRING_CONST, INTERPOLATED_STRING_CONST, INCOMPLETE_STRING_CONST,
        STRING_CONST_SINGLE,
        BINARY_CONST,
        // FoxPro
        TEXT_STRING_CONST,
        LAST_CONSTANT,

        // Pre processor symbols [entity]
        PP_FIRST,
        PP_COMMAND, PP_DEFINE, PP_ELSE, PP_ENDIF, PP_ENDREGION, PP_ERROR, PP_IF, PP_IFDEF, PP_IFNDEF, PP_INCLUDE, PP_LINE, PP_REGION, PP_STDOUT, PP_TRANSLATE,
        PP_UNDEF, PP_WARNING, PP_PRAGMA,
		// Text .. endText
        PP_TEXT, PP_ENDTEXT,

        PP_LAST,

        // PP constant [entity]
        MACRO,  // __term__
        UDCSEP, // =>

        // Ids
        ID, 

        // Comments
        DOC_COMMENT, SL_COMMENT, ML_COMMENT,

        // Separators
        LINE_CONT, LINE_CONT_OLD,
        SEMI, WS, NL, EOS,

        // Error
        UNRECOGNIZED,

        // Non-lexer tokens
        TYPECAST,
        EOF,

        // Last token
        LAST
    }

    internal class TokenSource
    {
        internal IList<Token> Tokens;
        internal string SourceText;
        internal string SourceName;
        internal TokenSource(string source)
        {
            SourceName = null;
            SourceText = source;
            Tokens = new List<Token>();
        }
        internal Token Get(int index) => index < Tokens.Count ? Tokens[index] : EofToken;
        internal Token EofToken = new Token(TokenType.EOF);
        internal int Size => Tokens.Count;
        internal bool HasTextBefore(int index)
        {
            if (index >= 0 && index < Tokens.Count)
            {
                if (index == 0)
                    return Tokens[index].Start > 0;
                return Tokens[index-1].End < Tokens[index].Start;
            }
            return false;
        }
        internal string TextBefore(int index)
        {
            if (index >= 0 && index < Tokens.Count)
            {
                if (index == 0)
                    return SourceText.Substring(0, Tokens[index].Start);
                var t1 = Tokens[index - 1];
                var t2 = Tokens[index];
                return SourceText.Substring(t1.End, t2.Start - t1.End);
            }
            return null;
        }
        internal string TextAfter(int index)
        {
            if (index >= 0 && index < Tokens.Count)
            {
                if (index == Tokens.Count-1)
                    return SourceText.Substring(Tokens[index].End, SourceText.Length - Tokens[index].End);
                var t1 = Tokens[index];
                var t2 = Tokens[index + 1];
                return SourceText.Substring(t1.End, t2.Start - t1.End);
            }
            return null;
        }
    }

    [DebuggerDisplay("{Type} {Text}")]
    internal class Token
    {
        internal TokenType Type;
        internal TokenType SubType;
        internal Channel Channel;
        internal int Start;
        internal int Length;
        internal string Value;
        internal TokenSource Source = null;
        internal int Index = -1;
        internal int Line => this.Location().Line;
        internal virtual Token SourceSymbol => null;
        internal Token(TokenType type, TokenType subtype, int start, int length, string value, Channel channel)
        {
            this.Type = type;
            this.SubType = subtype;
            this.Start = start;
            this.Length = length;
            this.Channel = channel;
            this.Value = value;
        }
        internal Token(Token o) : this(o.Type, o.SubType, o.Start, o.Length, o.Value, o.Channel)
        {
            Source = o.Source;
            Index = o.Index;
        }
        internal Token(TokenType type) : this(type, TokenType.UNRECOGNIZED, -1, -1, type.ToString(), Channel.Default) { }
        internal Token(TokenType type, string value) : this(type, TokenType.UNRECOGNIZED, -1, -1, value, Channel.Default) { }
        internal Token(Token o, TokenType type, string value) : this(o) { this.Type = type; this.Value = value; }
        internal Token(TokenType type, string value, Token o) : this(o) { this.Type = type; this.Value = value; }
        internal static readonly Token None = new Token(TokenType.UNRECOGNIZED, TokenType.UNRECOGNIZED, -1, 0, null, Channel.Default);
        internal int End => Start + Length;
        public override string ToString() => ( !string.IsNullOrEmpty(Value) ? Value : TokenAttr.TokenText(Type) ) ;
        internal CompilationError Error(ErrorCode e, params object[] args) => Compilation.Error(this, e, args);
        internal string SourceText => Source?.SourceText.Substring(Start, Length);
        internal string Text => Value ?? SourceText;
        internal Token Prev => Index == 0 ? null : Index < 0 ? Source.Tokens.Last() : Source.Tokens[Index - 1];
        internal Token Next => Index >= Source.Tokens.Count-1 ? null : Index < 0 ? null : Source.Tokens[Index + 1];
        internal bool HasTrivia => Source?.HasTextBefore(Index) ?? false;
        internal string TriviaAsText => LeadingWhitespace;
        internal string LeadingWhitespace => Source?.TextBefore(Index);
        internal string TrailingWhitespace => Source?.TextAfter(Index);
    }

    internal class TokenAttr
    {
        static internal readonly IDictionary<string, TokenType> coreKwIds;
        static internal readonly IDictionary<string, TokenType> coreKwIdsS;
        static internal readonly IDictionary<string, TokenType> coreKwIdsE;

        static internal readonly IDictionary<string, TokenType> abbrKwIds;
        static internal readonly IDictionary<string, TokenType> abbrKwIdsS;
        static internal readonly IDictionary<string, TokenType> abbrKwIdsE;

        static internal readonly IDictionary<string, TokenType> voKwIds;
        static internal readonly IDictionary<string, TokenType> voKwIdsS;
        static internal readonly IDictionary<string, TokenType> voKwIdsE;

        static internal readonly IDictionary<string, TokenType> xsKwIds;
        static internal readonly IDictionary<string, TokenType> xsKwIdsS;
        static internal readonly IDictionary<string, TokenType> xsKwIdsE;

        static internal readonly IDictionary<string, TokenType> foxKwIds;
        static internal readonly IDictionary<string, TokenType> foxKwIdsS;
        static internal readonly IDictionary<string, TokenType> foxKwIdsE;

        static internal readonly IDictionary<string, TokenType> symIds;
        static internal readonly IDictionary<string, TokenType> symIdsS;

        static internal readonly TokenType[] specialTable;
        static readonly BitArray softKws;
        static string[] _tokenText = null;

        static TokenAttr()
        {
            var VoKeywordsEnt = new Dictionary<string, TokenType>
            {
                {"ACCESS", TokenType.ACCESS},
                {"ALIGN", TokenType.ALIGN},
                {"ASPEN", TokenType.ASPEN},
                {"ASSIGN", TokenType.ASSIGN},
                {"CALLBACK", TokenType.CALLBACK},
                {"CLASS", TokenType.CLASS},
                {"CLIPPER", TokenType.CLIPPER},
                {"DEFINE", TokenType.DEFINE},
                {"_DLL", TokenType.DLL},
                {"DLLEXPORT", TokenType.DLLEXPORT},
                {"EXPORT", TokenType.EXPORT},
                {"FASTCALL", TokenType.FASTCALL},
                {"FUNCTION", TokenType.FUNCTION},
                {"GLOBAL", TokenType.GLOBAL},
                {"HIDDEN", TokenType.HIDDEN},
                {"INHERIT", TokenType.INHERIT},
                {"INSTANCE", TokenType.INSTANCE},
                {"MEMBER", TokenType.MEMBER},
                {"METHOD", TokenType.METHOD},
                {"PASCAL", TokenType.PASCAL},
                {"PROCEDURE", TokenType.PROCEDURE},
                {"PROTECTED", TokenType.PROTECTED},
                {"STRICT", TokenType.STRICT},
                {"THISCALL", TokenType.THISCALL},
                {"UNION", TokenType.UNION},
                {"WINCALL", TokenType.WINCALL},
            };

            var VoKeywordsStmt = new Dictionary<string, TokenType>
            {
                {"AS", TokenType.AS},
                {"BEGIN", TokenType.BEGIN},
                {"BREAK", TokenType.BREAK},
                {"CASE", TokenType.CASE},
                {"DECLARE", TokenType.DECLARE},
                {"DIM", TokenType.DIM},
                {"DIMENSION", TokenType.DIMENSION},
                {"DO", TokenType.DO},
                {"DOWNTO", TokenType.DOWNTO},
                {"ELSE", TokenType.ELSE},
                {"ELSEIF", TokenType.ELSEIF},
                {"END", TokenType.END},
                {"ENDCASE", TokenType.ENDCASE},
                {"ENDDO", TokenType.ENDDO},
                {"ENDIF", TokenType.ENDIF},
                {"EXIT", TokenType.EXIT},
                {"FOR", TokenType.FOR},
                {"IN", TokenType.IN},
                {"LOCAL", TokenType.LOCAL},
                {"LOOP", TokenType.LOOP},
                {"MEMVAR", TokenType.MEMVAR},
                {"_MEMVAR", TokenType.MEMVAR},
                {"NEXT", TokenType.NEXT},
                {"OF", TokenType.OF},
                {"OTHERWISE", TokenType.OTHERWISE},
                {"PRIVATE", TokenType.PRIVATE},
                {"PUBLIC", TokenType.PUBLIC},
                {"RECOVER", TokenType.RECOVER},
                {"RETURN", TokenType.RETURN},
                {"SELF", TokenType.SELF},
                {"SEQUENCE", TokenType.SEQUENCE},
                {"STATIC", TokenType.STATIC},
                {"STEP", TokenType.STEP},
                {"SUPER", TokenType.SUPER},
                {"TO", TokenType.TO},
                {"THEN", TokenType.THEN },
                {"UPTO", TokenType.UPTO},
                {"USING", TokenType.USING},
                {"WHILE", TokenType.WHILE},

            };

            var VoKeywords = new Dictionary<string, TokenType>
            {
                {"_AND", TokenType.BIT_AND},
                {"_CAST", TokenType.CAST},
                {"FIELD", TokenType.FIELD},
                {"_FIELD", TokenType.FIELD},
                {"IF", TokenType.IF},
                {"IIF", TokenType.IIF},
                {"IS", TokenType.IS},
                {"_NOT", TokenType.BIT_NOT},
                {"_OR", TokenType.BIT_OR},
                {"REF", TokenType.REF},
                {"_XOR", TokenType.BIT_XOR},

			    // Predefined types
                {"ARRAY", TokenType.ARRAY},
                {"BINARY", TokenType.BINARY},
                {"BYTE", TokenType.BYTE},
                {"CODEBLOCK", TokenType.CODEBLOCK},
                {"CURRENCY", TokenType.CURRENCY},
                {"DATE", TokenType.DATE},
                {"DWORD", TokenType.DWORD},
                {"FLOAT", TokenType.FLOAT},
                {"INT", TokenType.INT},
                {"LOGIC", TokenType.LOGIC},
                {"LONGINT", TokenType.LONGINT},
                {"OBJECT", TokenType.OBJECT},
                {"PSZ", TokenType.PSZ},
                {"PTR", TokenType.PTR},
                {"REAL4", TokenType.REAL4},
                {"REAL8", TokenType.REAL8},
                {"SHORTINT", TokenType.SHORTINT},
                {"STRING", TokenType.STRING},
                {"SYMBOL", TokenType.SYMBOL},
                {"USUAL", TokenType.USUAL},
                {"VOID", TokenType.VOID},
                {"WORD", TokenType.WORD},
                
			    // Null types
			    {"NIL", TokenType.NIL},
                {"NULL", TokenType.NULL},
                {"NULL_ARRAY", TokenType.NULL_ARRAY},
                {"NULL_CODEBLOCK", TokenType.NULL_CODEBLOCK},
                {"NULL_DATE", TokenType.NULL_DATE},
                {"NULL_OBJECT", TokenType.NULL_OBJECT},
                {"NULL_PSZ", TokenType.NULL_PSZ},
                {"NULL_PTR", TokenType.NULL_PTR},
                {"NULL_STRING", TokenType.NULL_STRING},
                {"NULL_SYMBOL", TokenType.NULL_SYMBOL},

			    // Consts
			    {"FALSE", TokenType.FALSE_CONST},
                {"TRUE", TokenType.TRUE_CONST},
            };

            var KeywordsEnt = new Dictionary<string, TokenType>
            {
                // VO keywords that cannot be abbreviated
                {"_INIT1", TokenType.INIT1},
                {"_INIT2", TokenType.INIT2},
                {"_INIT3", TokenType.INIT3},

                // Vulcan keywords
			    {"ABSTRACT", TokenType.ABSTRACT},
                {"ANSI", TokenType.ANSI},
                {"AUTO", TokenType.AUTO},
                {"CONSTRUCTOR", TokenType.CONSTRUCTOR},
                {"DELEGATE", TokenType.DELEGATE},
                {"DESTRUCTOR", TokenType.DESTRUCTOR},
                {"ENUM", TokenType.ENUM},
                {"EVENT", TokenType.EVENT},
                {"EXPLICIT", TokenType.EXPLICIT},
                {"GET", TokenType.GET},
                {"IMPLEMENTS", TokenType.IMPLEMENTS},
                {"IMPLICIT", TokenType.IMPLICIT},
                {"INITONLY", TokenType.INITONLY},
                {"INTERFACE", TokenType.INTERFACE},
                {"INTERNAL", TokenType.INTERNAL},
                {"NAMESPACE", TokenType.NAMESPACE},
                {"NEW", TokenType.NEW},
                {"OPERATOR", TokenType.OPERATOR},
                {"PARAMS", TokenType.PARAMS},
                {"PARTIAL", TokenType.PARTIAL},
                {"PROPERTY", TokenType.PROPERTY},
                {"SEALED", TokenType.SEALED},
                {"SET", TokenType.SET},
                {"STRUCTURE", TokenType.STRUCTURE},
                {"STRUCT", TokenType.STRUCTURE},
                {"UNICODE", TokenType.UNICODE},
                {"VALUE", TokenType.VALUE},
                {"VIRTUAL", TokenType.VIRTUAL},
                {"VOSTRUCT", TokenType.VOSTRUCT},

			    // XSharp keywords
                {"ASSEMBLY", TokenType.ASSEMBLY},
                {"ASYNC", TokenType.ASYNC},
                {"EXTERN", TokenType.EXTERN},
                {"MODULE", TokenType.MODULE},

			    // Macros
			    {"__ARRAYBASE__", TokenType.MACRO},
                {"__CLR2__", TokenType.MACRO},
                {"__CLR4__", TokenType.MACRO},
                {"__CLRVERSION__", TokenType.MACRO},
                {"__DATE__", TokenType.MACRO},
                {"__DATETIME__", TokenType.MACRO},
                {"__DEBUG__", TokenType.MACRO},
                {"__DIALECT__", TokenType.MACRO},
                {"__DIALECT_CORE__", TokenType.MACRO},
                {"__DIALECT_VO__", TokenType.MACRO},
                {"__DIALECT_VULCAN__", TokenType.MACRO},
                {"__DIALECT_HARBOUR__", TokenType.MACRO},
                {"__DIALECT_XBASEPP__", TokenType.MACRO},
                {"__DIALECT_FOXPRO__", TokenType.MACRO},
                { "__MEMVAR__", TokenType.MACRO},
                { "__UNDECLARED__", TokenType.MACRO},
                { "__UNSAFE__", TokenType.MACRO},
                {"__ENTITY__", TokenType.MACRO},
                {"__FILE__", TokenType.MACRO},
                {"__FUNCTION__", TokenType.MACRO},
                {"__FUNCTIONS__", TokenType.MACRO},
                {"__LINE__", TokenType.MACRO},
                {"__MODULE__", TokenType.MACRO},
                {"__SIG__", TokenType.MACRO},
                {"__SRCLOC__", TokenType.MACRO},
                {"__SYSDIR__", TokenType.MACRO},
                {"__TIME__", TokenType.MACRO},
                {"__UTCTIME__", TokenType.MACRO},
                {"__VERSION__", TokenType.MACRO},
                {"__VO__", TokenType.MACRO},
                {"__VO1__", TokenType.MACRO},
                {"__VO2__", TokenType.MACRO},
                {"__VO3__", TokenType.MACRO},
                {"__VO4__", TokenType.MACRO},
                {"__VO5__", TokenType.MACRO},
                {"__VO6__", TokenType.MACRO},
                {"__VO7__", TokenType.MACRO},
                {"__VO8__", TokenType.MACRO},
                {"__VO9__", TokenType.MACRO},
                {"__VO10__", TokenType.MACRO},
                {"__VO11__", TokenType.MACRO},
                {"__VO12__", TokenType.MACRO},
                {"__VO13__", TokenType.MACRO},
                {"__VO14__", TokenType.MACRO},
                {"__VO15__", TokenType.MACRO},
                {"__VO16__", TokenType.MACRO},
                {"__VO17__", TokenType.MACRO},
                {"__VULCAN__", TokenType.MACRO},
                {"__WINDIR__", TokenType.MACRO},
                {"__WINDRIVE__", TokenType.MACRO},
                {"__XPP__", TokenType.MACRO},
                {"__XSHARP__", TokenType.MACRO},
                {"__XSHARP_RT__", TokenType.MACRO},
            };

            var KeywordsStmt = new Dictionary<string, TokenType>
            {
                // Vulcan keywords
                {"CATCH", TokenType.CATCH},
                {"CONST", TokenType.CONST},
                {"FINALLY", TokenType.FINALLY},
                {"FOREACH", TokenType.FOREACH},
                {"EACH", TokenType.EACH},
                {"IMPLIED", TokenType.IMPLIED},
                {"LOCK", TokenType.LOCK},
                {"ON", TokenType.ON},
                {"OUT", TokenType.OUT},
                {"REPEAT", TokenType.REPEAT},
                {"SCOPE", TokenType.SCOPE},
                {"THROW", TokenType.THROW},
                {"TRY", TokenType.TRY},
                {"UNTIL", TokenType.UNTIL},

			    // XSharp keywords
	   		    {"__ARGLIST", TokenType.ARGLIST},
                {"ADD", TokenType.ADD},
                {"ASCENDING", TokenType.ASCENDING},
                {"BY", TokenType.BY},
                {"DESCENDING", TokenType.DESCENDING},
                {"EQUALS", TokenType.EQUALS},
                {"FIXED", TokenType.FIXED},
                {"FROM", TokenType.FROM},
                {"GROUP", TokenType.GROUP},
                {"INTO", TokenType.INTO},
                {"JOIN", TokenType.JOIN},
                {"LET", TokenType.LET},
                {"NOP", TokenType.NOP},
                {"NAMEOF", TokenType.NAMEOF},
                {"ORDERBY", TokenType.ORDERBY},
                {"OVERRIDE", TokenType.OVERRIDE},
                {"REMOVE", TokenType.REMOVE},
                {"SELECT", TokenType.SELECT},
                {"SWITCH", TokenType.SWITCH},
                {"UNSAFE", TokenType.UNSAFE},
                {"VAR", TokenType.VAR},
                {"VOLATILE", TokenType.VOLATILE},
                {"WHERE", TokenType.WHERE},
                {"YIELD", TokenType.YIELD},
                {"WITH", TokenType.WITH},
                {"WHEN", TokenType.WHEN},

			    // XSharp types
			    {"DYNAMIC", TokenType.DYNAMIC},

                // FoxPro keywords
                {"PARAMETERS", TokenType.PARAMETERS},
                {"LPARAMETERS", TokenType.LPARAMETERS},
            };

            var Keywords = new Dictionary<string, TokenType>
            {
                // VO keywords that cannot be abbreviated
                {"_SIZEOF", TokenType.SIZEOF},
                {"_TYPEOF", TokenType.TYPEOF},  
                
                // Vulcan keywords
                {"CHAR", TokenType.CHAR},
                {"DEFAULT", TokenType.DEFAULT},
                {"SIZEOF", TokenType.SIZEOF},
                {"TYPEOF", TokenType.TYPEOF},

			    // XSharp keywords
                {"ASTYPE", TokenType.ASTYPE},
                {"AWAIT", TokenType.AWAIT},
                {"CHECKED", TokenType.CHECKED},
                {"UNCHECKED", TokenType.UNCHECKED},

			    // Vulcan types
			    {"INT64", TokenType.INT64},
                {"UINT64", TokenType.UINT64},

                // Fox kws
                {"M", TokenType.M},
            };

            //=====================
            // Expression keywords
            //=====================

            coreKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            abbrKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            voKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            xsKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            foxKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);

            // These keywords are abbreviated for VO
            coreKwIds.AddKeywords(VoKeywords);
            abbrKwIds.AddKeywordAbbrevs(VoKeywords);

            // These keywords are inserted without abbreviations
            coreKwIds.AddKeywords(Keywords);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            xsKwIds.Add("SHORT", TokenType.SHORTINT);
            xsKwIds.Add("LONG", TokenType.LONGINT);
            xsKwIds.Add("_CODEBLOCK", TokenType.CODEBLOCK);

            // Add FoxPro dialect LOGICAL operators AND, OR, NOT and XOR
            foxKwIds.Add("AND", TokenType.LOGIC_AND);
            foxKwIds.Add("OR", TokenType.LOGIC_OR);
            foxKwIds.Add("NOT", TokenType.LOGIC_NOT);
            foxKwIds.Add("XOR", TokenType.LOGIC_XOR);

            //====================
            // Statement keywords
            //====================

            coreKwIdsS = new Dictionary<string, TokenType>(coreKwIds, StringComparer.OrdinalIgnoreCase);
            abbrKwIdsS = new Dictionary<string, TokenType>(abbrKwIds, StringComparer.OrdinalIgnoreCase);
            voKwIdsS = new Dictionary<string, TokenType>(voKwIds, StringComparer.OrdinalIgnoreCase);
            xsKwIdsS = new Dictionary<string, TokenType>(xsKwIds, StringComparer.OrdinalIgnoreCase);
            foxKwIdsS = new Dictionary<string, TokenType>(foxKwIds, StringComparer.OrdinalIgnoreCase);

            // These keywords are abbreviated for VO
            coreKwIdsS.AddKeywords(VoKeywordsStmt);
            abbrKwIdsS.AddKeywordAbbrevs(VoKeywordsStmt);

            // These keywords are inserted without abbreviations
            coreKwIdsS.AddKeywords(KeywordsStmt);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            voKwIdsS.Add("ANY", TokenType.USUAL);

            //=================
            // Entity keywords
            //=================

            coreKwIdsE = new Dictionary<string, TokenType>(coreKwIdsS, StringComparer.OrdinalIgnoreCase);
            abbrKwIdsE = new Dictionary<string, TokenType>(abbrKwIdsS, StringComparer.OrdinalIgnoreCase);
            voKwIdsE = new Dictionary<string, TokenType>(voKwIdsS, StringComparer.OrdinalIgnoreCase);
            xsKwIdsE = new Dictionary<string, TokenType>(xsKwIdsS, StringComparer.OrdinalIgnoreCase);
            foxKwIdsE = new Dictionary<string, TokenType>(foxKwIdsS, StringComparer.OrdinalIgnoreCase);

            // These keywords are abbreviated for VO
            coreKwIdsE.AddKeywords(VoKeywordsEnt);
            abbrKwIdsE.AddKeywordAbbrevs(VoKeywordsEnt);

            // These keywords are inserted without abbreviations
            coreKwIdsE.AddKeywords(KeywordsEnt);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            xsKwIdsE.Add("PROC", TokenType.PROC);
            xsKwIdsE.Add("FUNC", TokenType.FUNC);
            xsKwIdsE.Add("PROTECT", TokenType.PROTECTED);

            //===============
            // Hash keywords
            //===============

            symIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase)
            {
			    // Null types
			    {"#NIL", TokenType.NIL},
                //{"#NULL", TokenType.NULL}, // #NULL symbol must be allowed
                {"#NULL_ARRAY", TokenType.NULL_ARRAY},
                {"#NULL_CODEBLOCK", TokenType.NULL_CODEBLOCK},
                {"#NULL_DATE", TokenType.NULL_DATE},
                {"#NULL_OBJECT", TokenType.NULL_OBJECT},
                {"#NULL_PSZ", TokenType.NULL_PSZ},
                {"#NULL_PTR", TokenType.NULL_PTR},
                {"#NULL_STRING", TokenType.NULL_STRING},
                {"#NULL_SYMBOL", TokenType.NULL_SYMBOL},
            };

            symIdsS = new Dictionary<string, TokenType>(symIds, StringComparer.OrdinalIgnoreCase)
            {
                { "#COMMAND", TokenType.PP_COMMAND},		// #command   <matchPattern> => <resultPattern>
                { "#DEFINE", TokenType.PP_DEFINE},			// #define <idConstant> [<resultText>] or #define <idFunction>([<arg list>]) [<exp>]
                { "#ELSE", TokenType.PP_ELSE},				// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
                { "#ENDIF", TokenType.PP_ENDIF},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
                { "#ENDREGION", TokenType.PP_ENDREGION},	// #region [description]sourceCode#endregion
                { "#ERROR", TokenType.PP_ERROR},			// #error [errorMessage]
                { "#IFDEF", TokenType.PP_IFDEF},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
                { "#IFNDEF", TokenType.PP_IFNDEF},			// #ifndef <identifier>   <statements>...[#else]   <statements>...#endif
                { "#INCLUDE", TokenType.PP_INCLUDE},		// #include "<headerfilename>"
                { "#LINE", TokenType.PP_LINE},				// #line <number> [FileName] or #line default
                { "#REGION", TokenType.PP_REGION},			// #region [description]sourceCode#endregion
                { "#TRANSLATE", TokenType.PP_TRANSLATE},	// #translate <matchPattern> => <resultPattern>
                { "#UNDEF", TokenType.PP_UNDEF},			// #undef <identifier>
                { "#WARNING", TokenType.PP_WARNING},		// #warning [warningMessage]
                { "#XCOMMAND", TokenType.PP_COMMAND},		// #xcommand   <matchPattern> => <resultPattern>  // alias for #command   , no 4 letter abbrev
                { "#XTRANSLATE", TokenType.PP_TRANSLATE},   // #xtranslate <matchPattern> => <resultPattern>  // alias for #translate , no 4 letter abbrev
                { "#YCOMMAND", TokenType.PP_COMMAND},		// #ycommand   <matchPattern> => <resultPattern>  // alias for #xcommand   , case sensitive
                { "#YTRANSLATE", TokenType.PP_TRANSLATE},   // #ytranslate <matchPattern> => <resultPattern>  // alias for #xtranslate , case sensitive
                { "#PRAGMA", TokenType.PP_PRAGMA},          // #pragma: Not supported
                { "#IF", TokenType.PP_IF},			        // #if <condition>   <statements>...[#else]   <statements>...#endif
                { "#USING", TokenType.USING},
                { "#STDOUT", TokenType.PP_STDOUT},			// #stdout [<message>]
                { "#TEXT", TokenType.PP_TEXT},			    // 
                { "#ENDTEXT", TokenType.PP_ENDTEXT},		// 
            };

            //=================
            // Special symbols
            //=================

            specialTable = new TokenType[128];
            for (int i = 0; i < specialTable.Length; i++)
                specialTable[i] = TokenType.UNRECOGNIZED;
            specialTable['('] = TokenType.LPAREN;
            specialTable[')'] = TokenType.RPAREN;
            specialTable['{'] = TokenType.LCURLY;
            specialTable['}'] = TokenType.RCURLY;
            specialTable['['] = TokenType.LBRKT;
            specialTable[']'] = TokenType.RBRKT;
            specialTable[':'] = TokenType.COLON; // :: :=
            specialTable[','] = TokenType.COMMA;
            specialTable['\\'] = TokenType.BACKSLASH;
            specialTable['|'] = TokenType.PIPE; // || |=
            specialTable['&'] = TokenType.AMP; // && &=
            specialTable['@'] = TokenType.ADDROF; // @@id
            specialTable['-'] = TokenType.MINUS; // -> -- -=
            specialTable['+'] = TokenType.PLUS; // ++ +=
            specialTable['/'] = TokenType.DIV; // // /* /=
            specialTable['%'] = TokenType.MOD; // %=
            specialTable['^'] = TokenType.EXP; // ^=
            specialTable['<'] = TokenType.LT; // << <<= <= <>
            specialTable['>'] = TokenType.GT; // >> >>= >=
            specialTable['~'] = TokenType.TILDE; // ~= ~"..."
            specialTable['*'] = TokenType.MULT; // *comment *= ** **=
            specialTable['?'] = TokenType.QMARK; // ??
            specialTable['='] = TokenType.EQ; // == =>
            specialTable['$'] = TokenType.SUBSTR;
            specialTable['!'] = TokenType.NOT; // !=
            specialTable[';'] = TokenType.SEMI; // ;\n
            specialTable['.'] = TokenType.DOT; // .F. .T. .N. .Y. ... .OR. .AND. .NOT. .XOR.
            specialTable['\r'] = TokenType.NL; // \r\r\r...
            specialTable['\n'] = TokenType.NL; // \n\n\n...
            specialTable['\t'] = TokenType.WS; // ...
            specialTable[' '] = TokenType.WS; // ...
            specialTable['#'] = TokenType.NEQ2; // #sym
            specialTable['\''] = TokenType.STRING_CONST_SINGLE;
            specialTable['"'] = TokenType.STRING_CONST;

            specialTable['_'] = TokenType.ID;
            for (char c = 'A'; c <= 'Z'; c++)
                specialTable[c] = TokenType.ID;
            for (char c = 'a'; c <= 'z'; c++)
                specialTable[c] = TokenType.ID;

            for (char c = '0'; c <= '9'; c++)
                specialTable[c] = TokenType.INT_CONST;

            //===============
            // Soft keywords
            //===============

            softKws = new BitArray((int)TokenType.LAST);

            //vo soft KWs
            softKws[(int)TokenType.DATE] = true;
            softKws[(int)TokenType.DATETIME] = true;
            softKws[(int)TokenType.ARRAY] = true;
            softKws[(int)TokenType.OF] = true;

            //vulcan soft KWs
            softKws[(int)TokenType.ABSTRACT] = true;
            softKws[(int)TokenType.ANSI] = true;
            softKws[(int)TokenType.AUTO] = true;
            softKws[(int)TokenType.CHAR] = true;
            softKws[(int)TokenType.CONST] = true;
            softKws[(int)TokenType.DEFAULT] = true;
            softKws[(int)TokenType.EXPLICIT] = true;
            softKws[(int)TokenType.FOREACH] = true;
            softKws[(int)TokenType.EACH] = true;
            softKws[(int)TokenType.GET] = true;
            softKws[(int)TokenType.IMPLEMENTS] = true;
            softKws[(int)TokenType.IMPLICIT] = true;
            softKws[(int)TokenType.IMPLIED] = true;
            softKws[(int)TokenType.INITONLY] = true;
            softKws[(int)TokenType.INTERNAL] = true;
            softKws[(int)TokenType.LOCK] = true;
            softKws[(int)TokenType.NAMESPACE] = true;
            softKws[(int)TokenType.NEW] = true;
            softKws[(int)TokenType.OUT] = true;
            softKws[(int)TokenType.PARTIAL] = true;
            softKws[(int)TokenType.SCOPE] = true;
            softKws[(int)TokenType.SEALED] = true;
            softKws[(int)TokenType.SET] = true;
            softKws[(int)TokenType.TRY] = true;
            softKws[(int)TokenType.UNICODE] = true;
            softKws[(int)TokenType.VALUE] = true;
            softKws[(int)TokenType.VIRTUAL] = true;

            // X# soft KWs
            softKws[(int)TokenType.ADD] = true;
            softKws[(int)TokenType.ARGLIST] = true;
            softKws[(int)TokenType.ASCENDING] = true;
            softKws[(int)TokenType.ASSEMBLY] = true;
            softKws[(int)TokenType.ASYNC] = true;
            softKws[(int)TokenType.AWAIT] = true;
            softKws[(int)TokenType.BINARY] = true;
            softKws[(int)TokenType.BY] = true;
            softKws[(int)TokenType.CHECKED] = true;
            softKws[(int)TokenType.CURRENCY] = true;
            softKws[(int)TokenType.DESCENDING] = true;
            softKws[(int)TokenType.DYNAMIC] = true;
            softKws[(int)TokenType.EQUALS] = true;
            softKws[(int)TokenType.EXTERN] = true;
            softKws[(int)TokenType.FIELD] = true;
            softKws[(int)TokenType.FIXED] = true;
            softKws[(int)TokenType.FROM] = true;
            softKws[(int)TokenType.GROUP] = true;
            softKws[(int)TokenType.INTO] = true;
            softKws[(int)TokenType.JOIN] = true;
            softKws[(int)TokenType.LET] = true;
            softKws[(int)TokenType.MODULE] = true;
            softKws[(int)TokenType.NAMEOF] = true;
            softKws[(int)TokenType.NOP] = true;
            softKws[(int)TokenType.ON] = true;
            softKws[(int)TokenType.ORDERBY] = true;
            softKws[(int)TokenType.OVERRIDE] = true;
            softKws[(int)TokenType.PARAMS] = true;
            softKws[(int)TokenType.REMOVE] = true;
            softKws[(int)TokenType.SELECT] = true;
            softKws[(int)TokenType.SWITCH] = true;
            softKws[(int)TokenType.UNCHECKED] = true;
            softKws[(int)TokenType.UNSAFE] = true;
            softKws[(int)TokenType.VAR] = true;
            softKws[(int)TokenType.VOLATILE] = true;
            softKws[(int)TokenType.WHERE] = true;
            softKws[(int)TokenType.YIELD] = true;
            softKws[(int)TokenType.WITH] = true;
            softKws[(int)TokenType.WHEN] = true;
            softKws[(int)TokenType.CHAR] = true;
            //softKws[(int)TokenType.MEMVAR] = true;
            softKws[(int)TokenType.PARAMETERS] = true;
            softKws[(int)TokenType.LPARAMETERS] = true;
            softKws[(int)TokenType.DEFINE] = true;
            softKws[(int)TokenType.DELEGATE] = true;
            softKws[(int)TokenType.ENUM] = true;
            softKws[(int)TokenType.GLOBAL] = true;
            softKws[(int)TokenType.INHERIT] = true;
            softKws[(int)TokenType.INTERFACE] = true;
            softKws[(int)TokenType.OPERATOR] = true;
            softKws[(int)TokenType.PROPERTY] = true;
            softKws[(int)TokenType.STRUCTURE] = true;
            softKws[(int)TokenType.VOSTRUCT] = true;
            softKws[(int)TokenType.ALIGN] = true;
            softKws[(int)TokenType.CALLBACK] = true;
            softKws[(int)TokenType.CLIPPER] = true;
            softKws[(int)TokenType.DECLARE] = true;
            softKws[(int)TokenType.DIM] = true;
            softKws[(int)TokenType.DIMENSION] = true;
            softKws[(int)TokenType.DOWNTO] = true;
            softKws[(int)TokenType.DLLEXPORT] = true;
            softKws[(int)TokenType.EVENT] = true;
            softKws[(int)TokenType.FASTCALL] = true;
            softKws[(int)TokenType.FUNC] = true;
            softKws[(int)TokenType.IN] = true;
            softKws[(int)TokenType.INSTANCE] = true;
            softKws[(int)TokenType.PASCAL] = true;
            softKws[(int)TokenType.PROC] = true;
            softKws[(int)TokenType.SEQUENCE] = true;
            softKws[(int)TokenType.STEP] = true;
            softKws[(int)TokenType.STRICT] = true;
            softKws[(int)TokenType.TO] = true;
            softKws[(int)TokenType.THEN] = true;
            softKws[(int)TokenType.THISCALL] = true;
            softKws[(int)TokenType.UNION] = true;
            softKws[(int)TokenType.UNTIL] = true;
            softKws[(int)TokenType.UPTO] = true;
            softKws[(int)TokenType.USING] = true;
            softKws[(int)TokenType.WINCALL] = true;

            // fox soft KWs
            softKws[(int)TokenType.M] = true;

            for (var i = (int)TokenType.FIRST_POSITIONAL_KEYWORD + 1; i < (int)TokenType.LAST_POSITIONAL_KEYWORD; i++)
            {
                softKws[i] = true;
            }
        }

        internal static bool IsSoftKeyword(TokenType t)
        {
            return softKws[(int)t];
        }

        internal static string TokenText(TokenType token)
        {
            if (_tokenText == null)
            {
                string[] v = new string[(int)TokenType.LAST];

                for (TokenType t = TokenType.FIRST_KEYWORD; t < TokenType.LAST; t++)
                {
                    v[(int)t] = t.ToString();
                }

                foreach (var s in xsKwIds.Keys)
                {
                    var t = xsKwIds[s];
                    v[(int)t] = s;
                }

                // Relational operators
                v[(int)TokenType.LT] = "<";
                v[(int)TokenType.LTE] = "<=";
                v[(int)TokenType.GT] = ">";
                v[(int)TokenType.GTE] = ">=";
                v[(int)TokenType.EQ] = "=";
                v[(int)TokenType.EEQ] = "==";
                v[(int)TokenType.SUBSTR] = "$";
                v[(int)TokenType.NEQ] = "!=";
                v[(int)TokenType.NEQ2] = "#";

                // Prefix and postfix Operators
                v[(int)TokenType.INC] = "++";
                v[(int)TokenType.DEC] = "--";

                // Unary & binary operators
                v[(int)TokenType.AMP] = "&";
                v[(int)TokenType.PLUS] = "+";
                v[(int)TokenType.MINUS] = "-";
                v[(int)TokenType.DIV] = "/";
                v[(int)TokenType.MOD] = "%";
                v[(int)TokenType.EXP] = "^";
                v[(int)TokenType.LSHIFT] = "<<";
                v[(int)TokenType.RSHIFT] = ">>";
                v[(int)TokenType.TILDE] = "~";
                v[(int)TokenType.MULT] = "*";
                v[(int)TokenType.QQMARK] = "?";
                v[(int)TokenType.QMARK] = "??";

                // Boolean operators
                v[(int)TokenType.AND] = "&&";
                v[(int)TokenType.OR] = "||";
                v[(int)TokenType.NOT] = "!";

                // Assignments
                v[(int)TokenType.ASSIGN_OP] = ":=";
                v[(int)TokenType.ASSIGN_ADD] = "+=";
                v[(int)TokenType.ASSIGN_SUB] = "-=";
                v[(int)TokenType.ASSIGN_EXP] = "^=";
                v[(int)TokenType.ASSIGN_MUL] = "*=";
                v[(int)TokenType.ASSIGN_DIV] = "/=";
                v[(int)TokenType.ASSIGN_MOD] = "%=";
                v[(int)TokenType.ASSIGN_BITAND] = "&=";
                v[(int)TokenType.ASSIGN_BITOR] = "|=";
                v[(int)TokenType.ASSIGN_LSHIFT] = "<<=";
                v[(int)TokenType.ASSIGN_RSHIFT] = ">>=";
                v[(int)TokenType.ASSIGN_XOR] = "~=";


                // Symbols
                v[(int)TokenType.LPAREN] = "(";
                v[(int)TokenType.RPAREN] = ")";
                v[(int)TokenType.LCURLY] = "{";
                v[(int)TokenType.RCURLY] = "}";
                v[(int)TokenType.LBRKT] = "[";
                v[(int)TokenType.RBRKT] = "]";
                v[(int)TokenType.COLON] = ":";
                v[(int)TokenType.COMMA] = ",";
                v[(int)TokenType.PIPE] = "|";
                v[(int)TokenType.AMP] = "&";
                v[(int)TokenType.ADDROF] = "@";
                v[(int)TokenType.ALIAS] = "->";
                v[(int)TokenType.DOT] = ".";
                v[(int)TokenType.COLONCOLON] = "::";
                v[(int)TokenType.BACKSLASH] = "\\";
                v[(int)TokenType.ELLIPSIS] = "...";

                // PP constant
                v[(int)TokenType.UDCSEP] = "=>";


                System.Threading.Interlocked.CompareExchange(ref _tokenText, v, null);
            }
            return _tokenText[(int)token];
        }
    }
    internal static class TokenExtensions
    {
        internal static void AddKeywords(this IDictionary<string, TokenType> kwIds, Dictionary<string, TokenType> keywords)
        {
            foreach (var text in keywords.Keys)
            {
                var token = keywords[text];
                kwIds.Add(text, token);
            }
        }
        internal static void AddKeywordAbbrevs(this IDictionary<string, TokenType> kwIds, Dictionary<string, TokenType> keywords, int minLen = 4)
        {
            foreach (var text in keywords.Keys)
            {
                var token = keywords[text];
                {
                    var s = text;
                    while (s.Length > minLen)
                    {
                        s = s.Substring(0, s.Length - 1);
                        if (!kwIds.ContainsKey(s))
                            kwIds.Add(s, token);
                    }
                }
            }
        }
        internal static SourceLocation Location(this Token token) => new SourceLocation(token.Source.SourceText, token.Start);
        public static bool IsMemberOperator(this Token token)
        {
            switch (token.Type)
            {
                case TokenType.DOT:
                case TokenType.COLON:
                case TokenType.ALIAS:
                case TokenType.COLONCOLON:
                    return true;
            }
            return false;
        }

    }
}
