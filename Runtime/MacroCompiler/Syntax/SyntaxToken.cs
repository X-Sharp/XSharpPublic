using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    internal enum Channel
    {
        XMLDOCCHANNEL,
        DEFOUTCHANNEL,
        PREPROCESSORCHANNEL,
        PRAGMACHANNEL,
        HIDDENCHANNEL,
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
        ACCESS, ALIGN, ASPEN, ASSIGN, CALLBACK, CLASS, CLIPPER, DECLARE, DEFINE, DLL, DLLEXPORT, EXPORT, FASTCALL, 
        FUNC, FUNCTION, GLOBAL, HIDDEN, INHERIT, INIT1, INIT2, INIT3, INSTANCE, MEMBER, METHOD, PARAMETERS, PASCAL,
        PRIVATE, PROC, PROCEDURE, PROTECTED, PUBLIC, STRICT, THISCALL, UNION,
        USING, WINCALL,

        // Statement keywords [statement]
        BEGIN, BREAK, CASE, DIM, DO, DOWNTO, ELSE, ELSEIF, END, ENDCASE, ENDDO, ENDIF, EXIT,
        FOR, IN, LOCAL, LOOP, NAMEOF, NEXT, OTHERWISE,
        RECOVER, RETURN, SEQUENCE, STATIC, STEP, TO,
        UPTO, WHILE,

        // Vulcan stmt keywords [statement]
        CATCH, FINALLY, THROW,

        // Expression keywords
        IIF, IF, AS, SELF, SUPER, SIZEOF, TYPEOF, FIELD, CAST, IS, MEMVAR,


        FIRST_POSITIONAL_KEYWORD,
        // New Vulcan Keywords (no 4 letter abbreviations) [entity]
        ABSTRACT, ANSI, AUTO, CONSTRUCTOR, DELEGATE, DESTRUCTOR, ENUM, EVENT, EXPLICIT, GET, IMPLEMENTS, INITONLY, INTERFACE, INTERNAL,
        NAMESPACE, NEW, OPERATOR, PARTIAL, PROPERTY, SEALED, SET, STRUCTURE, UNICODE, UNTIL, VALUE, VIRTUAL, VOSTRUCT,

        // New Vulcan Keywords (no 4 letter abbreviations) [statement]
        CONST, FOREACH, IMPLICIT, IMPLIED, LOCK, OUT, REPEAT, SCOPE, TRY,

        // New Vulcan expr Keywords (no 4 letter abbreviations)
        DEFAULT,

        //// New XSharp Keywords (no 4 letter abbreviations) [entity]
        ADD, ASSEMBLY, EXTERN, FIXED, MODULE, OVERRIDE, PARAMS, REMOVE, UNSAFE,

        //// New XSharp Keywords (no 4 letter abbreviations) [statement]
        ARGLIST, ASCENDING, BY, DESCENDING, EQUALS, FROM, GROUP, INTO, JOIN, LET, NOP,
        ON, ORDERBY, SELECT, SWITCH, VAR, VOLATILE, WHERE, YIELD,

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
        DYNAMIC, DECIMAL, DATETIME,
        LAST_TYPE,

        // UDC Tokens that should be shown in the keyword color [entity]
        UDC_KEYWORD,

        // Scripting directives (pseudo-preprocessor handling) [entity]
        SCRIPT_REF, SCRIPT_LOAD,

        LAST_KEYWORD,

        // Null values
        FIRST_NULL,

        NIL, NULL, NULL_ARRAY, NULL_CODEBLOCK, NULL_DATE, NULL_OBJECT, NULL_PSZ, NULL_PTR, NULL_STRING, NULL_SYMBOL,

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
        VO_NOT, VO_AND, VO_OR, VO_XOR,

        // Assignments
        ASSIGN_OP, ASSIGN_ADD, ASSIGN_SUB, ASSIGN_EXP, ASSIGN_MUL, ASSIGN_DIV,
        ASSIGN_MOD, ASSIGN_BITAND, ASSIGN_BITOR, ASSIGN_LSHIFT, ASSIGN_RSHIFT,
        ASSIGN_XOR,


        // Operators
        LOGIC_AND, LOGIC_OR, LOGIC_NOT, LOGIC_XOR,

        // Symbols
        LPAREN, RPAREN, LCURLY, RCURLY, LBRKT, RBRKT, COLON, COMMA, PIPE, AMP, ADDROF, ALIAS, DOT, COLONCOLON, BACKSLASH, ELLIPSIS,

        LAST_OPERATOR,

        FIRST_CONSTANT,
        // Logics
        FALSE_CONST, TRUE_CONST,
        // Consts
        HEX_CONST, BIN_CONST, INT_CONST, DATE_CONST, REAL_CONST, REAL_CONST_EXP, SYMBOL_CONST, CHAR_CONST, INVALID_NUMBER,
        STRING_CONST, ESCAPED_STRING_CONST, INTERPOLATED_STRING_CONST, INCOMPLETE_STRING_CONST,
        STRING_CONST_SINGLE,

        LAST_CONSTANT,

        // Pre processor symbols [entity]
        PP_FIRST,
        PP_COMMAND, PP_DEFINE, PP_ELSE, PP_ENDIF, PP_ENDREGION, PP_ERROR, PP_IFDEF, PP_IFNDEF, PP_INCLUDE, PP_LINE, PP_REGION, PP_TRANSLATE,
        PP_UNDEF, PP_WARNING,
        PP_LAST,

        // PP constant [entity]
        MACRO,  // __term__
        UDCSEP, // =>

        // Ids
        ID, KWID,

        // Pragma [entity]
        PRAGMA,

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

    internal class Token
    {
        internal TokenType type;
        internal TokenType subtype;
        internal Channel channel;
        internal int start;
        internal int length;
        internal string value;
        internal Token(TokenType type, TokenType subtype, int start, int length, string value, Channel channel)
        {
            this.type = type;
            this.subtype = subtype;
            this.start = start;
            this.length = length;
            this.channel = channel;
            this.value = value;
        }
        internal static readonly Token None = new Token(TokenType.UNRECOGNIZED, TokenType.UNRECOGNIZED, -1, 0, null, Channel.DEFOUTCHANNEL);
        public override string ToString() => "'" + value + "'";
    }

    internal class TokenAttr
    {
        static internal readonly IDictionary<string, TokenType> voKwIds;
        static internal readonly IDictionary<string, TokenType> voKwIdsS;
        static internal readonly IDictionary<string, TokenType> voKwIdsE;

        static internal readonly IDictionary<string, TokenType> xsKwIds;
        static internal readonly IDictionary<string, TokenType> xsKwIdsS;
        static internal readonly IDictionary<string, TokenType> xsKwIdsE;

        static internal readonly IDictionary<string, TokenType> symIds;
        static internal readonly IDictionary<string, TokenType> symIdsE;

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
                {"DECLARE", TokenType.DECLARE},
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
                {"PARAMETERS", TokenType.PARAMETERS},
                {"PASCAL", TokenType.PASCAL},
                {"PRIVATE", TokenType.PRIVATE},
                {"PROCEDURE", TokenType.PROCEDURE},
                {"PROTECTED", TokenType.PROTECTED},
                {"PUBLIC", TokenType.PUBLIC},
                {"STRICT", TokenType.STRICT},
                {"THISCALL", TokenType.THISCALL},
                {"UNION", TokenType.UNION},
                {"USING", TokenType.USING},
                {"WINCALL", TokenType.WINCALL},
            };

            var VoKeywordsStmt = new Dictionary<string, TokenType>
            {
                {"AS", TokenType.AS},
                {"BEGIN", TokenType.BEGIN},
                {"BREAK", TokenType.BREAK},
                {"CASE", TokenType.CASE},
                {"DIM", TokenType.DIM},
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
                {"NEXT", TokenType.NEXT},
                {"OTHERWISE", TokenType.OTHERWISE},
                {"RECOVER", TokenType.RECOVER},
                {"RETURN", TokenType.RETURN},
                {"SELF", TokenType.SELF},
                {"SEQUENCE", TokenType.SEQUENCE},
                {"STATIC", TokenType.STATIC},
                {"STEP", TokenType.STEP},
                {"SUPER", TokenType.SUPER},
                {"TO", TokenType.TO},
                {"UPTO", TokenType.UPTO},
                {"WHILE", TokenType.WHILE},

            };

            var VoKeywords = new Dictionary<string, TokenType>
            {
                {"_AND", TokenType.VO_AND},
                {"_CAST", TokenType.CAST},
                {"FIELD", TokenType.FIELD},
                {"_FIELD", TokenType.FIELD},
                {"IF", TokenType.IF},
                {"IIF", TokenType.IIF},
                {"IS", TokenType.IS},
                {"_NOT", TokenType.VO_NOT},
                {"_OR", TokenType.VO_OR},
                {"REF", TokenType.REF},
                {"_XOR", TokenType.VO_XOR},

			    // Predefined types
                {"ARRAY", TokenType.ARRAY},
                {"BYTE", TokenType.BYTE},
                {"CODEBLOCK", TokenType.CODEBLOCK},
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
                {"__ENTITY__", TokenType.MACRO},
                {"__FILE__", TokenType.MACRO},
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

			    // XSharp types
			    {"DYNAMIC", TokenType.DYNAMIC},
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

            voKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            xsKwIds = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            xsKwIds.Add("SHORT", TokenType.SHORTINT);
            xsKwIds.Add("LONG", TokenType.LONGINT);
            xsKwIds.Add("_CODEBLOCK", TokenType.CODEBLOCK);

            // These keywords are abbreviated for VO
            foreach (var text in VoKeywords.Keys)
            {
                var token = VoKeywords[text];
                xsKwIds.Add(text, token);
                voKwIds.Add(text, token);
                {
                    var s = text;
                    while (s.Length > 4)
                    {
                        s = s.Substring(0, s.Length - 1);
                        if (!voKwIds.ContainsKey(s))
                            voKwIds.Add(s, token);
                    }
                }
            }

            // These keywords are inserted without abbreviations
            foreach (var text in Keywords.Keys)
            {
                var token = Keywords[text];
                xsKwIds.Add(text, token);
                voKwIds.Add(text, token);
            }

            voKwIdsS = new Dictionary<string, TokenType>(voKwIds, StringComparer.OrdinalIgnoreCase);
            xsKwIdsS = new Dictionary<string, TokenType>(xsKwIds, StringComparer.OrdinalIgnoreCase);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            voKwIdsS.Add("ANY", TokenType.USUAL);

            // These keywords are abbreviated for VO
            foreach (var text in VoKeywordsStmt.Keys)
            {
                var token = VoKeywordsStmt[text];
                xsKwIdsS.Add(text, token);
                voKwIdsS.Add(text, token);
                {
                    var s = text;
                    while (s.Length > 4)
                    {
                        s = s.Substring(0, s.Length - 1);
                        if (!voKwIdsS.ContainsKey(s))
                            voKwIdsS.Add(s, token);
                    }
                }
            }

            // These keywords are inserted without abbreviations
            foreach (var text in KeywordsStmt.Keys)
            {
                var token = KeywordsStmt[text];
                xsKwIdsS.Add(text, token);
                voKwIdsS.Add(text, token);
            }

            voKwIdsE = new Dictionary<string, TokenType>(voKwIdsS, StringComparer.OrdinalIgnoreCase);
            xsKwIdsE = new Dictionary<string, TokenType>(xsKwIdsS, StringComparer.OrdinalIgnoreCase);

            // These are predefined abbreviations of some keywords that are also valid in Vulcan
            xsKwIdsE.Add("PROC", TokenType.PROC);
            xsKwIdsE.Add("FUNC", TokenType.FUNC);
            xsKwIdsE.Add("PROTECT", TokenType.PROTECTED);

            // These keywords are abbreviated for VO
            foreach (var text in VoKeywordsEnt.Keys)
            {
                var token = VoKeywordsEnt[text];
                xsKwIdsS.Add(text, token);
                voKwIdsS.Add(text, token);
                {
                    var s = text;
                    while (s.Length > 4)
                    {
                        s = s.Substring(0, s.Length - 1);
                        if (!voKwIdsS.ContainsKey(s))
                            voKwIdsS.Add(s, token);
                    }
                }
            }

            // These keywords are inserted without abbreviations
            foreach (var text in KeywordsEnt.Keys)
            {
                var token = KeywordsEnt[text];
                xsKwIdsS.Add(text, token);
                voKwIdsS.Add(text, token);
            }

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

            symIdsE = new Dictionary<string, TokenType>(symIds, StringComparer.OrdinalIgnoreCase)
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
                { "#XTRANSLATE", TokenType.PP_TRANSLATE},    // #xtranslate <matchPattern> => <resultPattern>  // alias for #translate , no 4 letter abbrev

                { "#USING", TokenType.USING},
                { "#PRAGMA", TokenType.PRAGMA},
            };

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

            softKws = new BitArray((int)TokenType.LAST);

            //vulcan soft KWs
            softKws[(int)TokenType.ABSTRACT] = true;
            softKws[(int)TokenType.ANSI] = true;
            softKws[(int)TokenType.AUTO] = true;
            softKws[(int)TokenType.CHAR] = true;
            softKws[(int)TokenType.CONST] = true;
            softKws[(int)TokenType.DEFAULT] = true;
            softKws[(int)TokenType.EXPLICIT] = true;
            softKws[(int)TokenType.FOREACH] = true;
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
            softKws[(int)TokenType.BY] = true;
            softKws[(int)TokenType.CHECKED] = true;
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
            softKws[(int)TokenType.CHAR] = true;
            softKws[(int)TokenType.MEMVAR] = true;
            softKws[(int)TokenType.PARAMETERS] = true;
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
            softKws[(int)TokenType.THISCALL] = true;
            softKws[(int)TokenType.UNION] = true;
            softKws[(int)TokenType.UNTIL] = true;
            softKws[(int)TokenType.UPTO] = true;
            softKws[(int)TokenType.USING] = true;
            softKws[(int)TokenType.WINCALL] = true;

            for(var i = (int)TokenType.FIRST_POSITIONAL_KEYWORD+1; i < (int)TokenType.LAST_POSITIONAL_KEYWORD; i++)
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

                // Pragma
                v[(int)TokenType.PRAGMA] = "#pragma";

                System.Threading.Interlocked.CompareExchange(ref _tokenText, v, null);
            }
            return _tokenText[(int)token];
        }
    }
}
