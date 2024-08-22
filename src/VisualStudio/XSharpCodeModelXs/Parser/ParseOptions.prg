// ParseOptions.prg
// Created by    : robert
// Creation Date : 8/22/2024 11:07:12 AM
// Created for   :
// WorkStation   : LEDA

USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharpModel

	/// <summary>
    /// The ParseOptions class.
    /// </summary>
    CLASS XParseOptions
        private initonly _options := XSharpParseOptions.Default AS XSharpParseOptions
        public static PROPERTY Default AS XParseOptions GET XParseOptions{XSharpParseOptions.Default}

        PRIVATE CONSTRUCTOR(options as XSharpParseOptions)
            _options := options
            RETURN
    STATIC METHOD FromVsValues(opt AS IList<string>) AS XParseOptions
        var options  := XSharpParseOptions.FromVsValues(opt)
        RETURN XParseOptions{options}

    OPERATOR IMPLICIT(options AS XSharpParseOptions) AS XParseOptions
        RETURN XParseOptions{options}

    OPERATOR IMPLICIT(options AS XParseOptions) AS XSharpParseOptions
        RETURN options._options

    PROPERTY SupportsMemvars AS LOGIC GET _options:SupportsMemvars
    PROPERTY XSharpRuntime AS LOGIC GET _options:XSharpRuntime
    PROPERTY VOUntypedAllowed AS LOGIC GET _options:VOUntypedAllowed
    PROPERTY ImplicitNamespace AS LOGIC GET _options:ImplicitNamespace
    PROPERTY HasRuntime AS LOGIC GET _options:HasRuntime
    PROPERTY Dialect as XDialect GET (XDialect) (LONG) _options:Dialect
    PROPERTY DefaultNamespace AS STRING GET _options:DefaultNamespace
    PROPERTY CaseSensitive AS LOGIC GET _options:CaseSensitive
    PROPERTY AllowDotForInstanceMembers AS LOGIC GET _options:AllowDotForInstanceMembers
    PROPERTY EnforceSelf AS LOGIC GET _options:EnforceSelf
	END CLASS
END NAMESPACE // XSharpModel.Parser
