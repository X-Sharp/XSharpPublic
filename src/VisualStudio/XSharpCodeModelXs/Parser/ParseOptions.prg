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
using System.Diagnostics
using Microsoft.Win32
BEGIN NAMESPACE XSharpModel

	/// <summary>
    /// The ParseOptions class.
    /// </summary>
    #ifdef DEBUG
    [DebuggerDisplay("{Id} {Dialect} ")];
    CLASS XParseOptions
    #else
    CLASS XParseOptions
    #endif
    public static regKey as STRING
    public static defaultIncludeDir as STRING
    #ifdef DEBUG
    public Id as Int64
    public static nextId := 0 as Int64
    #endif
        private _options AS XSharpParseOptions
        public static PROPERTY Default AS XParseOptions GET XParseOptions{XSharpParseOptions.Default}

        PRIVATE CONSTRUCTOR(options as XSharpParseOptions)
            #ifdef DEBUG
            Id := ++nextId
            #endif
            _options := options
            RETURN

    STATIC METHOD FromVsValues(opt AS IList<string>) AS XParseOptions
        var options  := XSharpParseOptions.FromVsValues(opt)

        RETURN XParseOptions{options}

    OPERATOR EXPLICIT(options AS XSharpParseOptions) AS XParseOptions
        RETURN XParseOptions{options}

    OPERATOR EXPLICIT(options AS XParseOptions) AS XSharpParseOptions
        RETURN options._options

    STATIC PROPERTY DefaultIncludeDir as STRING GET XSharpSpecificCompilationOptions.DefaultIncludeDir
    PROPERTY IncludePaths as IList<String> GET _options:IncludePaths
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
