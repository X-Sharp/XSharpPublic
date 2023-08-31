//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
#define FULLCOMPILER
using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
#if !VSPARSER
using static Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax.XSharpLanguageParser;
#endif
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using TokenType = System.Int32;
  
    internal class XSharpPreprocessor
    {
        static Dictionary<string, string> embeddedHeaders = null;

        static void loadResources()
        {
            if (embeddedHeaders == null)
            {
                embeddedHeaders = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                var asm = typeof(XSharpPreprocessor).GetTypeInfo().Assembly;
                string nameSpace;
#if VSPARSER
                nameSpace = "XSharp.VSParser.";
#else
#if MACROCOMPILER
                nameSpace = "XSharp.MacroCompiler.";
#else
                nameSpace = "LanguageService.CodeAnalysis.XSharp.";
#endif
#endif

                var strm = asm.GetManifestResourceStream(nameSpace + "Preprocessor.StandardHeaders.resources");
                if (strm != null)
                {
                    var rdr = new System.Resources.ResourceReader(strm);
                    foreach (DictionaryEntry item in rdr)
                    {
                        embeddedHeaders.Add((string)item.Key, (string)item.Value);
                    }
                }
            }
        }

        const string PPOPrefix = "//PP ";

        #region IncludeCache
        internal class PPIncludeFile
        {
            static readonly ConcurrentDictionary<String, PPIncludeFile> cache = new(StringComparer.OrdinalIgnoreCase);

            internal static void ClearOldIncludes()
            {
                // Remove old includes that have not been used in the last 1 minutes
                var oldkeys = new List<string>();
                var compare = DateTime.Now.Subtract(new TimeSpan(0, 1, 0));
                foreach (var include in cache.Values)
                {
                    if (include.LastUsed < compare)
                    {
                        oldkeys.Add(include.FileName);
                    }
                }
                foreach (var key in oldkeys)
                {
                    cache.TryRemove(key, out var _);
                }
            }

            internal static PPIncludeFile Get(string fileName)
            {
                PPIncludeFile file = null;
                if (cache.ContainsKey(fileName))
                {
                    if (cache.TryGetValue(fileName, out file))
                    {
                        DateTime timeStamp;
                        if (System.IO.File.Exists(fileName))
                            timeStamp = File.GetLastWriteTime(fileName);
                        else
                            timeStamp = DateTime.MinValue;
                        if (file.LastWritten != timeStamp)
                        {
                            cache.TryRemove(fileName, out _);
                            file = null;
                        }
                        else
                        {
                            file.LastUsed = DateTime.Now;
                            // Now clone the file so the tokens may be manipulated
                            file = file.Clone();
                        }
                    }
                }
                return file;
            }
            internal static PPIncludeFile Add(string fileName, IList<IToken> tokens, SourceText text, bool mustBeProcessed, ref bool newFile)
            {
                cache.TryGetValue(fileName, out PPIncludeFile file);
                if (file == null)
                {
                    newFile = true;
                    file = new PPIncludeFile(fileName, tokens, text, mustBeProcessed);
                    if (!cache.TryAdd(fileName, file))
                    {
                        if (cache.TryGetValue(fileName, out PPIncludeFile oldFile))
                        {
                            file = oldFile;
                            newFile = false;
                        }
                    }
                }
                else
                {
                    newFile = false;
                }
                return file;
            }
            internal DateTime LastWritten { get; private set; }
            internal string FileName { get; private set; }
            internal IList<IToken> Tokens { get; private set; }
            internal SourceText Text { get; private set; }
            internal DateTime LastUsed { get; set; }
            internal bool MustBeProcessed { get; set; }

            internal PPIncludeFile(string name, IList<IToken> tokens, SourceText text, bool mustBeProcessed)
            {
                this.FileName = name;
                this.Text = text;
                this.LastUsed = DateTime.Now;
                if (File.Exists(this.FileName))
                {
                    this.LastWritten = File.GetLastWriteTime(this.FileName);
                }
                else
                {
                    this.LastWritten = DateTime.MinValue;
                }
                this.Tokens = tokens.ToArray();
                this.MustBeProcessed = mustBeProcessed;

            }
            internal PPIncludeFile Clone()
            {
                return new PPIncludeFile(FileName, Tokens, Text, MustBeProcessed);
            }
        }

        #endregion

        [DebuggerDisplay("{SourceFileName}")]
        class InputState
        {
            internal ITokenStream Tokens;
            internal int Index;
            internal string SourceFileName;

            internal InputState(ITokenStream tokens, string fileName)
            {
                Tokens = tokens;
                SourceFileName = fileName;
                Index = 0;
            }

            internal TokenType La()
            {
                return Tokens.Get(Index).Type;
            }

            internal XSharpToken Lt()
            {
                return (XSharpToken)Tokens.Get(Index);
            }

            internal bool Eof()
            {
                return Index >= Tokens.Size || Tokens.Get(Index).Type == XSharpLexer.EOF;
            }

            internal bool Consume()
            {
                if (Eof())
                    return false;
                Index++;
                return true;
            }
        }

        readonly XSharpLexer _lexer;
        readonly ITokenStream _lexerStream;
        readonly CSharpParseOptions _options;

        internal CSharpParseOptions Options => _options;

        readonly Encoding _encoding;

        readonly SourceHashAlgorithm _checksumAlgorithm;

        readonly IList<ParseErrorData> _parseErrors;
        bool _duplicateFile = false;

        readonly IList<string> _includeDirs;

        readonly Dictionary<string, IList<XSharpToken>> _symbolDefines;

        readonly Dictionary<string, Func<XSharpToken, XSharpToken>> _macroDefines = new(StringComparer.OrdinalIgnoreCase);

        readonly Stack<bool> _defStates = new();
        readonly Stack<XSharpToken> _regions = new();
        TextProperties _textProps;
        readonly string _fileName = null;
        InputState inputs;
        readonly Stack<InputState> _files = new Stack<InputState>();
        IToken lastToken;
        readonly PPRuleDictionary _cmdRules = new();
        readonly PPRuleDictionary _transRules = new();
        bool _hasCommandrules = false;
        bool _hasTransrules = false;
        int _rulesApplied = 0;
#if FULLCOMPILER
        readonly int _defsApplied = 0;
        readonly bool _preprocessorOutput = false;
        StreamWriter _ppoWriter;
#endif
        internal Dictionary<string, SourceText> IncludedFiles = new(CaseInsensitiveComparison.Comparer);

        public int MaxIncludeDepth { get; set; } = 16;

        //public int MaxSymbolDepth { get; set; } = 16;

        public int MaxUDCDepth { get; set; } = 256;
        bool ProcessingIncludeFile => _files.Count > 0;
        public string StdDefs { get; set; } = string.Empty;
        internal List<PragmaBase> Pragmas;


        void addMacro(string macro, TokenType type)
        {
            addMacro(macro, type, macro);
        }
        void addMacro(string macro, TokenType type, string value)
        {
            if (type == XSharpLexer.STRING_CONST)
                value = '"' + value + '"';
            _macroDefines.Add(macro, (token) => new XSharpPPToken(type, value, token));
        }

        private void initStdDefines(string fileName)
        {
            // Note Macros such as __ENTITY__ and  __SIG__ are handled in the transformation phase
            // Make sure you also update the MACROs in XSharpLexerCode.cs !
            addMacro("__ARRAYBASE__", XSharpLexer.INT_CONST, _options.ArrayZero ? "0" : "1");
#if FULLCOMPILER
            if (_options.ClrVersion == 2)
                addMacro("__CLR2__", XSharpLexer.TRUE_CONST);
            if (_options.ClrVersion == 4)
                addMacro("__CLR4__", XSharpLexer.TRUE_CONST);
            addMacro("__CLRVERSION__", XSharpLexer.STRING_CONST, _options.ClrVersion.ToString() + ".0");
#endif
            addMacro("__DATE__", XSharpLexer.STRING_CONST, DateTime.Now.Date.ToString("yyyyMMdd"));
            addMacro("__DATETIME__", XSharpLexer.STRING_CONST, DateTime.Now.ToString());
            bool debug = false;
#if FULLCOMPILER
#if VSPARSER
            if (_options.PreprocessorSymbolsUpper.Contains("DEBUG"))
                debug = true;
            if (_options.PreprocessorSymbolsUpper.Contains("NDEBUG"))
                debug = false;
#else
            if (_options.PreprocessorSymbolNames.Contains((name) => name.ToUpper() == "DEBUG"))
                debug = true;
            if (_options.PreprocessorSymbolNames.Contains((name) => name.ToUpper() == "NDEBUG"))
                debug = false;
#endif
#endif

            if (debug)
            {
                addMacro("__DEBUG__", XSharpLexer.TRUE_CONST);
            }
            addMacro("__DIALECT__", XSharpLexer.STRING_CONST, _options.Dialect.ToString());
            switch (_options.Dialect)
            {
                case XSharpDialect.Core:
                    addMacro("__DIALECT_CORE__", XSharpLexer.TRUE_CONST);
                    break;
                case XSharpDialect.VO:
                    addMacro("__DIALECT_VO__", XSharpLexer.TRUE_CONST);
                    addMacro("__VO__", XSharpLexer.TRUE_CONST);
                    break;
                case XSharpDialect.Vulcan:
                    addMacro("__DIALECT_VULCAN__", XSharpLexer.TRUE_CONST);
                    addMacro("__VULCAN__", XSharpLexer.TRUE_CONST);
                    break;
                case XSharpDialect.Harbour:
                    addMacro("__DIALECT_HARBOUR__", XSharpLexer.TRUE_CONST);
                    // Harbour always includes hbver.h . The macro is defined in that file.
                    //addMacro("__HARBOUR__", XSharpLexer.TRUE_CONST);
                    break;
                case XSharpDialect.XPP:
                    addMacro("__DIALECT_XBASEPP__", XSharpLexer.TRUE_CONST);
                    addMacro("__XPP__", XSharpLexer.TRUE_CONST);
                    break;
                case XSharpDialect.FoxPro:
                    addMacro("__DIALECT_FOXPRO__", XSharpLexer.TRUE_CONST);
                    break;
                default:
                    break;
            }
            addMacro("__ENTITY__", XSharpLexer.STRING_CONST);
            addMacro("__FILE__", XSharpLexer.STRING_CONST, (inputs.SourceFileName ?? fileName));
            addMacro("__FUNCTION__", XSharpLexer.STRING_CONST);
            addMacro("__FUNCTIONS__", XSharpLexer.STRING_CONST);
            addMacro("__MODULE__", XSharpLexer.STRING_CONST, (inputs.SourceFileName ?? fileName));
            addMacro("__SIG__", XSharpLexer.STRING_CONST);
            addMacro("__TIME__", XSharpLexer.STRING_CONST, DateTime.Now.ToString("HH:mm:ss"));
            addMacro("__UTCTIME__", XSharpLexer.STRING_CONST, DateTime.Now.ToUniversalTime().ToString("HH:mm:ss"));
            addMacro("__VERSION__", XSharpLexer.STRING_CONST, global::XSharp.Constants.FileVersion);
            addMacro("__XSHARP__", XSharpLexer.TRUE_CONST);

            _macroDefines.Add("__LINE__", (token) => new XSharpPPToken(XSharpLexer.INT_CONST, token.Line.ToString(), token));
            _macroDefines.Add("__SRCLOC__", (token) => new XSharpPPToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + " line " + token.Line.ToString() + '"', token));

#if MACROCOMPILER
            addMacro("__XSHARP_RT__", XSharpLexer.TRUE_CONST);
#else
            if (_options.XSharpRuntime)
            {
                addMacro("__XSHARP_RT__", XSharpLexer.TRUE_CONST);
            }
            addMacro("__SYSDIR__", XSharpLexer.STRING_CONST, _options.SystemDir);
            addMacro("__WINDIR__", XSharpLexer.STRING_CONST, _options.WindowsDir);
            addMacro("__WINDRIVE__", XSharpLexer.STRING_CONST, _options.WindowsDir?.Substring(0, 2));
            var options = new string[] { "__VO1__", "__VO2__", "__VO3__", "__VO4__", "__VO5__", "__VO6__", "__VO7__", "__VO8__", "__VO9__", "__VO10__",
                                        "__VO11__","__VO12__","__VO13__","__VO14__","__VO15__","__VO16__","__VO17__","__XPP1__", "__FOX1__","__FOX2__",
                                        "__MEMVAR__","__UNDECLARED__"};
            foreach (var option in options)
            {
                _macroDefines.Add(option, (token) => new XSharpPPToken(_options.HasOption(option.Replace("_", ""), token, null) ? XSharpLexer.TRUE_CONST : XSharpLexer.FALSE_CONST, option, token));
            }

            if (!_options.NoStdDef || !String.IsNullOrEmpty(_options.StdDefs))
            {
                // when the compiler option nostddefs is not set:
                // read XSharpDefs.xh from the XSharp Include folder,
                // unless they have overridden the standard header file
                // and automatically include it.
                // read XsharpDefs.xh
                StdDefs = _options.StdDefs;
                if (string.IsNullOrWhiteSpace(StdDefs))
                {
                    StdDefs = global::XSharp.Constants.StandardHeaderFile;
                }
                ProcessIncludeFile(StdDefs, null);
            }
#endif
        }
#if MACROCOMPILER
        partial void writeToPPO(string text, bool addCRLF = false);
        partial void writeToPPO(IList<XSharpToken> tokens, bool addCRLF = false, bool ignore = false);

        internal void Error(string fileName, ErrorCode code, params object[] args)
        {
            if (!ErrorString.IsWarning(code))
                throw Compilation.Error(new SourceLocation() { FileName = fileName }, code, args);
        }

        internal void Error(Token token, ErrorCode code, params object[] args)
        {
            if (!ErrorString.IsWarning(code))
                throw token.Error(code, args);
        }
#endif
#if FULLCOMPILER
        internal void DumpStats()
        {
            DebugOutput("Preprocessor statistics");
            DebugOutput("-----------------------");
            DebugOutput("# of #defines    : {0}", _symbolDefines.Count);
            DebugOutput("# of #translates : {0}", _transRules.Count);
            DebugOutput("# of #commands   : {0}", _cmdRules.Count);
            DebugOutput("# of macros      : {0}", _macroDefines.Count);
            DebugOutput("# of defines used: {0}", _defsApplied);
            DebugOutput("# of UDCs used   : {0}", _rulesApplied);
        }

        private void _writeToPPO(string text, bool addCRLF)
        {
            var write = _preprocessorOutput;
            if (write)
            {
                // when we are in a header file then do not output
                // comment lines and blank lines
                if (ProcessingIncludeFile)
                {
                    var temp = text.Trim();
                    if (temp.Length == 0)
                        write = false;
                    else if (temp.StartsWith("//"))
                    {
                        write = false;
                    }
                }
            }
            if (write)
            {
                _ppoWriter.Write(text);
                if (addCRLF)
                {
                    _ppoWriter.WriteLine();
                }
            }
        }

        private bool mustWriteToPPO()
        {
            return _preprocessorOutput && _ppoWriter != null;
        }

        internal void writeToPPO(string text, bool addCRLF = true)
        {
            if (mustWriteToPPO())
            {
                _writeToPPO(text, addCRLF);
            }
        }
        private void writeToPPO(IList<XSharpToken> tokens, bool prefix = false, bool prefixNewLines = false)
        {
            if (mustWriteToPPO())
            {
                if (tokens?.Count == 0)
                {
                    _writeToPPO("", true);
                    return;
                }
                // We cannot use the interval and fetch the text from the source stream,
                // because some tokens may come out of an include file or otherwise
                // so concatenate text on the fly
                var bld = new StringBuilder(1024);
                if (prefix)
                {
                    bld.Append(PPOPrefix);
                }
                bool first = !prefix;
                foreach (var t in tokens)
                {
                    // Exclude comments from header files
                    if (t.FromInclude && t.IsComment())
                        continue;
                    // Copy the trivia from the original first symbol on the line so the UDC has the proper indent level
                    if (first && t.SourceSymbol != null && t.SourceSymbol.HasTrivia && t.SourceSymbol.Type == XSharpLexer.UDC_KEYWORD)
                    {
                        bld.Append(t.SourceSymbol.TriviaAsText);
                    }
                    bld.Append(t.Text);
                    first = false;
                }
                if (prefixNewLines)
                {
                    bld.Replace("\n", "\n" + PPOPrefix);
                }
                _writeToPPO(bld.ToString(), true);
            }
        }

        internal void Close()
        {
            if (_ppoWriter != null)
            {
                _ppoWriter.Flush();
                _ppoWriter.Close();
            }
            _ppoWriter = null;
        }
        internal void Error(XSharpToken token, ErrorCode error, params object[] args)
        {
            addParseError(new ParseErrorData(token, error, args));
        }

        private void addParseError(ParseErrorData error)
        {
#if !VSPARSER
            // use the filter mechanism in the compiler to only add errors that are not suppressed.
            var d = Diagnostic.Create(new SyntaxDiagnosticInfo(error.Code));
            if (_options.CommandLineArguments != null)
            {
                d = _options.CommandLineArguments.CompilationOptions.FilterDiagnostic(d, CancellationToken.None);
            }
            if (d != null)
            {
                _parseErrors.Add(error);
            }
#else
            _parseErrors.Add(error);
#endif
        }
#endif
        internal XSharpPreprocessor(XSharpLexer lexer, ITokenStream lexerStream, CSharpParseOptions options, string fileName, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            PPIncludeFile.ClearOldIncludes();
            Pragmas = new List<PragmaBase>();

            _lexer = lexer;
            _lexerStream = lexerStream;
            _options = options;
            _fileName = fileName;
            if (_options.VOPreprocessorBehaviour)
            {
                // with /vo8 or /vo8+  the comparison rules for defines will match the /cs rules!
                _symbolDefines = new Dictionary<string, IList<XSharpToken>>(XSharpString.Comparer);
            }
            else
            {
                // without /vo8 or with /vo8- the defines are always case sensitive
                _symbolDefines = new Dictionary<string, IList<XSharpToken>>(/* case sensitive */);
            }
            _encoding = encoding;
            _checksumAlgorithm = checksumAlgorithm;
            _parseErrors = parseErrors;
            _includeDirs = new List<string>(options.IncludePaths);
            if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName))
            {
                _includeDirs.Add(Path.GetDirectoryName(fileName));
#if FULLCOMPILER
                var ppoFile = FileNameUtilities.ChangeExtension(fileName, ".ppo");
                try
                {
                    _ppoWriter = null;
                    _preprocessorOutput = _options.PreprocessorOutput;
                    if (FileNameUtilities.GetExtension(fileName).ToLower() == ".ppo")
                    {
                        _preprocessorOutput = false;
                    }
                    else
                    {
                        if (_preprocessorOutput)
                        {
                            var ppoStream = new FileStream(ppoFile, FileMode.Create, FileAccess.Write, FileShare.None);
                            _ppoWriter = new StreamWriter(ppoStream, this._encoding);
                        }
#if !VSPARSER
                        else if (File.Exists(ppoFile))
                        {
                            File.Delete(ppoFile);
                        }
#endif
                    }
                }
                catch (Exception e)
                {
                    var token = new XSharpToken(0);
                    token.Text = fileName;
                    Error(token, ErrorCode.ERR_PreProcessorError, "Error processing PPO file: " + e.Message);
                }
#endif
            }
            // Add default IncludeDirs;
            if (!string.IsNullOrEmpty(options.DefaultIncludeDir))
            {
                string[] paths = options.DefaultIncludeDir.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var path in paths)
                {
                    _includeDirs.Add(path);
                }
            }

            inputs = new InputState(lexerStream, fileName);
            // Add defines from the command line.
            if (options.PreprocessorSymbols != null)
            {
                foreach (var symbol in options.PreprocessorSymbols)
                {
                    var tokens = new List<XSharpToken>();
                    _symbolDefines[symbol] = tokens;
                }
            }

            initStdDefines(fileName);
        }

        internal void DebugOutput(string format, params object[] objects)
        {
#if FULLCOMPILER
            if (_options.ConsoleOutput != null)
            {
                _options.ConsoleOutput.WriteLine("PP: " + format, objects);
            }
#endif
        }

        internal void StdOut(string text)
        {
#if FULLCOMPILER
            if (_options.ConsoleOutput != null)
            {
                _options.ConsoleOutput.WriteLine(text);
            }
#endif
        }


        /// <summary>
        /// Pre-processes the input stream. Reads #Include files, processes #ifdef commands and translations from #defines, macros and UDCs
        /// </summary>
        /// <returns>Translated input stream</returns>
        internal IList<IToken> PreProcess()
        {
            var result = new List<IToken>();
            XSharpToken t = Lt();
            List<XSharpToken> omitted = new List<XSharpToken>();
            while (t.Type != XSharpLexer.EOF)
            {
                // read until the next EOS
                var line = ReadLine(omitted);
                t = Lt();   // CRLF or EOS. Must consume now, because #include may otherwise add a new inputs
                Consume();
                if (line.Count > 0)
                {
                    line = ProcessLine(line, this._preprocessorOutput);
                    if (line != null && line.Count > 0)
                    {
                        result.AddRange(line.Where(t => !t.FromInclude || !t.IsComment()));
                    }
                }
                else
                {
                    if (omitted.Count > 0)
                        writeToPPO(omitted, false);
                    else
                        writeToPPO("");
                }
                if (t.Channel == Channel.Default && line != null && line.Count > 0)
                    result.Add(t);
            }
            doEOFChecks();
            return result;
        }

        static TokenType getFirstTokenType(IList<XSharpToken> line)
        {
            for (int i = 0; i < line.Count; i++)
            {
                switch (line[i].Channel)
                {
                    case Channel.Default:
                    case Channel.PreProcessor:
                        return line[i].Type;
                    default:
                        break;
                }
            }
            return XSharpLexer.EOF;
        }
        IList<XSharpToken> ProcessLine(IList<XSharpToken> line, bool write2ppo)
        {
            Debug.Assert(line.Count > 0);
            var nextType = getFirstTokenType(line);
            switch (nextType)
            {
                case XSharpLexer.PP_UNDEF:
                    doUnDefDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_IF:
                    doIfDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_IFDEF:
                    doIfDefDirective(line, true);
                    line = null;
                    break;
                case XSharpLexer.PP_IFNDEF:
                    doIfDefDirective(line, false);
                    line = null;
                    break;
                case XSharpLexer.PP_ENDIF:
                    doEndifDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ELSE:
                    doElseDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_LINE:
                    doLineDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ERROR:
                case XSharpLexer.PP_WARNING:
                case XSharpLexer.PP_STDOUT:
                    doErrorWarningDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_INCLUDE:
                    doIncludeDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_COMMAND:
                case XSharpLexer.PP_TRANSLATE:
                    doUDCDirective(line, true);
                    line = null;
                    break;
                case XSharpLexer.PP_DEFINE:
                    doDefineDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ENDREGION:
                    doEndRegionDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_REGION:
                    doRegionDirective(line);
                    line = null;
                    break;
                case XSharpLexer.UDCSEP:
                    doUnexpectedUDCSeparator(line);
                    line = null;
                    break;
                case XSharpLexer.PP_TEXT:
                    line = doTextDirective(line, write2ppo);
                    break;
                case XSharpLexer.PP_ENDTEXT:
                    line = doEndTextDirective(line, write2ppo);
                    break;
                case XSharpLexer.PP_PRAGMA:
                    doPragmaDirective(line, write2ppo);
                    line = null;
                    break;
                default:
                    if (_textProps != null && line.Count > 0)
                    {
                        var sb = new StringBuilder();
                        foreach (var token in line)
                        {
                            if (token.Type != XSharpLexer.WS)
                            {
                                sb.Append(token.Text);
                            }
                        }
                        var sLine = sb.ToString().Trim();
                        if (sLine.ToUpper().StartsWith("ENDTEXT"))
                        {
                            var temp = stripWs(line);
                            line = doNormalLine(temp, write2ppo);
                            _textProps = null;
                        }
                        else
                        {
                            line = doTextLine(line, write2ppo);
                        }
                    }
                    else
                    {
                        line = doNormalLine(line, write2ppo);
                    }
                    break;
            }
            return line;
        }

        /// <summary>
        /// Reads the a line from the input stream until the EOS token and skips hidden tokens
        /// </summary>
        /// <returns>List of tokens EXCLUDING the EOS but including statement separator char ;</returns>
        IList<XSharpToken> ReadLine(IList<XSharpToken> omitted)
        {
            Debug.Assert(omitted != null);
            var res = new List<XSharpToken>();
            omitted.Clear();
            XSharpToken t = Lt();
            while (t.Type != XSharpLexer.EOF)
            {
                if (t.IsEOS() && t.Text != ";")
                    break;
                if (t.Channel != Channel.Hidden && t.Channel != Channel.XmlDoc)
                {
                    var nt = FixToken(t);
                    res.Add(nt);
                }
                else
                {
                    res.Add(t);
                }
                Consume();
                t = Lt();
            }
            return res;
        }

        /// <summary>
        /// Returns the name of the active source. Can be the main prg file, but also an active #include file
        /// </summary>
        string SourceName
        {
            get
            {
                return _fileName;
            }
        }


        XSharpToken FixToken(XSharpToken token)
        {
            return token;
        }

        void checkForEof()
        {

            if (inputs.Eof())
            {
                while (inputs.Eof() && _files.Count > 0)
                {
                    inputs = _files.Pop();
                    var token = inputs.Lt();
                    _writeToPPO($"#line {token.Line} \"{token.SourceName}\"", true);
                }
            }
        }

        XSharpToken Lt()
        {
            checkForEof();
            return inputs.Lt();
        }

        void Consume()
        {
            while (!inputs.Consume())
            {
                checkForEof();
                if (inputs.Eof())
                    break;
            }
        }

        void InsertStream(string filename, ITokenStream input, XSharpToken symbol)
        {
#if FULLCOMPILER
            if (_options.ShowDefs)
            {
                if (symbol != null)
                {
                    var tokens = new List<XSharpToken>();
                    for (int i = 0; i < input.Size - 1; i++)
                    {
                        tokens.Add(new XSharpToken(input.Get(i)));
                    }
                    string text = tokens.AsString();
                    if (text.Length > 30)
                        text = text.Substring(0, 30) + "...";
                    DebugOutput("File {0} line {1}:", _fileName, symbol.Line);
                    DebugOutput("Input stack: Insert value of token Symbol {0}, {1} tokens => {2}", symbol.Text, input.Size - 1, text);
                }
                else
                    DebugOutput("Input stack: Insert Stream {0}, # of tokens {1}", filename, input.Size - 1);
            }
#endif
            // Detect recursion
            foreach (var x in _files)
            {
                if (string.Compare(x.SourceFileName, filename, true) == 0 || string.Compare(this._fileName, filename, true) == 0)
                {
                    Error(symbol, ErrorCode.ERR_PreProcessorError, "Recursive include file (" + filename + ") detected", filename);
                    return;
                }
            }
            InputState s = new InputState(input, filename)
            {
                //SymbolName = symbol?.Text,
                //Symbol = symbol,
                //isSymbol = symbol != null
            };
            _files.Push(inputs);
            inputs = s;
        }

        bool IsActive()
        {
            return _defStates.Count == 0 || _defStates.Peek();
        }


        bool IsDefinedMacro(XSharpToken t)
        {
            return (t.Type == XSharpLexer.MACRO) ? _macroDefines.ContainsKey(t.Text) : false;
        }
        static IList<XSharpToken> stripWs(IList<XSharpToken> line)
        {
            IList<XSharpToken> result = new List<XSharpToken>();
            foreach (var token in line)
            {
                if (token.Channel == Channel.Default || token.Channel == Channel.PreProcessor)
                {
                    result.Add(token);
                }
            }
            return result;
        }

        void addDefine(IList<XSharpToken> line, IList<XSharpToken> original)
        {
            // Check to see if the define contains a LPAREN, and there is no space in between them.
            // Then it is a pseudo function that we will store as a #xtranslate UDC
            // this returns a list that includes #define and the ID
            if (line.Count < 2)
            {
                var token = line[0];
                Error(token, ErrorCode.ERR_PreProcessorError, "Identifier expected");
                return;
            }
            // token 1 is the Identifier
            // other tokens are optional and may contain a value
            XSharpToken def = line[1];
            if (line.Count > 2)
            {
                var first = line[2];
                if (first.Type == XSharpLexer.LPAREN
                    && first.Start == def.End)
                {
                    doUDCDirective(original, false);
                    return;
                }
            }
            if (def.IsIdentifier() || def.IsKeyword())
            {
                line.RemoveAt(0);  // remove #define
                line.RemoveAt(0);  // remove ID
                if (_symbolDefines.ContainsKey(def.Text))
                {
                    // check to see if this is a new definition or a duplicate definition
                    var oldtokens = _symbolDefines[def.Text];
                    var cOld = oldtokens.AsString();
                    var cNew = line.AsString();
                    def = new XSharpPPToken(def, null);
                    if (cOld == cNew && oldtokens?.Count > 0)
                    {
                        // check to see if the same file has been added twice
                        var oldToken = oldtokens[0];
                        var newToken = line[0];
                        if (oldToken.TokenSource.SourceName != newToken.TokenSource.SourceName || oldToken.Line != newToken.Line)
                        {
                            Error(def, ErrorCode.WRN_DuplicateDefineSame, def.Text);
                        }
                        else
                        {
                            if (!_duplicateFile)
                            {
                                _duplicateFile = true;
                                var includeName = newToken.SourceName;
                                var defText = def.Text;
                                Error(def, ErrorCode.WRN_PreProcessorWarning, "Duplicate define (" + defText + ") found because include file \"" + includeName + "\" was included twice");
                            }
                        }
                    }
                    else
                    {
                        Error(def, ErrorCode.WRN_DuplicateDefineDiff, def.Text, cOld, cNew);
                    }
                }
                _symbolDefines[def.Text] = line;
#if FULLCOMPILER
                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add DEFINE {2} => {3}", def.FileName(), def.Line, def.Text, line.AsString());
                }
#endif
            }
            else
            {
                Error(def, ErrorCode.ERR_PreProcessorError, "Identifier expected");
                return;
            }
        }

        void removeDefine(IList<XSharpToken> line)
        {
            var errToken = line[0];
            bool ok = true;
            line = stripWs(line);
            if (line.Count < 2)
            {
                ok = false;
            }
            XSharpToken def = line[1];
            if (def.IsIdentifier() || def.IsKeyword())
            {
                if (_symbolDefines.ContainsKey(def.Text))
                    _symbolDefines.Remove(def.Text);
            }
            else
            {
                errToken = def;
                ok = false;
            }
            if (!ok)
            {
                Error(errToken, ErrorCode.ERR_PreProcessorError, "Identifier expected");
            }
        }

        void doUDCDirective(IList<XSharpToken> udc, bool mustWrite)
        {
            Debug.Assert(udc?.Count > 0);
            if (!IsActive())
            {
                writeToPPO("");
                return;
            }
            if (mustWrite)
                writeToPPO(udc, true, true);
            udc = stripWs(udc);
            if (udc.Count < 3)
            {
                Error(udc[0], ErrorCode.ERR_PreProcessorError, "Invalid UDC: '" + udc.AsString() + "'");
                return;
            }
            var cmd = udc[0];
            var rule = new PPRule(cmd, udc, out var errorMsgs, _options);
            if (rule.Type == PPUDCType.None)
            {
                if (errorMsgs.Count > 0)
                {
                    foreach (var s in errorMsgs)
                    {
                        Error(s.Token, ErrorCode.ERR_PreProcessorError, s.Message);
                    }
                }
                else
                {
                    Error(cmd, ErrorCode.ERR_PreProcessorError, "Invalid directive '" + cmd.Text + "' (are you missing the => operator?)");
                }
            }
            else
            {
                if (cmd.Type == XSharpLexer.PP_COMMAND)
                {
                    // COMMAND and XCOMMAND can only match from beginning of line
                    _cmdRules.Add(rule);
                    _hasCommandrules = true;
                }
                else
                {
                    // TRANSLATE and XTRANSLATE can also match from beginning of line
                    _transRules.Add(rule);
                    _hasTransrules = true;
                    if (cmd.Type == XSharpLexer.PP_DEFINE && _options.VOPreprocessorBehaviour)
                    {
                        rule._flags |= PPRuleFlags.VOPreprocessorBehaviour;

                    }
                }
#if FULLCOMPILER
                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add {2} {3}", cmd.FileName(), cmd.Line, cmd.Type == XSharpLexer.PP_DEFINE ? "DEFINE" : "UDC", rule.Name);
                }
#endif
            }
        }
#if !VSPARSER
        private Exception readFileContents(string fp, out string nfp, out SourceText text)
        {
            Exception ex = null;
            nfp = null;
            text = null;
            try
            {
                using (var data = new FileStream(fp, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, bufferSize: 1, options: FileOptions.None))
                {
                    nfp = data.Name;
                    try
                    {
                        text = EncodedStringText.Create(data, _encoding, _checksumAlgorithm);
                    }
                    catch (Exception)
                    {
                        text = null;
                    }
                    if (text == null)
                    {
                        // Encoding problem ?
                        text = EncodedStringText.Create(data);
                    }
                }
            }
            catch (Exception e)
            {
                nfp = null;
                ex = e;
            }
            return ex;
        }
#endif
        private bool isObsoleteIncludeFile(string includeFileName, XSharpToken token)
        {
            string file = Path.GetFileName(includeFileName).ToLower();
            bool obsolete = false;
            string assemblyName = "";
            bool sdkdefs = _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.SdkDefines);
            switch (file)
            {
                case "errorcodes.vh":
                case "set.vh":
                case "set.ch":
                case "directry.ch":
                case "vosystemlibrary.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.XSharpCore;
                    break;
                case "voguiclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoGui);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoGui;
                    break;
                case "vointernetclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoInet);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoInet;
                    break;
                case "vorddclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoRdd);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoRdd;
                    break;
                case "voreportclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoReport);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoReport;
                    break;
                case "vosqlclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoSql);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoSql;
                    break;
                case "vosystemclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoSystem);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoSystem;
                    break;
                case "vowin32apilibrary.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoWin32);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoWin32;
                    break;
                case "class.ch":
                    obsolete = _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpXPP);
                    assemblyName = XSharpAssemblyNames.XSharpXPP;
                    break;
                default:
                    obsolete = false;
                    break;
            }
            if (obsolete)
            {
                Error(token, ErrorCode.WRN_ObsoleteInclude, includeFileName, assemblyName + ".dll");
            }
            return obsolete;
        }

        private bool ProcessIncludeFile(string includeFileName, XSharpToken fileNameToken)
        {
            string resolvedIncludeFileName = null;
            SourceText text = null;
            Exception fileReadException = null;
            PPIncludeFile includeFile = null;
            if (isObsoleteIncludeFile(includeFileName, fileNameToken))
            {
                return true;
            }
            List<string> dirs = new List<string>() { Path.GetDirectoryName(_fileName) };
            foreach (var p in _includeDirs)
            {
                if (!dirs.Contains(p))
                    dirs.Add(p);
            }
            if (fileNameToken != null)
            {
                var path = Path.GetDirectoryName(fileNameToken.SourceName);
                if (!string.IsNullOrEmpty(path) && !dirs.Contains(path))
                    dirs.Add(path);
            }
#if FULLCOMPILER
            if (_options.Verbose)
            {
                DebugOutput("Process include file: {0}", includeFileName);
            }
#endif
            foreach (var p in dirs)
            {
                bool rooted = Path.IsPathRooted(includeFileName);
                string fullPath;
                try
                {
                    fullPath = rooted || p == null ? includeFileName : Path.Combine(p, includeFileName);
                }
                catch (Exception e)
                {
                    Error(fileNameToken, ErrorCode.ERR_PreProcessorError, "Error combining path " + p + " and filename  " + includeFileName + " " + e.Message);
                    continue;
                }
                if (File.Exists(fullPath))
                {
#if FULLCOMPILER
                    if (_options.Verbose)
                    {
                        DebugOutput("Found include file on disk: {0}", fullPath);
                    }
#endif
                    // get it from the cache, maybe we have read it before.
                    includeFile = PPIncludeFile.Get(fullPath);
                    if (includeFile != null)
                    {
                        resolvedIncludeFileName = fullPath;
                        text = includeFile.Text;
#if FULLCOMPILER
                        if (_options.Verbose)
                        {
                            DebugOutput("Include file retrieved from cache: {0}", fullPath);
                        }
#endif
                        break;
                    }
                    else
                    {
#if !VSPARSER
                        var ex = readFileContents(fullPath, out resolvedIncludeFileName, out text);
                        if (ex != null && fileReadException == null)
                        {
                            fileReadException = ex;
                        }
#else
                        Exception ex;
                        try
                        {
                            var contents = System.IO.File.ReadAllText(fullPath);
                            text = SourceText.From(contents);
                            resolvedIncludeFileName = fullPath;
                        }
                        catch (Exception e)
                        {
                            ex = e;
                        }
#endif
                    }
                    if (rooted || resolvedIncludeFileName != null)
                        break;

                }
            }
            if (resolvedIncludeFileName == null)
            {
                loadResources();
                var baseName = Path.GetFileNameWithoutExtension(includeFileName).ToLower();
                if (embeddedHeaders.TryGetValue(baseName, out var source))
                {
                    text = SourceText.From(source);
                    resolvedIncludeFileName = includeFileName;
                }
            }
            if (resolvedIncludeFileName == null)
            {
                if (fileReadException != null)
                {
                    Error(fileNameToken, ErrorCode.ERR_PreProcessorError, "Error Reading include file '" + includeFileName + "': " + fileReadException.Message);
                }
                else
                {
                    Error(fileNameToken, ErrorCode.ERR_PreProcessorError, "Include file not found: '" + includeFileName + "'");
                }

                return false;
            }
            else
            {
                _writeToPPO("#line 1 \"" + resolvedIncludeFileName + "\"", true);
                if (!IncludedFiles.ContainsKey(resolvedIncludeFileName))
                {
                    IncludedFiles.Add(resolvedIncludeFileName, text);
                }
            }
#if FULLCOMPILER
            if (_options.ShowIncludes)
            {
                var fname = Path.GetFileName(this.SourceName);
                if (fileNameToken != null)
                {
                    fname = Path.GetFileName(fileNameToken.InputStream.SourceName);
                    DebugOutput("{0} line {1} Include {2}", fname, fileNameToken.Line, resolvedIncludeFileName);
                }
                else
                {
                    // Most likely the Standard Header file.
                    DebugOutput("{0} Include Standard Header file {1}", fname, resolvedIncludeFileName);
                }
            }
#endif

            if (includeFile == null)
            {
                // we have nfp and text with the file contents
                // now parse the stuff and insert in the cache
                var startTime = DateTime.Now;

                var stream = new AntlrInputStream(text.ToString()) { name = resolvedIncludeFileName };
                var lexer = new XSharpLexer(stream)
                {
                    TokenFactory = XSharpTokenFactory.Instance,
                    Options = _options
                };
                var ct = new CommonTokenStream(lexer);
                ct.Fill();
                foreach (var e in lexer.LexErrors)
                {
                    addParseError(e);
                }
#if FULLCOMPILER
                if (_options.Verbose)
                {
                    var endTime = DateTime.Now;
                    DebugOutput("Lexed include file {0} milliseconds", (endTime - startTime).Milliseconds);
                }
#endif
                bool newFile = false;
                var tokens = ct.GetTokens();
                foreach (XSharpToken token in tokens)
                {
                    token.FromInclude = true;
                }
                includeFile = PPIncludeFile.Add(resolvedIncludeFileName, tokens, text, lexer.MustBeProcessed, ref newFile);
#if FULLCOMPILER
                if (_options.Verbose)
                {
                    if (newFile)
                    {
                        DebugOutput("Added include file to cache {0}", resolvedIncludeFileName);
                    }
                    else
                    {
                        DebugOutput("Bummer: include file was already added to cache by another thread {0}", resolvedIncludeFileName);
                    }
                }
#endif
            }
            resolvedIncludeFileName = includeFile.FileName;
            if (_options.ParseLevel == ParseLevel.Complete || includeFile.MustBeProcessed || _lexer.HasPPIfdefs)
            {
                // Clone the array because we may be changing things, changing the channel for example
                // This does not only clone the array but also clones every token.
                var clone = includeFile.Tokens.CloneArray();
                var tokenSource = new XSharpListTokenSource(_lexer, clone, resolvedIncludeFileName);
                var tokenStream = new BufferedTokenStream(tokenSource);
                tokenStream.Fill();
                InsertStream(resolvedIncludeFileName, tokenStream, fileNameToken);
            }
#if FULLCOMPILER
            else
            {
                if (_options.Verbose)
                {
                    DebugOutput("Skipping Include File in Parse Mode {0} line {1}:", resolvedIncludeFileName, fileNameToken.Line);
                }
            }
#endif
            return true;

        }


        private bool IsDefined(string define, XSharpToken token)
        {
            // Handle /VO8 compiler option:
            // When /VO8 is active and the variable is defined and has a value of FALSE or a numeric value = 0
            // Then #ifdef is FALSE
            // otherwise #ifdef is TRUE
            // and when there is more than one token, then #ifdef is also TRUE
            bool isdefined = _symbolDefines.ContainsKey(define);
            if (isdefined)
            {
                if (_options.VOPreprocessorBehaviour)
                {
                    // With VO Compatible preprocessor behavior we return the logical value
                    // so a #define FOO FALSE will not match #ifdef
                    var value = _symbolDefines[define];
                    if (value?.Count == 1)
                    {
                        var deftoken = value[0];
                        if (deftoken.Type == XSharpLexer.FALSE_CONST)
                        {
                            isdefined = false;
                        }
                        else if (deftoken.Type == XSharpLexer.INT_CONST)
                        {
                            isdefined = Convert.ToInt64(deftoken.Text) != 0;
                        }
                    }
                }
            }
            else
            {
                isdefined = _macroDefines.ContainsKey(define);
                if (isdefined)
                {
                    // With VO Compatible preprocessor behavior we return the logical value
                    // so a #define FOO FALSE will not match #ifdef
                    if (_options.VOPreprocessorBehaviour)
                    {
                        var value = _macroDefines[define](token);
                        if (value != null)
                        {
                            if (value.Type == XSharpLexer.FALSE_CONST)
                            {
                                isdefined = false;
                            }
                            else if (value.Type == XSharpLexer.INT_CONST)
                            {
                                isdefined = Convert.ToInt64(value.Text) != 0;
                            }
                        }
                    }
                }
            }
            return isdefined;
        }

        private bool isDefineAllowed(IList<XSharpToken> line, int iPos)
        {
            // DEFINE will not be accepted immediately after or before a DOT
            // So this will not be recognized:
            // #define Console
            // System.Console.WriteLine("zxc")
            // But this will , since there are spaces around the token
            // System. Console .WriteLine("zxc")
            Debug.Assert(line?.Count > 0);
            if (iPos > 0 && line[iPos - 1].Type == XSharpLexer.DOT)
            {
                return false;
            }
            // The only tokens that may match a define are IDs and Keywords
            // Whitespace, operators and literals will never match a define
            XSharpToken token = line[iPos];
            if (token.IsIdentifier() || token.IsKeyword())
            {
                if (iPos < line.Count - 1)
                {
                    token = line[iPos + 1];
                    if (token.Type == XSharpLexer.DOT)
                        return false;
                }
                return true;
            }
            return false;
        }
        #region Preprocessor Directives

        private void checkForUnexpectedPPInput(IList<XSharpToken> line, int nMax)
        {
            if (line.Count > nMax)
            {
                Error(line[nMax], ErrorCode.ERR_EndOfPPLineExpected);
            }
        }
        private void doRegionDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.WRN_PreProcessorWarning, "Region name expected");
            }
            if (IsActive())
            {
                var token = line[0];
                _regions.Push(token);
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
        }

        private void doEndRegionDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                var token = line[0];
                if (_regions.Count > 0)
                    _regions.Pop();
                else
                    Error(token, ErrorCode.ERR_PreProcessorError, "#endregion directive without matching #region found");
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
            // ignore comments after #endregion
            //checkForUnexpectedPPInput(line, 1);
        }

        private void doDefineDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.ERR_PreProcessorError, "Identifier expected");
                return;
            }
            if (IsActive())
            {
                writeToPPO(original, true);
                addDefine(line, original);
            }
            else
            {
                writeToPPO("");
            }
        }

        private void doUnDefDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.ERR_PreProcessorError, "Identifier expected");
                return;
            }
            if (IsActive())
            {
                removeDefine(line);
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doErrorWarningDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            var nextType = line[0].Type;
            if (IsActive())
            {
                string text;
                XSharpToken ln;
                ln = line[0];
                if (line.Count < 2)
                {
                    text = "<Empty message>";
                }
                else
                {

                    writeToPPO(original, true);
                    text = "";
                    for (int i = 1; i < line.Count; i++)
                    {
                        text += line[i].TextWithTrivia;
                    }
                }
                if (ln.SourceSymbol != null)
                    ln = ln.SourceSymbol;
                switch (nextType)
                {
                    case XSharpLexer.PP_WARNING:
                        Error(ln, ErrorCode.WRN_WarningDirective, text);
                        break;
                    case XSharpLexer.PP_STDOUT:
                        StdOut(text);
                        break;
                    case XSharpLexer.PP_ERROR:
                        Error(ln, ErrorCode.ERR_ErrorDirective, text);
                        break;
                }
                lastToken = ln;
            }
            else
            {
                writeToPPO("");
            }
        }
        class TextProperties
        {
            internal readonly XSharpToken Start;
            internal readonly XSharpToken List = null;
            internal XSharpToken Operator = null;
            internal readonly XSharpToken LParen;
            internal readonly XSharpToken RParen;
            internal readonly XSharpToken Ws;
            internal readonly XSharpToken LCurly;
            internal readonly XSharpToken RCurly;
            internal readonly XSharpToken Colon;
            internal readonly XSharpToken Dot;
            internal IList<XSharpToken> textVarName = null;
            internal IList<XSharpToken> textDelim = null;
            internal IList<XSharpToken> textLineFunc = null;
            internal IList<XSharpToken> textLineEnd = null;
            internal IList<XSharpToken> textEndFunc = null;
            internal TextProperties(XSharpToken token)
            {
                if (token.SourceSymbol != null && token != token.SourceSymbol)
                    token = token.SourceSymbol;
                Start = token;
                LParen = new XSharpPPToken(XSharpLexer.LPAREN, "(", token, true);
                RParen = new XSharpPPToken(XSharpLexer.RPAREN, ")", token, true);
                Ws = new XSharpPPToken(XSharpLexer.WS, " ", token, false) { Channel = XSharpLexer.Hidden };
                Colon = new XSharpPPToken(XSharpLexer.COLON, ":", token, true);
                Dot = new XSharpPPToken(XSharpLexer.DOT, ".", token, true);
                var name = XSharpSpecialNames.LocalPrefix + Start.Position.ToString();
                List = new XSharpPPToken(XSharpLexer.ID, name, Start, true);
                LCurly = new XSharpPPToken(XSharpLexer.LCURLY, "{", token, true);
                RCurly = new XSharpPPToken(XSharpLexer.RCURLY, "}", token, true);
            }
        }

        IList<XSharpToken> doTextLine(IList<XSharpToken> original, bool write2PPO)
        {
            var result = new List<XSharpToken>();
            if (original.Count > 0 && _textProps != null)
            {
                if (_textProps.textLineFunc?.Count > 0)
                {
                    // UniqueName:Add( "    " )
                    // or
                    // UniqueName:Add( LTrim(
                    result.AddRange(_textProps.textLineFunc);
                }
                var sb = new StringBuilder();
                sb.Append("\"");
                foreach (var token in original)
                {
                    sb.Append(token.Text);
#if VSPARSER
                    //mark tokens with new type to give them a different color in the editor
                    token.Type = XSharpLexer.TEXT_STRING_CONST;
#endif
                }
                sb.Append("\"");
                result.Add(new XSharpPPToken(XSharpLexer.STRING_CONST, sb.ToString(), original[0], true));
                if (_textProps.textDelim?.Count > 0)
                {
                    result.Add(new XSharpPPToken(XSharpLexer.PLUS, "+", original[0], true));
                    result.AddRange(_textProps.textDelim);
                }
                if (_textProps.textLineEnd != null)
                {
                    // add one ) or ))
                    result.AddRange(_textProps.textLineEnd);
                }
                result.Add(new XSharpPPToken(XSharpLexer.EOS, "\n", original[0], true));

            }
            if (write2PPO)
            {
                if (result.Count > 0)
                    writeToPPO(result, false);
                else
                    writeToPPO("");
            }
            return result;
        }
        IList<XSharpToken> doTextDirective(IList<XSharpToken> original, bool write2PPO)
        {
            // #text := <cVarName> [, LineDelimiter [, LineFunc, [, EndFunc]] ]
            var result = new List<XSharpToken>();
            if (_textProps != null)
            {
                var sym = original[0].SourceSymbol;
                if (sym == null)
                    sym = original[0];
                Error(sym, ErrorCode.ERR_UnexpectedToken, sym.Text);
                return result;
            }
            var temp = new List<XSharpToken>();
            original = stripWs(original);
            _textProps = new TextProperties(original[0]);
            Debug.Assert(original.Count > 0 && original[0].Type == XSharpLexer.PP_TEXT);
            int current;
            int lastUsed;
            XSharpToken first = null;
            if (original.Count > 1)
            {
                first = original[1];
            }
            if (first != null && (first.Type == XSharpLexer.ASSIGN_OP || first.Type == XSharpLexer.ASSIGN_ADD))
            {
                _textProps.Operator = first;
                current = 2;
                PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                _textProps.textVarName = original.GetRange(current, lastUsed);
                current = lastUsed + 1;
                if (current < original.Count - 1 && original[current].Type == XSharpLexer.COMMA)
                {
                    current += 1;
                    PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                    _textProps.textDelim = original.GetRange(current, lastUsed);
                    current = lastUsed + 1;
                    if (current < original.Count - 1 && original[current].Type == XSharpLexer.COMMA)
                    {
                        current += 1;
                        PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                        _textProps.textLineFunc = original.GetRange(current, lastUsed);
                        current = lastUsed + 1;
                        if (current < original.Count - 1 && original[current].Type == XSharpLexer.COMMA)
                        {
                            current += 1;
                            PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                            _textProps.textEndFunc = original.GetRange(current, lastUsed);
                        }
                    }
                }
                if (_textProps.textVarName?.Count == 0)
                {
                    Error(original[0], ErrorCode.ERR_IdentifierExpected);
                }
                else
                {
                    // var UniqueName := System.Collections.Generic.StringList{}
                    result.Add(new XSharpPPToken(XSharpLexer.VAR, "VAR", _textProps.Start, true));
                    result.Add(_textProps.Ws);
                    result.Add(_textProps.List);
                    result.Add(_textProps.Ws);
                    result.Add(new XSharpPPToken(XSharpLexer.ASSIGN_OP, ":=", _textProps.Start, true));
                    result.Add(new XSharpPPToken(XSharpLexer.ID, "System", _textProps.Start, true));
                    result.Add(_textProps.Dot);
                    result.Add(new XSharpPPToken(XSharpLexer.ID, "Text", _textProps.Start, true));
                    result.Add(_textProps.Dot);
                    result.Add(new XSharpPPToken(XSharpLexer.ID, "StringBuilder", _textProps.Start, true));
                    result.Add(_textProps.LCurly);
                    result.Add(_textProps.RCurly);

                    // UniqueName:Append( .....)
                    temp.Add(_textProps.List);
                    temp.Add(_textProps.Colon);
                    temp.Add(new XSharpPPToken(XSharpLexer.ID, "Append", _textProps.Start, true));
                    temp.Add(_textProps.LParen);
                    if (_textProps.textLineFunc?.Count > 0)
                    {
                        temp.AddRange(_textProps.textLineFunc);
                        temp.Add(_textProps.LParen);
                        temp.Add(_textProps.Ws);
                        _textProps.textLineFunc = temp.ToArray();
                        temp.Clear();
                        temp.Add(_textProps.Ws);
                        temp.Add(_textProps.RParen);
                    }
                    else
                    {
                        _textProps.textLineFunc = temp.ToArray();
                        temp.Clear();
                    }
                    temp.Add(_textProps.Ws);
                    temp.Add(_textProps.RParen);
                    _textProps.textLineEnd = temp.ToArray();
                }
            }
            else
            {
                // #text LineFunc, [, EndFunc]
                current = 1;
                PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                temp.AddRange(original.GetRange(current, lastUsed));
                temp.Add(_textProps.LParen);
                _textProps.textLineFunc = temp.ToArray();
                temp.Clear();
                temp.Add(_textProps.RParen);
                _textProps.textLineEnd = temp.ToArray();
                current = lastUsed + 1;
                if (current < original.Count - 1 && original[current].Type == XSharpLexer.COMMA)
                {
                    current += 1;
                    temp.Clear();
                    PPRule.matchExpression(PPUDCType.Command, current, original, null, _options.VOPreprocessorBehaviour, out lastUsed);
                    temp.AddRange(original.GetRange(current, lastUsed));
                    _textProps.textEndFunc = temp.ToArray();
                }
            }
            if (write2PPO)
            {
                if (result.Count > 0)
                    writeToPPO(result, false);
                else
                    writeToPPO("");
            }
            return result;

        }
        IList<XSharpToken> doEndTextDirective(IList<XSharpToken> original, bool write2PPO)
        {
            Debug.Assert(original.Count > 0 && original[0].Type == XSharpLexer.PP_ENDTEXT);
            var anchor = original[0];
            var result = new List<XSharpToken>();
            if (_textProps == null)
            {
                var sym = original[0].SourceSymbol;
                if (sym == null)
                    sym = original[0];
                Error(sym, ErrorCode.ERR_UnexpectedToken, sym.Text);
            }
            else
            {
                if (_textProps.textVarName != null)
                {
                    result.AddRange(_textProps.textVarName);
                    result.Add(_textProps.Ws);
                    result.Add(_textProps.Operator);
                    result.Add(_textProps.Ws);
                }
                if (_textProps.textEndFunc?.Count > 0)
                {
                    result.AddRange(_textProps.textEndFunc);
                    result.Add(_textProps.LParen);
                }
                if (_textProps.textVarName != null)
                {
                    result.Add(_textProps.List);
                    result.Add(_textProps.Colon);
                    result.Add(new XSharpPPToken(XSharpLexer.ID, "ToString", anchor, true));
                    result.Add(_textProps.LParen);
                }
                if (_textProps.textVarName != null)
                {
                    result.Add(_textProps.RParen);
                }
                if (_textProps.textEndFunc?.Count > 0)
                {
                    result.Add(_textProps.RParen);
                }
                result.Add(new XSharpPPToken(XSharpLexer.EOS, "\n", anchor, true));
                if (write2PPO)
                {
                    if (result.Count > 0)
                        writeToPPO(result, false);
                    else
                        writeToPPO("");
                }
            }
            _textProps = null;
            return result;
        }
        private void doIfDirective(IList<XSharpToken> original)
        {
            List<IToken> expandedTokens = new();
            bool startOfExpression = false;
            bool afterIf = false;
            XSharpToken ifToken = null;
            writeToPPO(original, true);
            foreach (var token in original)
            {
                bool include = startOfExpression;
                switch (token.Type)
                {
                    case XSharpLexer.PP_IF:
                        afterIf = true;
                        ifToken = token;
                        break;
                    case XSharpLexer.WS:
                        if (afterIf)
                            startOfExpression = true;
                        break;
                    default:
                        break;
                }
                if (include)
                {
                    if (token.IsIdentifier() || token.IsKeyword())
                    {
                        var id = token.Text;
                        if (_symbolDefines.ContainsKey(id))
                        {
                            var tokens = _symbolDefines[id];
                            foreach (var t in tokens)
                            {
                                expandedTokens.Add(new XSharpPPToken(t, t, true));
                            }
                        }
                        else if (_macroDefines.ContainsKey(id))
                        {
                            var tokens = _symbolDefines[id];
                            foreach (var t in tokens)
                            {
                                expandedTokens.Add(new XSharpPPToken(t, t, true));
                            }
                        }
                        else
                        {
                            // undefined tokens get the value FALSE
                            var newtoken = new XSharpPPToken(XSharpLexer.FALSE_CONST, token.Text, token, true);
                            expandedTokens.Add(newtoken);
                        }
                    }
                    else if (token.Channel != Channel.Hidden)
                    {

                        if (token.Type == XSharpLexer.MACRO)
                        {
                            var newtoken = getMacroValue(token);
                            expandedTokens.Add(newtoken);

                        }
                        else
                        {
                            expandedTokens.Add(new XSharpPPToken(token, token, true));
                        }
                    }
                }
            }
            // expanded tokens now contains an expression that we want to evaluate
            bool condition = false;
            if (ifToken != null)
            {
                condition = EvaluateTokens(expandedTokens, ifToken);
            }
            _defStates.Push(condition);
        }

        bool EvaluateTokens(List<IToken> tokens, XSharpToken ifToken)
        {
            var source = new ListTokenSource(tokens);
            var stream = new CommonTokenStream(source);
            stream.Fill();
            var parser = new XSharpParser(stream);
            parser.RemoveErrorListeners();
            var errorListener = new XSharpErrorListener(_fileName, _parseErrors);
            parser.AddErrorListener(errorListener);
            var result = true;
            try
            {
                var tree = parser.expression();
                if (stream.Index < stream.GetTokens().Count)
                {
                    var token = stream.Lt(1);
                    if (token.Type != XSharpLexer.EOF)
                    {
                        Error((XSharpToken)token, ErrorCode.ERR_PreProcessorError, "Unexpected Token: " + token.Text);
                        result = false;
                    }
                }
                if (result)
                {
                    result = PPExpressionEvaluator.Evaluate(tree, this);
                }
            }
            catch (Exception e)
            {
                Error(ifToken, ErrorCode.ERR_PreProcessorError, "Error evaluating #if expression: " + e.Message);
                result = false;
            }
            return result;
        }

        private void doIfDefDirective(IList<XSharpToken> original, bool isIfDef)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.ERR_PreProcessorError, "Identifier expected");
                return;
            }
            if (IsActive())
            {
                var def = line[1];
                if (def.IsIdentifier() || def.IsKeyword())
                {
                    if (isIfDef)
                        _defStates.Push(IsDefined(def.Text, def));
                    else
                        _defStates.Push(!IsDefined(def.Text, def));
                }
                else if (def.Type == XSharpLexer.MACRO)
                {
                    if (isIfDef)
                        _defStates.Push(IsDefinedMacro(def));
                    else
                        _defStates.Push(!IsDefinedMacro(def));
                }
                else
                {
                    Error(def, ErrorCode.ERR_PreProcessorError, "Identifier expected");
                }
                writeToPPO(original, true);

            }
            else
            {
                _defStates.Push(false);
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doElseDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            writeToPPO(original, true);
            if (_defStates.Count > 0)
            {
                bool a = _defStates.Pop();
                if (IsActive())
                {
                    _defStates.Push(!a);
                }
                else
                    _defStates.Push(false);
            }
            else
            {
                Error(Lt(), ErrorCode.ERR_PreProcessorError, "Unexpected #else");
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doEndifDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (_defStates.Count > 0)
            {
                _defStates.Pop();
                if (IsActive())
                {
                    writeToPPO(original, true);
                }
                else
                {
                    writeToPPO("");
                }
            }
            else
            {
                Error(Lt(), ErrorCode.ERR_PreProcessorError, "Unexpected #endif");
                writeToPPO(line, true);
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doPragmaDirective(IList<XSharpToken> originalTokens, bool isIfDef)
        {
            Debug.Assert(originalTokens.Count > 0);
            var tokens = stripWs(originalTokens);
            XSharpToken errortoken = originalTokens[0];
            var pragmaText = "";
            foreach (var token in originalTokens)
            {
                pragmaText += token.text;
            }
            if (tokens.Count <= 2)
            {
                Error(errortoken, ErrorCode.WRN_IllegalPragma, pragmaText);
                return;
            }
            XSharpToken start = null;
            XSharpToken i1 = null;          // options or warnings
            XSharpToken i2 = null;
            ErrorCode error = ErrorCode.Unknown;
            Pragmastate state = Pragmastate.Default;
            var numbers = new List<IToken>();
            bool isWarning = false;
            PragmaBase pragma = null;

            start = tokens[0];
            i1 = tokens[1]; // #help 0 is the pragma token
            if (tokens.Count > 2)
            {
                i2 = tokens[2];
                // 0       1        2  3  4  5   6
                // #pragma warnings ( 123 , off  )
                // #pragma warnings ( pop )
                if (i2.Type == XSharpParser.LPAREN)
                {
                    if (tokens.Count >= 6 && tokens[4].Type == XSharpParser.COMMA && tokens[6].Type == XSharpParser.RPAREN)
                    {
                        i2 = tokens[5];
                        numbers.Add(tokens[3]);
                    }
                    else if (tokens.Count >= 4 && tokens[4].Type == XSharpParser.RPAREN)
                    {
                        i2 = tokens[3];
                    }
                }
                else
                {
                    // 0       1        2       3 4 5 6 7
                    // #pragma warnings disable 1 , 2 , 3
                    // #pragma warnings enable  1 , 2 , 3
                    if (tokens.Count > 3)
                    {
                        for (int i = 3; i < tokens.Count; i++)
                        {
                            if (tokens[i].Type != XSharpParser.COMMA)
                            {
                                numbers.Add(tokens[i]);
                            }
                        }
                    }
                    else
                    {
                        error = ErrorCode.WRN_IllegalPragma;
                        errortoken = tokens[0];
                    }
                }
                if (i1 == null)
                {
                    error = ErrorCode.WRN_IllegalPragma;
                    errortoken = tokens[0];
                }
                if (error == ErrorCode.Unknown)
                {

                    switch (i1.Text.ToLower())
                    {
                        case "options":
                            isWarning = false;
                            break;
                        case "warning":
                        case "warnings":
                            isWarning = true;
                            break;
                        default:
                            error = ErrorCode.WRN_IllegalPragma;
                            errortoken = i1;
                            break;
                    }
                }
                if (error == ErrorCode.Unknown)
                {
                    if (i2 != null)
                    {
                        switch (i2.Text.ToLower())
                        {
                            case "enable":
                            case "true":
                            case "on":
                                state = Pragmastate.On;
                                break;
                            case "disable":
                            case "false":
                            case "off":
                                state = Pragmastate.Off;
                                break;

                            case "restore":
                            case "default":
                            case "pop":
                                state = Pragmastate.Default;
                                break;
                            default:
                                error = ErrorCode.WRN_IllegalPPWarning;
                                errortoken = i2;
                                break;
                        }
                    }
                    else
                    {
                        error = ErrorCode.WRN_IllegalPPWarning;
                        errortoken = i1;
                    }
                }
                if (error == ErrorCode.Unknown)
                {
                    if (isWarning)
                    {
                        pragma = new PragmaWarning(start, state, numbers, i1, i2);
                    }
                    else if (numbers.Count == 0)
                    {
                        // options pop
                        if (i2.Text.ToLower() == "pop")
                        {
                            pragma = new PragmaOption(start, Pragmastate.Default, CompilerOption.All);
                        }
                        else
                        {
                            error = ErrorCode.WRN_IllegalPPWarning;
                            errortoken = i2;
                        }

                    }
                    else
                    {
                        var token = numbers[0];
                        var opt = token.Text.ToLower();
                        if (token.Type == XSharpParser.STRING_CONST && opt.StartsWith("\"") && opt.EndsWith("\"") && opt.Length > 2)
                        {
                            opt = opt.Substring(1, opt.Length - 2);
                        }
                        if (opt.Length > 0)
                        {
                            var compopt = CompilerOptionDecoder.Decode(opt);
                            if (compopt.NeedsRuntime() && !_options.HasRuntime)
                            {
                                var errdata = new ParseErrorData(token,
                                    ErrorCode.ERR_CompilerOptionNotSupportedForDialect, opt, compopt.Description(), _options.Dialect);
                                _parseErrors.Add(errdata);
                            }
                            // options sorted in alphabetical order with the exception of the fox and xpp options
                            switch (compopt)
                            {
                                case CompilerOption.AllowDotForInstanceMembers:
                                case CompilerOption.AllowOldStyleAssignments:
                                case CompilerOption.ArrayZero:
                                case CompilerOption.EnforceOverride:
                                case CompilerOption.EnforceSelf:
                                case CompilerOption.InitLocals:
                                case CompilerOption.LateBinding:
                                case CompilerOption.MemVars:
                                case CompilerOption.Overflow:
                                case CompilerOption.UndeclaredMemVars:
                                // case "vo1": // Init/axit
                                case CompilerOption.Vo2:     // Initialize string variables with empty strings
                                case CompilerOption.Vo3:     // All instance members virtual
                                case CompilerOption.Vo4:     // Integer conversions
                                case CompilerOption.Vo5:     // Implicit Clipper Calling convention
                                case CompilerOption.Vo6:     // ResolveTypedFunctionPointersToPtr
                                case CompilerOption.Vo7:     // Implicit Casts and Conversions
                                                             // case "vo8": // Compatible preprocessor
                                case CompilerOption.Vo9:     // Allow missing return statements or missing return values
                                case CompilerOption.Vo10:     // Compatible IIF
                                case CompilerOption.Vo11:    // Fractional -> Integral Conversions
                                case CompilerOption.Vo12:    // Clipper Integer divisions
                                case CompilerOption.Vo13:    // StringComparisons
                                case CompilerOption.Vo14:    // Embed real constants as float
                                case CompilerOption.Vo15:    // Untyped allowed
                                case CompilerOption.Vo16:    // Add Clipper CC Missing constructors
                                case CompilerOption.Vo17:    // Compatible Begin Sequence .. END Sequence
                                    pragma = new PragmaOption(start, state, compopt);
                                    break;
                                //case "xpp1":    // classes inherit from XPP.Abstract
                                //case "xpp2":    // strongly typed entry point
                                // case "fox1": // Classes inherit from unknown
                                case CompilerOption.Fox2:    // FoxPro array syntax
                                    if (_options.Dialect != XSharpDialect.FoxPro)
                                        goto default;
                                    pragma = new PragmaOption(start, state, compopt);
                                    break;
                                default:
                                    error = ErrorCode.WRN_IllegalPPOption;
                                    errortoken = (XSharpToken)numbers[0];
                                    break;
                            }
                        }
                    }
                }
                // C# does not validate the error codes, so we will not do that either.
                if (error != ErrorCode.Unknown)
                {
                    var errdata = new ParseErrorData(errortoken, error, pragmaText);
                    _parseErrors.Add(errdata);
                }
            }
            if (pragma != null)
                Pragmas.Add(pragma);
        }

        private void doIncludeDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.ERR_PreProcessorError, "Filename expected");
                return;
            }
            if (IsActive())
            {
                writeToPPO(original, true);
                if (_files.Count == MaxIncludeDepth)
                {
                    Error(line[0], ErrorCode.ERR_PreProcessorError, "Reached max include depth: " + MaxIncludeDepth);
                }
                else
                {
                    var token = line[1];
                    if (token.Type == XSharpLexer.STRING_CONST)
                    {
                        string fileName = token.Text.Substring(1, token.Text.Length - 2);
                        ProcessIncludeFile(fileName, token);

                    }
                    else
                    {
                        Error(token, ErrorCode.ERR_PreProcessorError, "String literal expected");
                    }
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doLineDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                writeToPPO(original, true);
                var ln = line[1];
                if (ln.Type == XSharpLexer.INT_CONST)
                {
                    if (line.Count > 2)
                    {
                        ln = line[2];
                    }
                    if (ln.Type == XSharpLexer.STRING_CONST)
                    {
                        inputs.SourceFileName = ln.Text.Substring(1, ln.Text.Length - 2);
                    }
                    else
                    {
                        Error(ln, ErrorCode.ERR_PreProcessorError, "String literal expected");
                    }
                }
                else
                {
                    Error(ln, ErrorCode.ERR_PreProcessorError, "Integer literal expected");
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 3);
        }

        private void doUnexpectedUDCSeparator(IList<XSharpToken> line)
        {
            Debug.Assert(line?.Count > 0);
            var ln = line[0];
            writeToPPO(line, true);
            Error(ln, ErrorCode.ERR_PreProcessorError, "Unexpected UDC separator character found");
        }

        private IList<XSharpToken> doNormalLine(IList<XSharpToken> line, bool write2PPO = true)
        {
            // Process the whole line in one go and apply the defines, macros and udcs
            // This is modeled after the way it is done in Harbour
            // 1) Look for and replace defines
            // 2) Look for and replace Macros (combined with 1) for performance)
            // 3) look for and replace (x)translates
            // 4) look for and replace (x)commands
            var orgline = line;
            if (IsActive())
            {
                Debug.Assert(line?.Count > 0);
                IList<XSharpToken> result = null;
                bool changed = true;
                // repeat this loop as long as there are matches
                while (changed)
                {
                    changed = false;
                    if (line.Count > 0)
                    {
                        if (doProcessDefinesAndMacros(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (_hasTransrules && line.Count > 0)
                    {
                        if (doProcessTranslates(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (_hasCommandrules && line.Count > 0)
                    {
                        if (doProcessCommands(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (changed && result != null && result.Count > 0)
                    {
                        // Adjust positions of the tokens to improve error reports
#if !VSPARSER
                        var source = orgline.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).FirstOrDefault();
                        if (source != null)
                        {
                            var start = source.StartIndex;
                            var lineNum = source.Line;
                            var column = source.Column;
                            for (int i = 0; i < result.Count; i++)
                            {
                                var t1 = result[i];
                                if (t1.Channel == XSharpLexer.DefaultTokenChannel)
                                {
                                    t1 = new XSharpToken(t1);
                                    t1.StartIndex = start;
                                    t1.Line = lineNum;
                                    t1.Column = column;
                                    t1.StopIndex = t1.StartIndex + t1.Text.Length - 1;
                                    t1.CopyTokenSource(source);
                                    result[i] = t1;
                                }
                                start += t1.Text.Length;
                                column += t1.Text.Length;
                            }
                        }
#endif
                        result = _lexer.ReclassifyTokens(result);
                    }
                }
            }
            else
            {
#if VSPARSER
                // Set the channel so the text will appear "greyed out" in the editor
                for (int i = 0; i < line.Count; i++)
                {
                    var t = line[i];
                    t.Original.Channel = Channel.DefOut;
                }
#endif
                line.Clear();
            }
            if (write2PPO)
            {
                if (line.Count > 0)
                    writeToPPO(line, false);
                else
                    writeToPPO("");
            }
            return line;
        }

        private static IList<XSharpToken> copySource(IList<XSharpToken> line, int nCount)
        {
            var result = new List<XSharpToken>(line.Count);
            var temp = new XSharpToken[nCount];
            for (int i = 0; i < nCount; i++)
            {
                temp[i] = line[i];
            }
            result.AddRange(temp);
            return result;
        }
        private bool doProcessDefinesAndMacros(IList<XSharpToken> line, out IList<XSharpToken> result)
        {
            Debug.Assert(line?.Count > 0);
            // we loop in here because one define may add tokens that are defined by another
            // such as:
            // #define FOO 1
            // #define BAR FOO + 1
            // when the code is "? BAR" then we need to translate this to "? 1 + 1"
            // For performance reasons we assume there is nothing to do, so we only
            // start allocating a result collection when a define is detected
            // otherwise we will simply return the original string
            bool hasChanged = false;
            IList<XSharpToken> tempResult = line;
            result = null;
            while (tempResult != null)
            {
                tempResult = null;
                // in a second iteration line will be the changed line
                for (int i = 0; i < line.Count; i++)
                {
                    var token = line[i];
                    if (isDefineAllowed(line, i) && token.Text != null && _symbolDefines.TryGetValue(token.Text, out var deflist))
                    {
                        if (tempResult == null)
                        {
                            // this is the first define in the list
                            // allocate a result and copy the items 0 .. i-1 to the result
                            tempResult = copySource(line, i);
                        }
                        if (deflist != null)
                        {
                            foreach (var t in deflist)
                            {
                                var t2 = new XSharpPPToken(t, token, true);
                                tempResult.Add(t2);
                            }
                        }
                        else
                        {
                            // add a space so error messages look proper
                            var t2 = new XSharpPPToken(XSharpLexer.WS, " <RemovedToken> ", token)
                            {
                                Channel = Channel.Hidden,
                            };
                            tempResult.Add(t2);
                        }
                    }
                    else if (token.Type == XSharpLexer.MACRO)
                    {
                        // Macros that cannot be found are changed to ID
                        if (_macroDefines.TryGetValue(token.Text, out var ft))
                        {
                            var nt = ft(token);
                            if (nt != null)
                            {
                                if (tempResult == null)
                                {
                                    // this is the first macro in the list
                                    // allocate a result and copy the items 0 .. i-1 to the result
                                    tempResult = copySource(line, i);
                                }
                                tempResult.Add(nt);
                            }
                        }
                    }
                    else if (tempResult != null)
                    {
                        tempResult.Add(token);
                    }
                }
                if (tempResult != null)
                {
                    // copy temporary result to line for next iteration
                    line = tempResult;
                    result = line;
                    hasChanged = true;
                }
            }
            return hasChanged;
        }

        private XSharpToken getMacroValue(XSharpToken token)
        {
            if (_macroDefines.TryGetValue(token.Text, out var ft))
            {
                var nt = ft(token);
                if (nt != null)
                {
                    return nt;
                }
            }
            return token;
        }
        private bool doProcessTranslates(IList<XSharpToken> line, out IList<XSharpToken> result)
        {
            Debug.Assert(line?.Count > 0);
            var temp = new List<XSharpToken>();
            temp.AddRange(line);
            result = new List<XSharpToken>();
            var usedRules = new PPUsedRules(this, MaxUDCDepth);
            while (temp.Count > 0)
            {
                var rule = _transRules.FindMatchingRule(temp, out var matchInfo);
                if (rule != null)
                {
                    temp = doReplace(temp, rule, matchInfo);
                    if (usedRules.HasRecursion(rule, temp))
                    {
                        // duplicate, so exit now
                        result.Clear();
                        return false;
                    }
                    // note that we do not add the result of the replacement to processed
                    // because it will processed further
                }
                else
                {
                    // first token of temp is not start of a #(x)translate. So add it to the result
                    // and try from second token etc
                    result.Add(temp[0]);
                    temp.RemoveAt(0);
                }
            }
            if (usedRules.Count > 0)
            {
                result.TrimLeadingSpaces();
                return true;
            }
            result = null;
            return false;
        }

        private List<IList<XSharpToken>> splitCommands(IList<XSharpToken> tokens, out IList<XSharpToken> separators)
        {
            var result = new List<IList<XSharpToken>>(10);
            var current = new List<XSharpToken>(tokens.Count);
            separators = new List<XSharpToken>();
            foreach (var t in tokens)
            {
                if (t.Type == XSharpLexer.EOS)
                {
                    current.TrimLeadingSpaces();
                    result.Add(current);
                    current = new List<XSharpToken>();
                    separators.Add(t);
                }
                else
                {
                    current.Add(t);
                }
            }
            result.Add(current);
            return result;
        }
        private bool doProcessCommands(IList<XSharpToken> line, out IList<XSharpToken> result)
        {
            Debug.Assert(line?.Count > 0);
            line = stripWs(line);
            result = null;
            if (line.Count == 0)
                return false;
            result = line;
            var usedRules = new PPUsedRules(this, MaxUDCDepth);
            while (true)
            {
                var rule = _cmdRules.FindMatchingRule(result, out var matchInfo);
                if (rule == null)
                {
                    // nothing to do, so exit. Leave changed the way it is. This does not have to be the first iteration
                    break;
                }
                result = doReplace(result, rule, matchInfo);
                if (usedRules.HasRecursion(rule, result))
                {
                    // duplicate so exit now
                    result.Clear();
                    return false;
                }
                // the UDC may have introduced a new semi colon and created more than one sub statement
                // so check to see and then process every statement
                var cmds = splitCommands(result, out var separators);
                Debug.Assert(cmds.Count == separators.Count + 1);
                if (cmds.Count <= 1)
                {
                    // single statement result. Try again to see if the new statement matches another UDC rule
                    continue;
                }
                else
                {
                    // multi statement result. Process each statement separately (recursively) as a 'normal line'
                    // the replacement may have introduced the usage of a define, translate or macro
                    result.Clear();
                    for (int i = 0; i < cmds.Count; i++)
                    {
                        if (cmds[i].Count > 0)
                        {
                            var res = ProcessLine(cmds[i], false);
                            if (res == null)
                                cmds[i].Clear();
                            else
                                cmds[i] = res;
                            if (cmds[i].Count > 0)
                            {
                                foreach (var token in cmds[i])
                                {
                                    result.Add(token);
                                }
                                if (i < cmds.Count - 1)
                                {
                                    result.Add(separators[i]);
                                }
                            }
                        }
                    }
                }
            }
            if (usedRules.Count > 0)
            {
                // somerule => #Error
                result.TrimLeadingSpaces();
                if (result.Count > 0 && result[0].Channel == Channel.PreProcessor)
                {
                    result = ProcessLine(result, false);
                    if (result == null)
                    {
                        result = new List<XSharpToken>();
                    }
                }
                return true;
            }
            result = null;
            return false;
        }

        private void doEOFChecks()
        {
            if (_defStates.Count > 0)
            {
                Error(Lt(), ErrorCode.ERR_EndifDirectiveExpected);
            }
            while (_regions.Count > 0)
            {
                var token = _regions.Pop();
                Error(token, ErrorCode.ERR_EndRegionDirectiveExpected);
            }
            if (_textProps != null)
            {
                var token = _textProps.Start.SourceSymbol;
                if (token == null)
                    token = _textProps.Start;
                Error(token, ErrorCode.ERR_MissingEndText);
                _textProps = null;
            }
        }

        #endregion

        private List<XSharpToken> doReplace(IList<XSharpToken> line, PPRule rule, PPMatchRange[] matchInfo)
        {
            Debug.Assert(line?.Count > 0);
            var res = rule.Replace(line, matchInfo);
            _rulesApplied += 1;
            var result = new List<XSharpToken>();
            result.AddRange(res);
#if FULLCOMPILER
            if (_options.Verbose)
            {
                int lineNo;
                if (line[0].SourceSymbol != null)
                    lineNo = line[0].SourceSymbol.Line;
                else
                    lineNo = line[0].Line;
                DebugOutput("----------------------");
                DebugOutput("File {0} line {1}:", _fileName, lineNo);
                DebugOutput("   UDC   : {0}", rule.GetDebuggerDisplay());
                DebugOutput("   Input : {0}", line.AsString());
                DebugOutput("   Output: {0}", res.AsString());
            }
#endif
            return result;
        }
    }
}
