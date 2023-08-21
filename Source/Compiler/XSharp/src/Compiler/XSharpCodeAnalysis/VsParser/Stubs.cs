//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Globalization;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;

namespace System.Collections.Immutable
{
    public static class IListExtensions
    {
        public static IList<T> ToImmutableArray<T>(this IList<T> source)
        {
            return source.ToArray();
        }
    }
}

namespace Microsoft.CodeAnalysis.Text
{
    public struct LinePositionSpan
    {
        public int Line;
        public int Column;
        public string FileName;
    }

    public class SourceText
    {
        public string Text { get; set; }
        public static SourceText From(string source)
        {
            return new SourceText(source);
        }
        private SourceText(string source)
        {
            Text = source;
        }
        public override string ToString() => Text;

    }
    public struct TextSpan
    {
        public int Start;
        public int Length;

        public TextSpan(int start, int length)
        {
            Start = start;
            Length = length;
        }
    }


}

namespace System
{
    public enum SourceHashAlgorithm
    {
        None = 0
    }

}


namespace Roslyn.Utilities
{
    internal static class FileUtilities
    {

        internal static DateTime GetFileTimeStamp(string fileName)
        {
            return System.IO.File.GetLastWriteTime(fileName);
        }

        internal static System.IO.Stream CreateFileStreamChecked(Func<string, System.IO.Stream> func, string name, string desc)
        {
            return func(name);
        }
    }
    internal static class PathUtilities
    {
        internal static string GetDirectoryName(string fileName) => System.IO.Path.GetDirectoryName(fileName);
        internal static string GetFileName(string fileName, bool includeExtension = true)
        {
            if (includeExtension)
                return System.IO.Path.GetFileName(fileName);
            else
                return System.IO.Path.GetFileNameWithoutExtension(fileName);
        }
    }
    internal static class FileNameUtilities
    {
        internal static string GetExtension(string fileName) => System.IO.Path.GetExtension(fileName);
        internal static string ChangeExtension(string fileName, string ext) => System.IO.Path.ChangeExtension(fileName, ext);
    }
}

namespace Microsoft.CodeAnalysis.Diagnostics
{

}
namespace Microsoft.CodeAnalysis.PooledObjects
{

    internal class ArrayBuilder<T> : List<T>
    {
        internal ArrayBuilder<T> Create()
        {
            return new ArrayBuilder<T>();
        }

    }
}

namespace Microsoft.CodeAnalysis.Emit
{

    public struct EmitOptions
    {

    }
}




namespace Microsoft.CodeAnalysis.CSharp
{

    public enum InternalErrorCode
    {
        Void = 0,
        Unknown = 1
    }

    public enum LanguageVersion
    {
        CSharp7_3,
        CSharp9
    }
    internal static partial class ErrorFacts
    {
        internal static string GetMessage(ErrorCode error, CultureInfo culture) => "message " + error.ToString();

    }

    public partial class CSharpParseOptions : ParseOptions
    {
        public LanguageVersion LanguageVersion { get; set; }
        public CSharpParseOptions()
        {
            return;
        }
        public CSharpParseOptions(CSharpParseOptions original)
        {
            return;
        }
        public SourceCodeKind Kind { get; set; }

        public IList<string> PreprocessorSymbols { get; set; } = new List<string>();
        public IList<string> PreprocessorSymbolsUpper { get; set; } = new List<string>();

        public static CSharpParseOptions Default => new CSharpParseOptions();

        public static CSharpParseOptions FromVsValues(IList<string> options)
        {
            var parser = new CSharpCommandLineParser();
            var diags = new List<Diagnostic>();
            parser.ResetXSharpCommandlineOptions();
            var defines = new List<string>();
            var definesUpper = new List<string>();
            foreach (var opt in options)
            {
                string name, value;
                var pos = opt.IndexOf(":");
                if (pos > 0)
                {

                    name = opt.Substring(0, pos);
                    value = opt.Substring(pos + 1);
                }
                else
                {
                    name = opt;
                    value = "";
                }
                name = name.ToLower();
                if (name == "d")
                {
                    var defs = value.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                    defines.AddRange(defs);
                    foreach (var d in defs)
                    {
                        definesUpper.Add(d.ToUpper());
                    }
                }
                else
                {
                    parser.ParseXSharpArgument(ref name, ref value, "", diags);
                }
            }
            var xopts = parser.XSharpSpecificCompilationOptions;
            var result = new CSharpParseOptions().WithXSharpSpecificOptions(xopts);
            result.PreprocessorSymbols = defines;
            result.PreprocessorSymbolsUpper = definesUpper;
            return result;
        }

    }

    public partial class CSharpCompilationOptions : CompilationOptions
    {

    }
}
namespace Microsoft.CodeAnalysis
{
    //
    // Summary:
    //     Describes how severe a diagnostic is.
    public enum DiagnosticSeverity
    {
        //
        // Summary:
        //     Something that is an issue, as determined by some authority, but is not surfaced
        //     through normal means. There may be different mechanisms that act on these issues.
        Hidden = 0,
        //
        // Summary:
        //     Information that does not indicate a problem (i.e. not prescriptive).
        Info = 1,
        //
        // Summary:
        //     Something suspicious but allowed.
        Warning = 2,
        //
        // Summary:
        //     Something not allowed by the rules of the language or other authority.
        Error = 3
    }


    internal static class XSharpString
    {
        internal static bool Equals(string lhs, string rhs)
        {
            return CaseInsensitiveComparison.Equals(lhs, rhs);
        }
        internal static int Compare(string lhs, string rhs)
        {
            return String.Compare(lhs, rhs, StringComparison.OrdinalIgnoreCase);
        }
        internal static StringComparer Comparer => CaseInsensitiveComparison.Comparer;
        internal const StringComparison Comparison = StringComparison.OrdinalIgnoreCase;
        internal static bool IgnoreCase => !CaseSensitive;
        internal static bool CaseSensitive = false;
    }

    public static class CaseInsensitiveComparison
    {
        public static StringComparer Comparer => StringComparer.OrdinalIgnoreCase;
    }

    public class ParseOptions
    {

    }

    public class CommandLineArguments
    {
        public CompilationOptions CompilationOptions { get; internal set; }
        public ParseOptions ParseOptions { get; internal set; }
        protected virtual ParseOptions ParseOptionsCore => null;
        protected virtual CompilationOptions CompilationOptionsCore => null;

        public bool EmitPdb { get; set; }
    }

    public class Compilation
    {

    }

    public class CompilationOptions
    {

    }

    public class CommandLineParser
    {

        internal void AddDiagnostic(IList<Diagnostic> diag, Microsoft.CodeAnalysis.CSharp.ErrorCode error, params object[] parameters)
        {

        }

        public CSharpCommandLineArguments Parse(string[] arguments, string baseDirectory, string sdkDirectoryOpt, string additionalReferenceDirectories)
        {
            return new CSharpCommandLineArguments();
        }
        internal static string[] ParseSeparatedPaths(string path)
        {
            return path.Split(new char[] { '\\' }, StringSplitOptions.RemoveEmptyEntries);
        }
    }


    public enum SourceCodeKind
    {
        Script = 1
    }

    public struct Diagnostic
    {

    }

    public static class MessageID
    {
        public static string IDS_Text => "Text";

        public static string Localize(this string input) => input;

    }


}
