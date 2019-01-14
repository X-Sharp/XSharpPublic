/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using System.IO;
namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents various options that affect compilation, such as
    /// whether to emit an executable or a library, whether to optimize
    /// generated code, and so on.
    /// </summary>
    public sealed class XSharpSpecificCompilationOptions
    {
        public static readonly XSharpSpecificCompilationOptions Default = new XSharpSpecificCompilationOptions();

        static string _defaultIncludeDir;
        static string _windir;
        static string _sysdir;
        public static void SetDefaultIncludeDir (string dir)
        {
            _defaultIncludeDir = dir;
        }
        public static void SetWinDir(string dir)
        {
            _windir = dir;
        }
        public static void SetSysDir(string dir)
        {
            _sysdir = dir;
        }
        public XSharpSpecificCompilationOptions()
        {
            // All defaults are set at property level
        }

        public bool ArrayZero { get; internal set; } = false;
        public bool CaseSensitive { get; internal set; } = false;
        public bool ClrVersionWasSet { get; internal set; } = false;
        public int ClrVersion { get; internal set; } = 4;
        public string DefaultIncludeDir { get; internal set; } = _defaultIncludeDir;
        public XSharpDialect Dialect { get; internal set; } = XSharpDialect.Core;
        public string WindowsDir { get; internal set; } = _windir;
        public string SystemDir { get; internal set; } = _sysdir;
        public string IncludePaths { get; internal set; } = "";
        public bool ImplicitNameSpace { get; internal set; } = false;
        public bool LateBinding { get; internal set; } = false;
        public bool AllowNamedArguments { get; internal set; } = false;
        public bool NamedArgsHasBeenSet { get; internal set; } = false;
        public bool NoClipCall { get; internal set; } = false;
        public bool NoStdDef { get; internal set; } = false;
        public string NameSpace { get; set; } = "";
        public ParseLevel ParseLevel { get;  set; } = ParseLevel.Complete;
        public bool PreProcessorOutput { get; internal set; } = false;
        public bool SaveAsCSharp { get; internal set; } = false;
        public bool DumpAST { get; internal set; } = false;
        public bool ShowDefs { get; internal set; } = false;
        public bool ShowIncludes { get; internal set; } = false;
        public string StdDefs { get; internal set; } = "XSharpDefs.xh";
        public XSharpTargetDLL TargetDLL { get; internal set; } = XSharpTargetDLL.Other;
        public bool Verbose { get; internal set; } = false;
        public bool Vo1 { get; internal set; } = false;
        public bool Vo2 { get; internal set; } = false;
        public bool Vo3 { get; internal set; } = false;
        public bool Vo4 { get; internal set; } = false;
        public bool Vo5 { get; internal set; } = false;
        public bool Vo6 { get; internal set; } = false;
        public bool Vo7 { get; internal set; } = false;
        public bool Vo8 { get; internal set; } = false;
        public bool Vo9 { get; internal set; } = false;
        public bool Vo10 { get; internal set; } = false;
        public bool Vo11 { get; internal set; } = false;
        public bool Vo12 { get; internal set; } = false;
        public bool Vo13{ get; internal set; } = false;
        public bool Vo14 { get; internal set; } = false;
        public bool Vo15 { get; internal set; } = false;
        public bool Vo16 { get; internal set; } = false;
        public bool Xpp1 { get; internal set; } = false;
        public bool Xpp2 { get; internal set; } = false;
        public bool VulcanRTFuncsIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRTFuncs);
        public bool VulcanRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRT);
        public bool XSharpRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpRT);
        public bool XSharpVOIncluded=> RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpVO);
        public bool XSharpCoreIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        public bool XSharpXPPIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpXPP);
        public bool ExplicitVO15 { get; internal set; } = false;
        internal RuntimeAssemblies RuntimeAssemblies { get; set; } = RuntimeAssemblies.None;
        public bool Overflow { get; internal set; } = false;
        public bool OverflowHasBeenSet { get; internal set; } = false;
        public bool MemVars { get; internal set; } = false;
        public bool UndeclaredLocalVars { get; internal set; } = false;

        public string PreviousArgument { get; internal set; } = string.Empty;
        public TextWriter ConsoleOutput { get; internal set; }
    }
}
