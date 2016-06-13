/*
   Copyright 2016 XSharp B.V.

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
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    public sealed partial class CSharpParseOptions
    {

        // Options that can be set from the outside

        public bool ArrayZero { get; private set; }

        public bool DebugEnabled { get; private set; }
        public XSharpDialect Dialect { get; private set; }
        public string DefaultIncludeDir { get; private set; }
        public string WindowsDir { get; private set; }
        public string SystemDir { get; private set; }
        public bool NoStdDef { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        public bool VOFloatConstants { get; private set; }
        public bool VONullStrings { get; private set; }
        public bool VOClipperCallingConvention { get; private set; }
        public bool VOCompatibleIIF { get; private set; }

        public string DefaultNamespace { get; private set; }
        public bool IsDialectVO { get { return this.Dialect == XSharpDialect.VO || this.Dialect == XSharpDialect.Vulcan; } }
        public bool SupportsMemvars { get { return this.Dialect != XSharpDialect.Vulcan; } }
        public ImmutableArray<string> IncludePaths { get; private set; } = ImmutableArray.Create<string>();
        public bool VulcanRTFuncsIncluded { get; private set; } = false;
        public bool VulcanRTIncluded { get; private set; } = false;
        public bool VOUntypedAllowed { get; private set; } = true;

        public CSharpCommandLineArguments CommandLineArguments { get; private set; }

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                Dialect = opt.Dialect;
                VirtualInstanceMethods = opt.Vo3;
                DefaultNamespace = opt.NameSpace;
                DefaultIncludeDir = opt.DefaultIncludeDir;
                WindowsDir = opt.WindowsDir;
                SystemDir = opt.SystemDir;
                NoStdDef = opt.NoStdDef;
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToImmutableArray();
                VOFloatConstants = opt.Vo14;
                VONullStrings = opt.Vo2;
                VOClipperCallingConvention = opt.Vo5;
                VOCompatibleIIF = opt.Vo10;
                VOUntypedAllowed = opt.Vo15;
                VulcanRTFuncsIncluded = opt.VulcanRTFuncsIncluded;
                VulcanRTIncluded = opt.VulcanRTIncluded;
            }
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {
            if (opt != null)
            {
                DebugEnabled = opt.EmitPdb;
                CommandLineArguments = opt;
            }
        }

        public void SetXSharpSpecificOptions(CSharpParseOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            DebugEnabled = opt.DebugEnabled;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            VirtualInstanceMethods = opt.VirtualInstanceMethods;
            DefaultNamespace = opt.DefaultNamespace;
            IncludePaths = opt.IncludePaths;
        }
    }
}