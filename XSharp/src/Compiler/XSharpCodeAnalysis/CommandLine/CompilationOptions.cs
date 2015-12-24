// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents various options that affect compilation, such as 
    /// whether to emit an executable or a library, whether to optimize
    /// generated code, and so on.
    /// </summary>
    public sealed class XSharpSpecificCompilationOptions 
    {

        public XSharpSpecificCompilationOptions()
        {
        }

        public bool ArrayZero { get; internal set; } = false;
        public bool CaseSensitive { get; internal set; } = false;
        public bool CompactFramework { get; internal set; } = false;

        public string IncludePaths { get; internal set; } = "";
        public bool IlDasm { get; internal set; } = false;
        public bool ImplicitNameSpace { get; internal set; } = false;
        public bool LateBinding { get; internal set; } = false;
        public bool NoRun { get; internal set; } = true;
        public bool NoStdDef { get; internal set; } = false;
        public string NameSpace { get; set; } = "";
        public bool PeVerify { get; internal set; } = false;
        public bool PreProcessorOutput { get; internal set; } = false;
        public bool SyntaxCheck { get; internal set; } = false;
        public bool ShowIncludes { get; internal set; } = false;
        public bool Time { get; internal set; } = false;
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
    }
}