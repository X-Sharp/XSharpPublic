//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Reflection
BEGIN NAMESPACE XSharp
    /// <include file="XSharp.Core.Docs.xml" path="doc/ICodeblock/*" />
    interface ICodeblock
        /// <include file="XSharp.Core.Docs.xml" path="doc/ICodeblock.EvalBlock/*" />
        METHOD	EvalBlock( args PARAMS OBJECT[]) AS OBJECT
        /// <include file="XSharp.Core.Docs.xml" path="doc/ICodeblock.PCount/*" />
        METHOD	PCount AS LONG
    END INTERFACE


    /// <include file="XSharp.Core.Docs.xml" path="doc/ICodeblock2/*" />
    INTERFACE ICodeblock2 INHERIT ICodeblock
        /// <include file="XSharp.Core.Docs.xml" path="doc/ICodeblock2.ResultType/*" />
        property ResultType as __UsualType get
    END INTERFACE


    /// <include file="XSharp.Core.Docs.xml" path="doc/MacroCompilerIncludeAssemblyInCache/*" />
    DELEGATE MacroCompilerIncludeAssemblyInCache(ass as Assembly) AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/MacroCompilerResolveAmbiguousMatch/*" />
    DELEGATE MacroCompilerResolveAmbiguousMatch(m1 AS MemberInfo, m2 AS MemberInfo, args AS System.Type[]) AS LONG

    /// <include file="XSharp.Core.Docs.xml" path="doc/MacroCompilerErrorHandler/*" />
    DELEGATE MacroCompilerErrorHandler(cMacro as STRING, oEx as Exception) AS ICodeblock

 /// <include file="XSharp.Core.Docs.xml" path="doc/IMacroCompiler2/*" />
	INTERFACE IMacroCompiler2 INHERIT IMacroCompiler
        /// <include file="XSharp.Core.Docs.xml" path="doc/IMacroCompiler2.Resolver/*" />
        PROPERTY Resolver AS MacroCompilerResolveAmbiguousMatch GET SET
    END INTERFACE

 /// <include file="XSharp.Core.Docs.xml" path="doc/IMacroCompiler/*" />
	INTERFACE IMacroCompiler
  /// <include file="XSharp.Core.Docs.xml" path="doc/IMacroCompiler.Compile/*" />
		METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module AS System.Reflection.Module, ;
            isCodeblock OUT LOGIC, addsMemVars OUT LOGIC) AS ICodeblock
    END INTERFACE

END NAMESPACE
