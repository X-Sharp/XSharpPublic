//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Linq
USING System.Reflection
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting
USING LanguageService.CodeAnalysis

CLASS XSharp.MacroCompiler IMPLEMENTS XSharp.IMacroCompiler
    STATIC options AS ScriptOptions
	STATIC cache   :=  Dictionary<STRING, XSharp.Codeblock>{} AS Dictionary<STRING, XSharp.Codeblock>
	STATIC CONSTRUCTOR
        LOCAL oMC AS IMacroCompiler
        oMC := XSharp.RuntimeState.MacroCompiler
        IF oMC == NULL
		    SetMacroCompiler (typeof(XSharp.MacroCompiler))
        ENDIF
		Appdomain.CurrentDomain:AssemblyLoad += AssemblyLoadEventHandler
		RETURN
	STATIC METHOD AssemblyLoadEventHandler(sender AS OBJECT, args AS AssemblyLoadEventArgs) AS VOID
        IF options != NULL
		    options := options:AddReferences(args:LoadedAssembly)
        ENDIF
		RETURN
    METHOD Compile (cMacro AS STRING, lAllowSingleQuotes AS LOGIC, Module AS System.Reflection.Module, lIsBlock REF LOGIC) AS ICodeBlock
        IF string.IsNullOrEmpty(cMacro)
            cMacro := "{||}"
        ENDIF
        lIsBlock := cMacro:Replace(" ",""):StartsWith("{|")
        IF cache:ContainsKey(cMacro)
	       	RETURN cache[cMacro]
        ENDIF
        IF options == NULL
            options := ScriptOptions:Default:WithReferences( ;
			System.AppDomain:CurrentDomain:GetAssemblies().Where({a => !string.IsNullOrEmpty(a:Location)}) ;
			)
        ENDIF                                         
        VAR result := XSharpMacro.Compile<XSharp.Codeblock>(cMacro, options, lAllowSingleQuotes)
        cache:Add(cMacro, result)
        RETURN result

END CLASS
