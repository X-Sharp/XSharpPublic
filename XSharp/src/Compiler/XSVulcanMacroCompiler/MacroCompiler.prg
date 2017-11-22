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
USING Vulcan
CLASS XSharp.MacroCompiler IMPLEMENTS Vulcan.Runtime.IMacroCompiler
    STATIC options AS ScriptOptions
	STATIC cache   :=  Dictionary<STRING, Vulcan.Codeblock>{} AS Dictionary<STRING, Vulcan.Codeblock>
	STATIC METHOD Initialize() AS VOID
		RETURN
	STATIC CONSTRUCTOR
		SetMacroCompiler(typeof( XSharp.MacroCompiler)) 
		Appdomain.CurrentDomain:AssemblyLoad += AssemblyLoadEventHandler
		RETURN
	STATIC METHOD AssemblyLoadEventHandler(sender as OBJECT, args as AssemblyLoadEventArgs) AS VOID
		IF options == null
            options := ScriptOptions:Default:WithReferences( ;
			System.AppDomain:CurrentDomain:GetAssemblies().Where({a => !string.IsNullOrEmpty(a:Location)}) ;
			)
		ENDIF
		options := options:AddReferences(args:LoadedAssembly)
		RETURN
    METHOD Compile (cMacro AS STRING, lAllowSingleQuotes AS LOGIC, Module AS System.Reflection.Module, lIsBlock REF LOGIC) AS Vulcan.Runtime.ICodeBlock
        IF string.IsNullOrEmpty(cMacro)
            cMacro := "{||}"
        ENDIF
        IF options == NULL
            options := ScriptOptions:Default:WithReferences( ;
			System.AppDomain:CurrentDomain:GetAssemblies().Where({a => !string.IsNullOrEmpty(a:Location)}) ;
			)
        ENDIF                                         
        IF cache:ContainsKey(cMacro)
	       	RETURN cache[cMacro]
        ENDIF
        VAR result := XSharpMacro.Compile<Vulcan.Codeblock>(cMacro, options, lAllowSingleQuotes)
        //cache:Add(cMacro, result)
        RETURN result

END CLASS