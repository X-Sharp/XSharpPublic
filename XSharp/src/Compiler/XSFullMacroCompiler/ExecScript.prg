//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System.Linq
USING System.Collections.Generic
USING System.Collections.Concurrent
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting
USING XSharp

CLASS ScriptParameters
    PUBLIC __XSHARP__PszLizt := List<IntPtr>{} AS List<IntPtr>
    PUBLIC __XSHARP__PrivatesLevel AS INT
    PUBLIC __XSHARP__Args AS USUAL[]
    PUBLIC PROPERTY __XSHARP__PCount AS INT GET __XSHARP__Args:Length
END CLASS

FUNCTION CompileScript(xsource AS STRING) AS Script          
VAR options :=  XSharpMacro.XsFoxOptions
SWITCH RuntimeState.Dialect
CASE XSharp.XSharpDialect.Core                    
    options :=  XSharpMacro.XsCoreOptions
CASE XSharp.XSharpDialect.Vulcan
    options :=  XSharpMacro.XsVulcanOptions
CASE XSharp.XSharpDialect.FoxPro
   options :=  XSharpMacro.XsFoxOptions   
CASE XSharp.XSharpDialect.VO                  
CASE XSharp.XSharpDialect.XPP                 
CASE XSharp.XSharpDialect.Harbour                 
OTHERWISE 
    options :=  XSharpMacro.XsVoOptions
END SWITCH
VAR references := System.AppDomain.CurrentDomain:GetAssemblies() ;
    :Where({a => !string.IsNullOrEmpty(a:Location)})
VAR scoptions := ScriptOptions.Default ;
    :WithXSharpSpecificOptions(options) ;
    :WithReferences(references) ;
    :WithImports(<STRING>{"System","System.Text","System.Collections.Generic","System.Linq"})

VAR xscript := XSharpScript.Create(xsource, scoptions, typeof(ScriptParameters)) //globalsType := NULL)
xscript:Compile()
RETURN xscript

GLOBAL CompiledScripts := ConcurrentDictionary<STRING,Script>{} AS ConcurrentDictionary<STRING,Script>

FUNCTION ExecScript(source AS STRING, args PARAMS USUAL[]) AS USUAL
    LOCAL script AS Script
    IF CompiledScripts:ContainsKey(source)
        script := CompiledScripts[source]
    ELSE                  
        TRY
            script := CompileScript(source)
        CATCH e AS Exception
            THROW  e
        END TRY
        CompiledScripts:TryAdd(source,script)
    END
    LOCAL res AS USUAL
    VAR ScriptArgs := ScriptParameters{}
    ScriptArgs:__XSHARP__Args := args
    ScriptArgs:__XSHARP__PrivatesLevel := XSharp.RT.Functions.__MemVarInit()
    TRY
        res := script:RunAsync(ScriptArgs):Result:ReturnValue
    FINALLY
        XSharp.RT.Functions.__MemVarRelease(ScriptArgs:__XSHARP__PrivatesLevel)
        XSharp.Internal.CompilerServices.String2PszRelease(ScriptArgs:__XSHARP__PszLizt)
        ScriptArgs:__XSHARP__PszLizt.Clear()
    END
RETURN res
