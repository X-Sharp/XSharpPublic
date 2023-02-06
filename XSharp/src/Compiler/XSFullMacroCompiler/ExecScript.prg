//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This file implements the wrapper around the scripting engine for the ExecScript() code.

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

INTERNAL FUNCTION CompileScript(xsource AS STRING) AS Script
    LOCAL options AS XSharpSpecificCompilationOptions
    options := XSharpMacro.GetOptions((INT) RuntimeState.Dialect)
    options:SetOption(CompilerOption.Vo11, RuntimeState.CompilerOptionVO11)
    options:SetOption(CompilerOption.Vo13, RuntimeState.CompilerOptionVO13)
    options:SetOption(CompilerOption.Overflow, RuntimeState.CompilerOptionOVF)


    VAR references := System.AppDomain.CurrentDomain:GetAssemblies() ;
        :Where({a => !String.IsNullOrEmpty(a:Location)})
    LOCAL scoptions AS ScriptOptions
    scoptions := ScriptOptions.Default ;
        :WithXSharpSpecificOptions(options) ;
        :WithReferences(references) ;
        :WithImports(<STRING>{"System","System.Text","System.Collections.Generic","System.Linq"})

    VAR xscript := XSharpScript.Create(xsource, scoptions, typeof(ScriptParameters))
    xscript:Compile()
    RETURN xscript

INTERNAL GLOBAL CompiledScripts := ScriptCache{200} AS ScriptCache
INTERNAL GLOBAL InitialScript := NULL AS Script

INTERNAL FUNCTION GetInitialScript() AS Script
    VAR s := InitialScript
    DO WHILE s == NULL
        s := CompileScript("")
        System.Threading.Interlocked.CompareExchange(REF InitialScript, s, NULL)
        s := InitialScript
    END
    RETURN s

FUNCTION _ExecScript(source AS STRING, args PARAMS USUAL[]) AS USUAL
    LOCAL script AS Script
    IF CompiledScripts:ContainsKey(source)
        script := CompiledScripts[source]
    ELSE
        TRY
            script := GetInitialScript():ContinueWith(source)
        CATCH e AS Exception
            THROW e
        END TRY
        CompiledScripts:TryAdd(source,script)
    END
    LOCAL res AS USUAL
    VAR ScriptArgs := ScriptParameters{}
    ScriptArgs:__XSHARP__Args := args
    ScriptArgs:__XSHARP__PrivatesLevel := XSharp.RT.Functions.__MemVarInit()
    TRY
        res := script:RunAsync(ScriptArgs):Result:ReturnValue
    CATCH e as Exception
        THROW e
    FINALLY
        XSharp.RT.Functions.__MemVarRelease(ScriptArgs:__XSHARP__PrivatesLevel)
        XSharp.Internal.CompilerServices.String2PszRelease(ScriptArgs:__XSHARP__PszLizt)
        ScriptArgs:__XSHARP__PszLizt:Clear()
    END
RETURN res
