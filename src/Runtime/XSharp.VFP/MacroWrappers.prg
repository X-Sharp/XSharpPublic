//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.Internal

/// <inheritdoc cref="M:XSharp.RT.Functions.Evaluate(System.String)" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[FoxProFunction("EVALUATE", FoxFunctionCategory.General, FoxEngine.Macro, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Evaluate(cString AS STRING) AS USUAL
    RETURN XSharp.RT.Functions.Evaluate(cString)

/// <inheritdoc cref="M:XSharp.RT.Functions.Evaluate(System.String, System.Boolean)" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[FoxProFunction("EVALUATE", FoxFunctionCategory.General, FoxEngine.Macro, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Evaluate(cString AS STRING, lAllowSingleQuotes AS LOGIC) AS USUAL
    RETURN XSharp.RT.Functions.Evaluate(cString, lAllowSingleQuotes)


/// <include file="VfpDocs.xml" path="Runtimefunctions/execscript/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[FoxProFunction("EXECSCRIPT", FoxFunctionCategory.General, FoxEngine.Macro, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION ExecScript(cExpression, eParameter1, eParameter2, eParameterN) AS USUAL CLIPPER
    RETURN XSharp.RT.Functions.ExecScript(_Args())

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/type/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
[FoxProFunction("TYPE", FoxFunctionCategory.General, FoxEngine.NameResolution, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Type(cExpression AS STRING) AS STRING
    RETURN XSharp.RT.Functions.Type(cExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/type/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[FoxProFunction("TYPE", FoxFunctionCategory.General, FoxEngine.NameResolution, FoxFunctionStatus.Full, FoxCriticality.High)];
[NeedsAccessToLocals(FALSE)];
FUNCTION Type(cExpression AS STRING, nArray AS LONG) AS STRING
    RETURN XSharp.RT.Functions.Type(cExpression, nArray)

/// <inheritdoc cref="M:XSharp.RT.Functions.MExec(XSharp.Codeblock)" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MExec(oBlock AS CODEBLOCK) AS USUAL
    RETURN XSharp.RT.Functions.MExec(oBlock)

/// <inheritdoc cref="M:XSharp.RT.Functions.StrEvaluate(System.String)" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
FUNCTION StrEvaluate( cString AS STRING ) AS STRING
    RETURN XSharp.RT.Functions.StrEvaluate(cString)

/// <inheritdoc cref="M:XSharp.RT.Functions.StrEvaluate(XSharp.__Usual[])" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
FUNCTION ExecScriptFast( cExpression, eParameter1, eParameter2, eParameterN ) AS USUAL CLIPPER
    RETURN XSharp.RT.Functions.ExecScriptFast(_Args())

/// <inheritdoc cref="M:XSharp.RT.Functions.Eval(XSharp.ICodeblock,XSharp.__Usual[])"/>
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
FUNCTION Eval(block AS ICodeblock, args PARAMS USUAL[]) AS USUAL
    RETURN XSharp.RT.Functions.Eval(block, args)

/// <inheritdoc cref="M:XSharp.RT.Functions.Eval(XSharp.__Usual,XSharp.__Usual[])"/>
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
FUNCTION Eval( uCodeBlock AS USUAL, args PARAMS USUAL[] ) AS USUAL
    RETURN XSharp.RT.Functions.Eval(uCodeBlock, args)

/// <inheritdoc cref="M:XSharp.RT.Functions.Eval(System.Object,XSharp.__Usual[])"/>
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(FALSE)];
FUNCTION Eval( obj AS OBJECT,  args PARAMS USUAL[] ) AS USUAL
    RETURN XSharp.RT.Functions.Eval(obj, args)
