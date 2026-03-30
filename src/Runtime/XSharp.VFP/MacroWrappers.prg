//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.Internal

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/evaluate/*" />
[NeedsAccessToLocals(TRUE)];
[FoxProFunction("EVALUATE", FoxFunctionCategory.General, FoxEngine.Macro, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Evaluate(cString AS STRING) AS USUAL
    RETURN XSharp.RT.Functions.Evaluate(cString)


/// <include file="VfpDocs.xml" path="Runtimefunctions/execscript/*" />
[FoxProFunction("EXECSCRIPT", FoxFunctionCategory.General, FoxEngine.Macro, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION ExecScript(cExpression, eParameter1, eParameter2, eParameterN) AS USUAL CLIPPER
    RETURN XSharp.RT.Functions.ExecScript(cExpression, eParameter1, eParameter2, eParameterN)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/type/*" />
[NeedsAccessToLocals(FALSE)];
[FoxProFunction("TYPE", FoxFunctionCategory.General, FoxEngine.NameResolution, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Type(cString AS STRING) AS STRING
    RETURN XSharp.RT.Functions.Type(cString)
