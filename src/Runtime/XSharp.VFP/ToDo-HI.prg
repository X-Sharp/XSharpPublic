//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options("vo15", on)

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/indbc/*" />
[FoxProFunction("INDBC", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION InDbc( cDatabaseObjectName, cType ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/inputbox/*" />
[FoxProFunction("INPUTBOX", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION InputBox( cInputPrompt , cDialogCaption , cDefaultValue , nTimeout ,cTimeoutValue,cCancelValue) AS STRING
    THROW NotImplementedException{}
    // RETURN ""

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/istransactable/*" />
[FoxProFunction("ISTRANSACTABLE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION IsTransactable( uArea ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

