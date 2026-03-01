//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options("vo15", on)


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/imestatus/*" />
[FoxProFunction("IMESTATUS", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION ImeStatus( nExpression ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/indbc/*" />
[FoxProFunction("INDBC", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION InDbc( cDatabaseObjectName, cType ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/indexseek/*" />
[FoxProFunction("INDEXSEEK", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION IndexSeek( eExpression , lMovePointer , uArea, uIndex) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/inputbox/*" />
[FoxProFunction("INPUTBOX", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION InputBox( cInputPrompt , cDialogCaption , cDefaultValue , nTimeout ,cTimeoutValue,cCancelValue) AS STRING
    THROW NotImplementedException{}
    // RETURN ""

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/isexclusive/*" />
[FoxProFunction("ISEXCLUSIVE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION IsExclusive( uArea, nType) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ismemofetched/*" />
[FoxProFunction("ISMEMOFETCHED", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION IsMemoFetched( uField , uArea) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ispen/*" />
[FoxProFunction("ISPEN", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION IsPen( ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/istransactable/*" />
[FoxProFunction("ISTRANSACTABLE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION IsTransactable( uArea ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

