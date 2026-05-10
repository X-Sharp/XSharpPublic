//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma options("vo15", on)

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/newobject/*" />
[FoxProFunction("NEWOBJECT", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION NewObject( cClassName ,_args) AS OBJECT
    THROW NotImplementedException{}
    // RETURN NULL_OBJECT

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/oldval/*" />
[FoxProFunction("OLDVAL", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION OldVal( cExpression , uArea ) AS USUAL
    THROW NotImplementedException{}
    // RETURN NIL

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/primary/*" />
// Note needs Database Container support
[FoxProFunction("PRIMARY", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION Primary( nIndexNumber, uArea) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/prtinfo/*" />
[FoxProFunction("PRTINFO", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION PrtInfo( nPrinterSetting , cPrinterName ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/putfile/*" />
[FoxProFunction("PUTFILE", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION PutFile( cCustomText , cFileName, cFileExtensions) AS STRING
    THROW NotImplementedException{}
    // RETURN ""

