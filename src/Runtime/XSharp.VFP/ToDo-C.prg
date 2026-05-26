//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#pragma options("vo15", on)

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/candidate/*" />
[FoxProFunction("CANDIDATE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION Candidate (nIndexNumber , uArea)
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cpconvert/*" />
[FoxProFunction("CPCONVERT", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION CpConvert ( nCurrentCodePage, nNewCodePage, cExpression)
    THROW NotImplementedException{}
    // RETURN ""

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cpdbf/*" />
[Obsolete( "This function will not be supported" )];
[FoxProFunction("CPCURRENT", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.NotSupported, FoxCriticality.Medium)];
FUNCTION CpCurrent( ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/createbinary/*" />
[Obsolete( "This function will not be supported" )];
[FoxProFunction("CREATEBINARY", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.NotSupported, FoxCriticality.Medium)];
FUNCTION CREATEBINARY( ) AS STRING
    THROW NotImplementedException{}
    // RETURN ""

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/createoffline/*" />
[FoxProFunction("CREATEOFFLINE", FoxFunctionCategory.Database, FoxEngine.SQL, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION  CreateOffline (ViewName , cPath)
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cursortoxml/*" />
[FoxProFunction("CURSORTOXML", FoxFunctionCategory.General, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION CursorToXML (uArea, cOutput, nOutputFormat, nFlags, nRecords, cSchemaName, cSchemaLocation, cNameSpace )
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/curval/*" />
[FoxProFunction("CURVAL", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION CurVal(cExpression, uArea)
    THROW NotImplementedException{}
    // RETURN NIL




