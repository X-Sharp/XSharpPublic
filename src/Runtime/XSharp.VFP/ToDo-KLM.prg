// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma options("vo15", on)
/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/keymatch/*" />
[FoxProFunction("KEYMATCH", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION KeyMatch(eIndexKey , nIndexNumber , uArea)  AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/locfile/*" />
// Example: = LOCFILE("","PRG File:prg;Compiled:fxp;Backup:bak","Bestand")
[FoxProFunction("LOCFILE", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION LocFile( cFileName , cFileExtensions , cFileNameCaption ) AS STRING
    THROW NotImplementedException{}
    // RETURN ""


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/lookup/*" />
[FoxProFunction("LOOKUP", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION Lookup( ReturnField, eSearchExpression, SearchedField , cTagName) AS USUAL
    THROW NotImplementedException{}
    // RETURN NIL

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/maketransactable/*" />
[FoxProFunction("MAKETRANSACTABLE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION MakeTransactable( uArea ) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE


