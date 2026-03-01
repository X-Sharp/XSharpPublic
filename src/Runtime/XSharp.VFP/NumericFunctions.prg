//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mton/*" />
[FoxProFunction("MTON", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION MToN( mExpression AS CURRENCY) AS FLOAT
    RETURN (FLOAT) mExpression

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ntom/*" />
[FoxProFunction("NTOM", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION NToM( nExpression AS FLOAT) AS CURRENCY
    RETURN (CURRENCY) nExpression

/// <include file="VFPDocs.xml" path="Runtimefunctions/ceiling/*" />
[FoxProFunction("CEILING", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Ceiling (nExpression) AS USUAL CLIPPER
    RETURN Ceil(nExpression)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sign/*" />
[FoxProFunction("SIGN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Sign( nExpression AS USUAL) AS LONG
    EnforceNumeric(REF nExpression)
    IF nExpression < 0
        RETURN -1
    ELSEIF nExpression  > 0
        RETURN 1
    ENDIF
    RETURN 0
