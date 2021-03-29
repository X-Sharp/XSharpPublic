//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
     // A variable is strictly speaking not an entity
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XSourceImpliedVariableSymbol INHERIT XSourceVariableSymbol
        
        CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval)
            SUPER(parent, name, span, position, XLiterals.VarType)
            SELF:ImpliedKind := ImpliedKind.None

        METHOD Clone() AS IXVariableSymbol
            RETURN (IXVariableSymbol) SELF:MemberwiseClone()

        // Properties
        PROPERTY IsParameter  AS LOGIC AUTO
        PROPERTY ImpliedKind as ImpliedKind AUTO
        PROPERTY Collection   AS IXSymbol AUTO


    END CLASS
         

END NAMESPACE

