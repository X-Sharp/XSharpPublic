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
    CLASS XSourceParameterSymbol INHERIT XSourceVariableSymbol
    
        CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval, parameterType AS STRING)
            SUPER(parent, name, span, position, parameterType)
            SELF:Kind := Kind.Parameter
        
        PROPERTY IsParameter AS LOGIC GET TRUE
        
    END CLASS
    
END NAMESPACE
