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
/// <summary>Parameter Symbol in the source</summary>
[DebuggerDisplay("{DebuggerDisplay(),nq}")];
CLASS XSourceParameterSymbol INHERIT XSourceVariableSymbol IMPLEMENTS IXParameterSymbol

    CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval, parameterType AS STRING)
        SUPER(parent, name, span, position, parameterType)
        SELF:Kind := Kind.Parameter
    PROPERTY IsParameter AS LOGIC GET TRUE
    PROPERTY ParamType AS ParamType AUTO
    PROPERTY ParamTypeDesc AS STRING
    GET
        SWITCH ParamType
        CASE ParamType.Ref
            RETURN XLiterals.RefKeyWord
        CASE ParamType.Out
            RETURN XLiterals.OutKeyWord
        CASE ParamType.Params
            RETURN XLiterals.ParamsKeyWord
        OTHERWISE // AS and IN
            RETURN XLiterals.AsKeyWord
        END SWITCH
    END GET
    END PROPERTY

    METHOD DebuggerDisplay() AS STRING
        VAR result := SUPER:Name
        IF SELF:IsTyped
            result += ParamTypeDesc+" "+SELF:TypeName
        ENDIF
        RETURN result
    METHOD Resolve() AS VOID
        IF SELF:ResolvedType == null
            var name := SELF:TypeName
            SELF:ResolvedType := SELF:File:FindType(name)
            if (SELF:ResolvedType != NULL)
                SELF:TypeName     := SELF:ResolvedType:FullName
            endif
        ENDIF
END CLASS
[DebuggerDisplay("{DebuggerDisplay(),nq}")];
CLASS XSourceTypeParameterSymbol INHERIT XSourceParameterSymbol
    CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval)
        SUPER(parent, name, span, position, "")
        SELF:Kind := Kind.TypeParameter

END CLASS
END NAMESPACE
