//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel
[DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XSignature
        PROPERTY Id             AS STRING                   AUTO GET INTERNAL SET
        PROPERTY TypeParameters AS List<STRING>             AUTO GET INTERNAL SET
        PROPERTY Parameters     AS List<XVariable>          AUTO GET INTERNAL SET
        PROPERTY TypeParameterContraints AS LIST<STRING>    AUTO GET INTERNAL SET
        PROPERTY CallingConvention       AS STRING          AUTO GET INTERNAL SET
        PROPERTY DataType       AS STRING                   AUTO GET INTERNAL SET
        CONSTRUCTOR()
            SELF:TypeParameters             := List<STRING>{}
            SELF:Parameters                 := List<XVariable>{}
            SELF:TypeParameterContraints    := List<STRING>{}

        METHOD DebuggerDisplay() AS STRING
            LOCAL res AS STRING
            res := SELF:Id
            IF SELF:TypeParameters:Count > 0
                res += "<"
                FOREACH VAR par IN SELF:TypeParameters
                    res += par +","
                NEXT
                res := res:Substring(0, res:Length-1)
                res += ">"
            ENDIF
            IF SELF:Parameters:Count > 0
                res += "("
                FOREACH VAR par IN SELF:Parameters
                    res += par:Name
                    IF par:IsTyped
                        res += " "+par:ParamTypeDesc+" "+par:TypeName
                    ENDIF
                    res += ","
                NEXT
                res := res:Substring(0, res:Length-1)
                res += ")"
            ENDIF
            IF SELF:TypeParameterContraints:Count > 0
                FOREACH VAR par IN SELF:TypeParameterContraints
                    res += " " + par
                NEXT
            ENDIF
            RETURN res
                
    END CLASS
END NAMESPACE
