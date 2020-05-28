//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XVariable INHERIT XElement
        // Fields
        PRIVATE _isParameter AS LOGIC
        // Methods
        CONSTRUCTOR(parent AS XElement, name AS STRING, kind AS Kind,  ;
            span AS TextRange, position AS TextInterval, typeName AS STRING, isParameter := FALSE AS LOGIC)
            SUPER(name, kind, Modifiers.None, Modifiers.None, span, position)
            SELF:TypeName       := typeName
            SELF:_isParameter   := isParameter
            SELF:VarDefinition  := NULL	// Not a VAR
            SUPER:Parent        := parent
            IF parent != NULL
                SELF:File           := parent:File
            ENDIF

        // Properties
        PROPERTY Description AS STRING
            GET
                //
                LOCAL prefix AS STRING
                IF (SELF:_isParameter)
                    //
                    prefix := "PARAMETER "
                ELSE
                    //
                    prefix := "LOCAL "
                ENDIF
                VAR result := prefix + SELF:Prototype
                IF (SELF:IsTyped)
                    result += ParamTypeDesc + SELF:TypeName + IIF(SELF:IsArray,"[]","")
                ENDIF
                RETURN result
            END GET
        END PROPERTY

		PROPERTY IsParameter AS LOGIC GET _isParameter
        PROPERTY ParamType AS ParamType AUTO
        PROPERTY Prototype AS STRING GET SUPER:Name
        PROPERTY ParamTypeDesc AS STRING
            GET
                SWITCH ParamType
                CASE ParamType.Ref
                    RETURN RefKeyWord
                CASE ParamType.Out
                    RETURN OutKeyWord
                CASE ParamType.Params
                    RETURN ParamsKeyWord
                OTHERWISE // AS and IN
                    RETURN AsKeyWord
                END SWITCH
            END GET
        END PROPERTY


        PROPERTY ShortTypeName AS STRING
            GET
                VAR cType := SELF:TypeName
                VAR nPos := cType:LastIndexOf(".")
                IF (nPos >= 0)
                    cType := cType:SubString(nPos+1)
                ENDIF
                RETURN cType
            END GET
        END PROPERTY

        PROPERTY VarDefinition AS ParseContext AUTO

        METHOD DebuggerDisplay() AS STRING
            VAR result := SUPER:Name
            IF SELF:IsTyped
                result += paramTypeDesc+" "+SELF:TypeName
            ENDIF
            RETURN result

    END CLASS

END NAMESPACE

