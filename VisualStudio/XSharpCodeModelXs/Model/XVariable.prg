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
    CLASS XVariable INHERIT XEntityDefinition IMPLEMENTS IXVariable
        
        // Methods
        CONSTRUCTOR(parent AS XEntityDefinition, name AS STRING, span AS TextRange, position AS TextInterval, typeName AS STRING)
            SUPER(name, Kind.Local, Modifiers.None, span, position)
            SELF:TypeName       := typeName
            SUPER:Parent        := parent
            IF parent != NULL
                SELF:File           := parent:File
            ENDIF

        // Properties
	
        PROPERTY Expression   AS IList<IToken> AUTO GET INTERNAL SET
	
        PROPERTY FullName     AS STRING GET SELF:TypeName
	
        PROPERTY Description  AS STRING
            GET
                //
                LOCAL prefix AS STRING
                prefix := SELF:Kind:ToString():ToUpper() +" "
                VAR result := prefix + SELF:Prototype
                IF SELF:IsTyped
                    result += ParamTypeDesc + SELF:TypeName + IIF(SELF:IsArray,"[]","")
                ENDIF
                RETURN result
            END GET
        END PROPERTY



        PROPERTY IsParameter AS LOGIC GET FALSE
        PROPERTY ParamType AS ParamType AUTO
        PROPERTY Prototype AS STRING 
            GET 
               VAR result := SUPER:Name
               IF SELF:Kind == Kind.DbField
                  IF !String.IsNullOrEmpty(SELF:Value)
                     result := SELF:Value+"->"+result
                  ENDIF
                  result := "FIELD "+result
               ENDIF
               RETURN result
            END GET
        END PROPERTY
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


        PROPERTY ShortTypeName AS STRING
            GET
                VAR cType := SELF:TypeName
                VAR nPos := cType:LastIndexOf(".")
                IF (nPos >= 0)
                    cType := cType:Substring(nPos+1)
                ENDIF
                RETURN cType
            END GET
        END PROPERTY


        METHOD DebuggerDisplay() AS STRING
            VAR result := SUPER:Name
            IF SELF:IsTyped
                result += ParamTypeDesc+" "+SELF:TypeName
            ENDIF
            RETURN result

    END CLASS
    CLASS XParameter INHERIT XVariable

        CONSTRUCTOR(parent AS XEntityDefinition, name AS STRING, span AS TextRange, position AS TextInterval, parameterType AS STRING)
            SUPER(parent, name, span, position, parameterType)
            SELF:Kind := Kind.Parameter
	    
        PROPERTY IsParameter AS LOGIC GET TRUE

   END CLASS
         

END NAMESPACE

