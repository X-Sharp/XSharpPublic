//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel
     // A variable is strictly speaking not an entity
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XSourceVariableSymbol INHERIT XSourceEntity  IMPLEMENTS IXVariableSymbol, IXSourceSymbol
        
        // Methods
        CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval, typeName AS STRING)
            SUPER(name, Kind.Local, Modifiers.None, span, position)
            SELF:TypeName       := iif (String.IsNullOrEmpty(typeName), "System.Object", typeName)
            SUPER:Parent        := parent
            IF parent != NULL
                SELF:File           := parent:File
            ENDIF
            SELF:CheckForGenericTypeName()
            
        // Properties
	
        PROPERTY Expression   AS IList<XSharpToken> AUTO GET INTERNAL SET
	
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
        PROPERTY IsArray      AS LOGIC AUTO
        PROPERTY IsTyped      AS LOGIC GET TRUE
        PROPERTY Value        AS STRING AUTO

        METHOD DebuggerDisplay() AS STRING
            VAR result := SUPER:Name
            IF SELF:IsTyped
                result += ParamTypeDesc+" "+SELF:TypeName
            ENDIF
            RETURN result

          METHOD Clone() AS IXVariableSymbol
            RETURN (IXVariableSymbol) SELF:MemberwiseClone()

    END CLASS
         

END NAMESPACE

