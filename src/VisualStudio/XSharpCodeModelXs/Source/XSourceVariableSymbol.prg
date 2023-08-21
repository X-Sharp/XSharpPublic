//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
     // A variable is strictly speaking not an entity
    /// <summary>Local Variable Symbol in the source</summary>
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XSourceVariableSymbol INHERIT XSourceEntity  IMPLEMENTS IXVariableSymbol, IXSourceSymbol

        // Methods
        CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval, typeName AS STRING)
            SUPER(name, Kind.Local, Modifiers.None, span, position)
            SELF:TypeName       := typeName
            SUPER:Parent        := parent
            SELF:LocalType      := LocalType.As
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
                    result += LocalTypeDesc + SELF:TypeName + IIF(SELF:IsArray,"[]","")
                ENDIF
                RETURN result
            END GET
        END PROPERTY


        PROPERTY IsIs        AS LOGIC AUTO
        PROPERTY IsParameter AS LOGIC GET FALSE
        PROPERTY LocalType   AS LocalType AUTO
        PROPERTY LocalTypeDesc as STRING
            GET
                SWITCH LocalType
                CASE LocalType.As
                    RETURN XLiterals.AsKeyWord
                OTHERWISE
                    RETURN XLiterals.IsKeyWord
                END SWITCH
            END GET
        END PROPERTY

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
        PROPERTY Value        AS STRING AUTO

        METHOD DebuggerDisplay() AS STRING
            VAR result := SUPER:Name
            IF SELF:IsTyped
                result += LocalTypeDesc+" "+SELF:TypeName
            ENDIF
            RETURN result

          METHOD Clone() AS IXVariableSymbol
            RETURN (IXVariableSymbol) SELF:MemberwiseClone()

    END CLASS
    CLASS XSourceUndeclaredVariableSymbol INHERIT XSourceVariableSymbol

        // Methods
        CONSTRUCTOR(parent AS XSourceEntity, name AS STRING, span AS TextRange, position AS TextInterval)
        SUPER(parent, name, span, position,"USUAL")
        SELF:Kind := Kind.Undeclared

    END CLASS

END NAMESPACE


