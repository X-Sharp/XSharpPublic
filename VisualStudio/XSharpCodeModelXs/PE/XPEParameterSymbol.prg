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
   CLASS XPEParameterSymbol INHERIT XSymbol IMPLEMENTS IXParameterSymbol

      // Methods
      CONSTRUCTOR(parent AS IXMemberSymbol, name AS STRING, typeName AS STRING)
         SUPER(name, Kind.Parameter, Modifiers.Public)
         SELF:TypeName      := typeName
         SELF:Parent        := parent
         if typeName:EndsWith("&")
             SELF:TypeName := typeName:Substring(0, typeName:Length-1)
         ENDIF

         // Properties
      PROPERTY Value        AS STRING AUTO
      PROPERTY TypeName     AS STRING AUTO
      PROPERTY IsTyped      AS LOGIC GET TRUE
      PROPERTY IsStatic     AS LOGIC GET FALSE
      PROPERTY OriginalTypeName as STRING AUTO
      PROPERTY Location     AS STRING GET ""
      PROPERTY Description  AS STRING
         GET
            LOCAL prefix AS STRING
            prefix := "PARAMETER "
            VAR result := prefix + SELF:Prototype
            result += ParamTypeDesc + SELF:TypeName + IIF(SELF:IsArray,"[]","")
            RETURN result
         END GET
      END PROPERTY

      PROPERTY IsParameter AS LOGIC GET TRUE
      PROPERTY FullName as STRING GET Name
      PROPERTY Namespace as STRING GET ""
      PROPERTY ParamType AS ParamType AUTO
      PROPERTY Prototype AS STRING GET SUPER:Name
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


      INTERNAL METHOD DebuggerDisplay() AS STRING
         RETURN SELF:Name + " "+ParamTypeDesc+" "+SELF:TypeName

      METHOD ForceComplete() as VOID

         /*
         "Create Table Parameters ("
         " Id integer NOT NULL PRIMARY KEY, IdMember integer NOT NULL, Ordinal integer not null, "
         " Name text NOT NULL COLLATE NOCASE, ParamKind integer NOT NULL, ParamType TEXT NOT NULL COLLATE NOCASE, "
         " DefaultValue text, "
         */

        METHOD Clone() AS IXVariableSymbol
            RETURN (XPEParameterSymbol) SELF:MemberwiseClone()


   END CLASS

END NAMESPACE

