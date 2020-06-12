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
   CLASS XParameterReference INHERIT XElement IMPLEMENTS IXVariable
      
      // Methods
      CONSTRUCTOR(parent AS IXMember, name AS STRING, typeName AS STRING)
         SUPER(name, Kind.Parameter, Modifiers.Public)
         SELF:TypeName      := typeName
         SELF:Parent        := parent
         
         // Properties
      PROPERTY Value        AS STRING AUTO
      PROPERTY IsArray      AS LOGIC AUTO
      PROPERTY TypeName     AS STRING AUTO
      PROPERTY IsTyped      AS LOGIC GET TRUE
      PROPERTY OriginalTypeName as STRING AUTO
      PROPERTY Description  AS STRING
         GET
            //
            LOCAL prefix AS STRING
            prefix := "PARAMETER "
            VAR result := prefix + SELF:Prototype
            result += ParamTypeDesc + SELF:TypeName + IIF(SELF:IsArray,"[]","")
            RETURN result
         END GET
      END PROPERTY
      
      PROPERTY IsParameter AS LOGIC GET TRUE
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
      
      
      METHOD DebuggerDisplay() AS STRING
         RETURN SELF:Name + " "+ParamTypeDesc+" "+SELF:TypeName
         
      METHOD ForceComplete() as VOID
      
         /*
         "Create Table Parameters ("
         " Id integer NOT NULL PRIMARY KEY, IdMember integer NOT NULL, Ordinal integer not null, "
         " Name text NOT NULL COLLATE NOCASE, ParamKind integer NOT NULL, ParamType TEXT NOT NULL COLLATE NOCASE, " 
         " DefaultValue text, "
         */
         
   END CLASS
   
   
END NAMESPACE

