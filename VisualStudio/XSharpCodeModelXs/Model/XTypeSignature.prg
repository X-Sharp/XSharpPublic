//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{DebuggerDisplay(),nq}")];
   CLASS XTypeSignature
      PROPERTY Interfaces               AS List<STRING>  AUTO GET INTERNAL SET
      PROPERTY TypeParameters           AS List<STRING>  AUTO GET INTERNAL SET
      PROPERTY TypeParameterContraints  AS List<STRING>  AUTO GET INTERNAL SET
      PROPERTY BaseType                 AS STRING        AUTO GET INTERNAL SET
         
      
      CONSTRUCTOR(cBaseType AS STRING)
         SELF:TypeParameters             := List<STRING>{}
         SELF:TypeParameterContraints    := List<STRING>{}
         SELF:Interfaces                 := List<STRING>{}
         SELF:BaseType                   := cBaseType
         
     METHOD AddInterface(sInterface AS STRING) AS VOID
         SELF:Interfaces:Add(sInterface)
         RETURN
         
     METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:TypeParameters:Add(name)
         RETURN
         
      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:TypeParameterContraints:Add(name)
         RETURN
          
         
      METHOD DebuggerDisplay() AS STRING
         LOCAL res AS STRING
         res := ""
         IF SELF:TypeParameters:Count > 0
            res += "<"
            FOREACH VAR par IN SELF:TypeParameters
               res += par +","
            NEXT
            res := res:Substring(0, res:Length-1)
            res += ">"
         ENDIF
         IF SELF:TypeParameterContraints:Count > 0
            FOREACH VAR par IN SELF:TypeParameterContraints
               res += " " + par
            NEXT
         ENDIF
         RETURN res
         
   END CLASS
END NAMESPACE
