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
   CLASS XMemberSignature  INHERIT XBaseSignature
      PROPERTY Id                       AS STRING               AUTO GET INTERNAL SET
      PROPERTY Parameters               AS List<IXVariable>     AUTO GET INTERNAL SET
      PROPERTY CallingConvention        AS CallingConvention    AUTO GET INTERNAL SET
      PROPERTY DataType                 AS STRING               AUTO GET INTERNAL SET
      PROPERTY IsExtension              AS LOGIC                AUTO GET INTERNAL SET
      
      PROPERTY HasParameters AS LOGIC GET SELF:Parameters:Count > 0
      PROPERTY ParameterCount  AS INT GET SELF:Parameters:Count
      PROPERTY ParameterList AS STRING
         GET
            VAR parameters := ""
            FOREACH variable AS IXVariable IN SELF:Parameters
               IF (parameters:Length > 0)
                  parameters := parameters + ", "
               ENDIF
               parameters += variable:Name
               IF !String.IsNullOrEmpty(variable:Value)
                  parameters += " := "+variable:Value
               ENDIF
               IF variable:IsTyped
                  parameters += variable:ParamTypeDesc + variable:TypeName
               ENDIF
            NEXT
            RETURN parameters
         END GET
      END PROPERTY  
      
      CONSTRUCTOR()
         SUPER()
         SELF:TypeParameters             := List<STRING>{}
         SELF:Parameters                 := List<IXVariable>{}
         SELF:TypeParameterContraints    := List<STRING>{}
         SELF:CallingConvention          := CallingConvention.None
         
      
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
