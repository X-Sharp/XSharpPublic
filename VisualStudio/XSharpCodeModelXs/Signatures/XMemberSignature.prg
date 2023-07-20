//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq
using XSharp.Settings

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{DebuggerDisplay(),nq}")];
   CLASS XMemberSignature  INHERIT XBaseSignature

      PROPERTY Id                       AS STRING                   AUTO
      PROPERTY Parameters               AS List<IXParameterSymbol>  AUTO
      PROPERTY CallingConvention        AS CallingConvention        AUTO
      PROPERTY DataType                 AS STRING                   AUTO
      PROPERTY IsExtension              AS LOGIC                    AUTO

      PROPERTY HasParameters AS LOGIC GET SELF:Parameters:Count > 0
      PROPERTY ParameterCount  AS INT GET SELF:Parameters:Count
      PROPERTY ParameterList AS STRING
         GET
            VAR parameters := ""
            var isExt := SELF:IsExtension
            FOREACH variable AS IXParameterSymbol IN SELF:Parameters
               IF (parameters:Length > 0)
                  parameters := parameters + ", "
               ENDIF
               if isExt
                   parameters += "SELF "
                   isExt := FALSE
               endif
               parameters += XLiterals.EscapeName(variable:Name)
               IF !String.IsNullOrEmpty(variable:Value)
                  parameters += " := "+variable:Value
               ENDIF
               IF variable:IsTyped
                  parameters += variable:ParamTypeDesc + variable:TypeName:GetXSharpTypeName()
               ENDIF
            NEXT
            RETURN parameters
         END GET
      END PROPERTY

      CONSTRUCTOR()
         SUPER()
         SELF:TypeParameters             := List<STRING>{}
         SELF:Parameters                 := List<IXParameterSymbol>{}
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
                  res += " "+par:ParamTypeDesc+" "+par:TypeName:GetXSharpTypeName()
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
      METHOD Clone() AS XMemberSignature
         VAR oClone := (XMemberSignature) self:MemberwiseClone()
         oClone:Parameters := List<IXParameterSymbol>{}
         FOREACH var param in SELF:Parameters
             oClone:Parameters:Add( (IXParameterSymbol) param:Clone())
         NEXT
         RETURN oClone
   END CLASS
END NAMESPACE
