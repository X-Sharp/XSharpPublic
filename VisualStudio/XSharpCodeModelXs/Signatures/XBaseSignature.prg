//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq
using Mono.Collections.Generic

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XBaseSignature
      PROPERTY TypeParameters                AS List<STRING>   AUTO
      PROPERTY TypeParameterContraints       AS List<STRING>   AUTO
      PROPERTY TypeParameterList             AS STRING         GET ToList(SELF:TypeParameters)
      PROPERTY TypeParameterConstraintsList  AS STRING         GET ToList(SELF:TypeParameterContraints)


      CONSTRUCTOR()
         SELF:TypeParameters             := List<STRING>{}
         SELF:TypeParameterContraints    := List<STRING>{}

      STATIC METHOD ToList(list as List<STRING>) AS STRING
         var result := ""
         FOREACH var item in list
            if result:Length > 0
               result += ", "
            endif
            result += item
         NEXT
         return result

      METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:TypeParameters:Add(name)
         RETURN

      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:TypeParameterContraints:Add(name)
         RETURN

      METHOD GetTypeParameterNames() AS STRING
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
        return res

      METHOD GetTypeParameterContraints() AS STRING
         LOCAL res AS STRING
         res := ""
         IF SELF:TypeParameterContraints:Count > 0
            FOREACH VAR par IN SELF:TypeParameterContraints
               res += par + " "
            NEXT
        ENDIF
        return res:Trim()


      METHOD ToString() AS STRING
         LOCAL res AS STRING
         res := SELF:GetTypeParameterNames()
         res += " "+SELF:GetTypeParameterContraints()
         RETURN res:Trim()

      METHOD ReadGenericParameters(genericParameters as Mono.Collections.Generic.Collection<Mono.Cecil.GenericParameter> ) AS VOID
            FOREACH VAR genparam IN genericParameters
                SELF:TypeParameters:Add(genparam:Name)
                if (genparam:HasConstraints)
                    VAR cConstraint := ""
                    foreach var constraint in genparam:Constraints
                        IF cConstraint:Length > 0
                            cConstraint += ","
                        ENDIF
                        var constName := constraint:ConstraintType:Name
                        if constName:ToLower() == "valuetype"
                            cConstraint += "STRUCT"
                        else
                            cConstraint += constName
                        endif
                    next
                    SELF:TypeParameterContraints:Add(cConstraint)
                ELSE
                    SELF:TypeParameterContraints:Add("")

                ENDIF
            NEXT
   END CLASS
END NAMESPACE
