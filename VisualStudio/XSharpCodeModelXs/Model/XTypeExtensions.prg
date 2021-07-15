//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Linq

BEGIN NAMESPACE XSharpModel

   STATIC CLASS XTypeExtensions
      STATIC METHOD GetDescription(SELF type as IXTypeSymbol) AS STRING
         VAR modVis := type:ModVis
         IF  type:IsStatic
            modVis += "STATIC "
         ENDIF
         RETURN modVis + type:Kind:ToString() + " " + type:Prototype

      STATIC METHOD GetFullName(SELF type as IXTypeSymbol) AS STRING
         IF ! String.IsNullOrEmpty(type:Namespace) .AND. type:Kind != Kind.Namespace
            var result := type:Namespace + "." + type:Name
            RETURN result
         ENDIF
         RETURN type:Name

      STATIC METHOD GetMethods(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Kind:IsMethod() }):ToArray()

      STATIC METHOD GetFields(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Kind:IsField()}):ToArray()

      STATIC METHOD GetEvents(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Kind == Kind.Event}):ToArray()

      STATIC METHOD GetProperties(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Kind:IsProperty()}):ToArray()

      STATIC METHOD GetMethods(SELF type AS IXTypeSymbol, strName AS STRING) AS IXMemberSymbol[]
         RETURN type:GetMembers(strName, TRUE):Where( { m=> m.Kind:IsMethod()}):ToArray()

      STATIC METHOD GetProperties(SELF type as IXTypeSymbol, strName as STRING) AS IXMemberSymbol[]
         RETURN type:GetMembers(strName, TRUE):Where( { m=> m.Kind:IsProperty()}):ToArray()

      STATIC METHOD GetFields(SELF type AS IXTypeSymbol, strName AS STRING) AS IXMemberSymbol[]
         RETURN type:GetMembers(strName, TRUE):Where( { m=> m.Kind:IsField()}):ToArray()

      STATIC METHOD GetXmlSignature(SELF tm as IXTypeSymbol) AS STRING
         // todo: need to handle type parameters !
         IF tm is XPETypeSymbol VAR peType
            RETURN "T:"+peType:OriginalTypeName
         ENDIF
         RETURN "T:"+tm:FullName
      STATIC METHOD IsVoStruct(SELF type AS IXTypeSymbol) AS LOGIC
        IF (type == NULL)
            RETURN FALSE
        ENDIF
        SWITCH type:Kind
        CASE Kind.VOStruct
        CASE Kind.Union
            RETURN TRUE
        CASE Kind.Structure
            IF type IS XPETypeSymbol VAR peType
                RETURN peType:HasCustomAttribute("vostruct")
            ENDIF
        END SWITCH
        RETURN FALSE
   END CLASS
END NAMESPACE



