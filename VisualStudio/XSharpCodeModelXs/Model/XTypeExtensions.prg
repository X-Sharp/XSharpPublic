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
      STATIC METHOD IsGlobalType(SELF type as IXTypeSymbol) AS LOGIC
          RETURN type:Name == XLiterals.GlobalName
      STATIC METHOD GetDescription(SELF type as IXTypeSymbol) AS STRING
         VAR modVis := type:ModVis
         IF  type:IsStatic
            modVis += "STATIC "
         ENDIF
         RETURN modVis + type:Kind:ToString() + " " + type:Prototype

      STATIC METHOD GetFullName(SELF type as IXTypeSymbol) AS STRING
         LOCAL result as STRING
         result := iif(type:IsGeneric, type:GenericName, type:Name)
         IF ! String.IsNullOrEmpty(type:Namespace) .AND. type:Kind != Kind.Namespace
            result := type:Namespace + "." +result
         ENDIF
         RETURN result

      INTERNAL STATIC METHOD _GetGenericName(SELF type as IXTypeSymbol) AS STRING
            var result := type:Name
            var pos := result:IndexOfAny(<char>{'`','<'})
            if pos > 0
                result := result:Substring(0, pos)+"<"+type:TypeParameterList+">"
            endif
            return result

      STATIC METHOD GetMethods(SELF type as IXTypeSymbol, declaredOnly := false AS LOGIC) AS IXMemberSymbol[]
         if declaredOnly
               return type:GetDeclaredMembers():Where( { m => m.Kind:IsMethod()}):ToArray()
         endif
         return type:AllMembers:Where( { m => m.Kind:IsMethod() }):ToArray()

      STATIC METHOD GetDeclaredMembers( SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Parent == type}):ToArray()

        /// <summary>
        /// Return the instance constructors
        /// </summary>
        /// <param name="type">Type to search</param>
        /// <param name="declaredOnly">Only constructors of the class itself. Exclude inherited constructors?</param>
        /// <returns></returns>
      STATIC METHOD GetConstructors(SELF type as IXTypeSymbol, declaredOnly AS LOGIC) AS IXMemberSymbol[]
         if declaredOnly
               return type:GetDeclaredMembers():Where( { m => m.Kind == Kind.Constructor .and. !m.IsStatic}):ToArray()
         endif
         return type:Members:Where( { m => m.Kind == Kind.Constructor .and. !m.IsStatic }):ToArray()

        /// <summary>
        /// Return the instance constructors
        /// </summary>
        /// <param name="type">Type to search</param>
        /// <returns></returns>
      STATIC METHOD GetConstructors(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
        RETURN GetConstructors(type, FALSE)

      STATIC METHOD GetFields(SELF type as IXTypeSymbol) AS IXMemberSymbol[]
         return type:Members:Where( { m => m.Kind:IsField()}):ToArray()

      STATIC METHOD GetEvents(SELF type as IXTypeSymbol, declaredOnly := false AS LOGIC) AS IXMemberSymbol[]
         if declaredOnly
               return type:GetDeclaredMembers():Where( { m => m.Kind == Kind.Event}):ToArray()
         endif
         return type:AllMembers:Where( { m => m.Kind == Kind.Event}):ToArray()

      STATIC METHOD GetProperties(SELF type as IXTypeSymbol, declaredOnly := false AS LOGIC) AS IXMemberSymbol[]
         if declaredOnly
               return type:GetDeclaredMembers():Where( { m => m.Kind.IsProperty()}):ToArray()
         endif
         return type:AllMembers:Where( { m => m.Kind:IsProperty()}):ToArray()

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
       STATIC METHOD HasEnumerator(SELF type as IXTypeSymbol) AS LOGIC
           var interfaces  := type.Interfaces
           var hasEnumerator := false
            foreach var interf in interfaces

                if (interf.EndsWith("IEnumerable", StringComparison.OrdinalIgnoreCase))

                    hasEnumerator := true
                    exit

                elseif (interf.EndsWith("IEnumerable`1", StringComparison.OrdinalIgnoreCase))

                    hasEnumerator := true
                    exit
                elseif (interf.EndsWith("IEnumerable<T>", StringComparison.OrdinalIgnoreCase))

                    hasEnumerator := true
                    exit
                endif
            next
            if hasEnumerator
                return true
            endif
            if type:BaseType != NULL
                RETURN type:BaseType:HasEnumerator()
            ENDIF
            RETURN FALSE

   END CLASS
END NAMESPACE



