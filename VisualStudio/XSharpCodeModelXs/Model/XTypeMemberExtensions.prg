//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Text
USING System.Linq
USING System.Collections.Generic


BEGIN NAMESPACE XSharpModel

   STATIC CLASS TypeMemberExtensions


      STATIC METHOD ClipperParameters(SELF tm as IXMemberSymbol) AS STRING
          IF tm:Kind:HasParameters()
                VAR names := StringBuilder{}
                FOREACH var param in tm:Parameters
                    if names:Length > 0
                        names:Append(", ")
                    endif
                    names:Append(param:Name)
                NEXT
                return names:ToString()
          ENDIF
          RETURN ""

      STATIC METHOD GetProtoType(SELF tm as IXMemberSymbol, lApplyCallingConvention := FALSE AS LOGIC) AS STRING
         VAR vars := StringBuilder{}
         IF tm:TypeParameters?:Count > 0
            VAR delim := "<"
            FOREACH VAR typeParam IN tm:TypeParameters
               vars:Append(delim)
               delim := ","
               vars:Append(typeParam)
            NEXT
            vars:Append(">")
         ENDIF
         IF tm:Kind:HasParameters()
            IF ( tm:Kind == Kind.@@Constructor )
               vars:Append("{")
               if lApplyCallingConvention .and. tm:CallingConvention == CallingConvention.Clipper
                  vars:Append(tm:ClipperParameters())
               else
                  vars:Append(tm:ParameterList)
               endif
               vars:Append("}")
            ELSE
               IF ( !tm:Kind:IsProperty() )
                  vars:Append("(")
                  if lApplyCallingConvention .and. tm:CallingConvention == CallingConvention.Clipper
                    vars:Append(tm:ClipperParameters())
                  else
                    vars:Append(tm:ParameterList)
                  endif
                  vars:Append(")")
               ENDIF
            ENDIF
         ENDIF
         IF tm:Kind == Kind.VODefine .OR. tm:Kind == Kind.EnumMember
            vars:Append(" "+tm:Value)
         ENDIF

         IF ( tm:Kind == Kind.@@Constructor )
            // System.Exception{params}
            vars:Insert(0, tm:DeclaringType )
          ELSE
            // Compare(params)
            vars:Insert(0, tm:Name )
         ENDIF
         IF (tm:Kind != Kind.@@Constructor)
            vars:Append(XLiterals.AsKeyWord)
            vars:Append(tm:TypeName:GetXSharpTypeName())
         ENDIF

         RETURN vars:ToString()


      STATIC METHOD GetDescription(SELF tm AS IXMemberSymbol) AS STRING
         VAR desc := tm:ModVis
         IF ( tm:IsStatic )
            desc += "STATIC "
         ENDIF
         IF (tm:Kind != Kind.Field)
            desc := desc + tm:Kind:ToDisplayString()
            IF (tm:Kind == Kind.VODefine)
               RETURN desc + tm:Name
            ENDIF
         ENDIF
         RETURN desc + " "+tm:GetProtoType()

      STATIC METHOD GetFullName(SELF tm as IXMemberSymbol) AS STRING
         IF (tm:Parent != NULL)
            RETURN tm:Parent:FullName +"." + tm:Name
         ENDIF
         RETURN tm:Name

      STATIC METHOD GetOverloads(SELF tm as IXMemberSymbol) AS IXMemberSymbol[]
         var result := List<IXMemberSymbol>{}
         IF tm:ParentType != NULL
            VAR overloads := tm:ParentType:GetMembers(tm.Name, TRUE)
            result:AddRange(overloads:Where( { x => x:IsStatic == tm:IsStatic}))
         ENDIF
         RETURN result:ToArray()

      STATIC METHOD GetXmlSignature(SELF tm as IXMemberSymbol) AS STRING
         local prefix as String
         local name   as String
         // todo: need to handle type parameters !
         name := tm:Name
         SWITCH tm:Kind
         CASE Kind.Method
         CASE Kind.Function
            prefix  := "M:"
         CASE Kind.Constructor
           prefix   := "M:"
            name     := "#ctor"
         CASE Kind.Destructor
            prefix   := "M:"
            name     := "Finalize"
         CASE Kind.Property
         CASE Kind.Access
         CASE Kind.Assign
            prefix  := "P:"
         CASE Kind.Event
            prefix  := "E:"
         CASE Kind.Field
            prefix  := "F:"
         OTHERWISE
            RETURN ""
         END SWITCH
         var sb := StringBuilder{}
         sb:Append(prefix)
         sb:Append(tm:ParentType:FullName)
         sb:Append("."+name)
         IF tm:TypeParameters:Count > 0
            sb:Append("``"+tm:TypeParameters:Count:ToString())
         ENDIF
         IF tm:Parameters:Count > 0
            sb:Append( "(")
            FOR var iParam := 0 to tm:Parameters:Count-1
               var param := tm:Parameters[iParam]
               IF iParam > 0
                  sb:Append(",")
               ENDIF
               IF param IS XPEParameterSymbol VAR xrefpar
                  // check if the original parameter contains a type reference
                  VAR typeName := xrefpar:OriginalTypeName
                  VAR result   := typeName
                  IF typeName:Contains("<")
                     VAR nPos := typeName:IndexOf("<")
                     result   := typeName:Substring(0, nPos)
                     typeName := typeName:Substring(nPos+1):Replace(">","")
                     nPos := result:IndexOf("`")
                     IF  nPos > 0
                        result := result:Substring(0, nPos)
                     ENDIF
                     VAR vars := typeName:Split(",":ToCharArray())
                     VAR delim := "{"
                     FOREACH VAR parName IN vars
                        result += delim
                        delim  := ","
                        nPos := tm:TypeParameters:IndexOf(parName)
                        IF nPos >= 0
                           result += "``"+nPos:ToString()
                        ELSE
                           result += parName
                        ENDIF
                     NEXT
                     result += "}"
                     sb:Append(result)
                  ELSE
                     VAR nPos := tm:TypeParameters:IndexOf(typeName)
                     IF nPos >= 0
                        sb:Append("``"+nPos:ToString())
                     ELSE
                        sb:Append(typeName)
                     ENDIF
                  ENDIF
               endif
            NEXT
            sb:Append(")")
         ENDIF
         sb:Replace('&','@')  // Ampersand is not allowed in the string
         RETURN sb:ToString()

        STATIC METHOD IsVisible(SELF tm AS IXMemberSymbol, Wanted AS Modifiers) AS LOGIC
            VAR vis := tm:Visibility
            IF vis >= Wanted
                RETURN TRUE
            ELSE
                SWITCH vis
                CASE Modifiers.ProtectedInternal
                CASE Modifiers.Internal
                    RETURN tm IS IXSourceSymbol
                OTHERWISE
                    RETURN FALSE
                END SWITCH
            ENDIF

        STATIC METHOD IsMethodVisibleInSubclass(SELF m as IXMemberSymbol) AS LOGIC
            IF m.Visibility > Modifiers.Private .and. m.Kind != Kind.Constructor
                if m:Name:Length > 4 .and. m:Kind == Kind.Method
                    SWITCH m:Name:Substring(0,4)
                    CASE "set_"
                    CASE "get_"
                    CASE "add_"
                        RETURN FALSE
                    CASE "remo"
                        RETURN ! m:Name:StartsWith("remove_")
                    END SWITCH
                endif
                RETURN TRUE
            ENDIF
            RETURN FALSE

   END CLASS
END NAMESPACE



