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
      
      STATIC METHOD GetProtoType(SELF tm as IXMember) AS STRING
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
               vars:Append(tm:ParameterList)
               vars:Append("}")
            ELSE
               vars:Append("(")
               vars:Append(tm:ParameterList)
               vars:Append(")")
            ENDIF 
         ENDIF
         IF tm:Kind == Kind.VODefine .OR. tm:Kind == Kind.EnumMember
            vars:Append(" "+tm:Value)
         ENDIF
         
         IF ( tm:Kind == Kind.@@Constructor )
            vars:Insert(0, tm:DeclaringType )
         ELSE
            vars:Insert(0, tm:Name )
         ENDIF 
         vars:Append(XLiterals.AsKeyWord)
         vars:Append(tm:TypeName)
         RETURN vars:ToString()
      
      
      STATIC METHOD GetDescription(SELF tm AS IXMember) AS STRING
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
      
      STATIC METHOD GetFullName(SELF tm as IXMember) AS STRING
         IF (tm:Parent != NULL)
            RETURN tm:Parent:FullName +"." + tm:Name
         ENDIF
         RETURN tm:Name
         
      STATIC METHOD GetOverloads(SELF tm as IXMember) AS IXMember[]
         var result := List<IXMember>{}
         IF tm:ParentType != NULL
            result:AddRange(tm:ParentType:GetMembers(tm.Name, TRUE))
         ENDIF
         RETURN result:ToArray()         
         
      STATIC METHOD GetXmlSignature(SELF tm as IXMember) AS STRING
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
               IF param IS XParameterReference VAR xrefpar
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
      
   END CLASS
END NAMESPACE 



