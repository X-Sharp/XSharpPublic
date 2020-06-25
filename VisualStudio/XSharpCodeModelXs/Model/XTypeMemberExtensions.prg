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
         VAR vars := ""
         VAR desc := ""
         IF tm:Kind:HasParameters()
            IF ( tm:Kind == Kind.@@Constructor )
               vars := "{" + tm:ParameterList + "}"
            ELSE
               vars := "(" + tm:ParameterList + ")"
            ENDIF 
         ENDIF
         IF tm:Kind == Kind.VODefine .OR. tm:Kind == Kind.EnumMember
            vars := " "+tm:Value
         ENDIF
         IF ( tm:Kind == Kind.@@Constructor )
            desc := tm:DeclaringType + vars
         ELSE
            desc := tm:Name + vars
         ENDIF 
         desc := desc +  XLiterals.AsKeyWord + tm:TypeName
         RETURN desc
      
      
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
         IF tm:Parameters:Count > 0
            sb:Append( "(")
            FOR var iParam := 0 to tm:Parameters:Count-1
               var param := tm:Parameters[iParam]
               IF iParam > 0
                  sb:Append(",")
               ENDIF
               if param is XParameterReference var xrefpar
                  sb:Append(xrefpar:OriginalTypeName)
               else
                  sb:Append(param:TypeName)
               endif
            NEXT
            sb:Append(")")
         ENDIF
         RETURN sb:ToString()
         
   END CLASS
END NAMESPACE 



