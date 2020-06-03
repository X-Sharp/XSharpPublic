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
    STATIC METHOD GetDescription(SELF type as IXType) AS STRING
     	VAR modVis := type:ModVis
      IF  type:IsStatic
         modVis += "STATIC "
      ENDIF
      RETURN modVis + type:Kind:ToString() + " " + type:Prototype
   
   STATIC METHOD GetFullName(SELF type as IXType) AS STRING
      IF ! String.IsNullOrEmpty(type:NameSpace)
         RETURN type:NameSpace + "." + type:Name
      ENDIF
      RETURN type:Name
   
   STATIC METHOD GetMethods(SELF type as IXType) AS IXTypeMember[]
      return type:Members:Where( { m => m.Kind == Kind.Method }):ToArray()

   STATIC METHOD GetFields(SELF type as IXType) AS IXTypeMember[]
      return type:Members:Where( { m => m.Kind == Kind.Field}):ToArray()

   STATIC METHOD GetEvents(SELF type as IXType) AS IXTypeMember[]
      return type:Members:Where( { m => m.Kind == Kind.Event}):ToArray()

   STATIC METHOD GetProperties(SELF type as IXType) AS IXTypeMember[]
      return type:Members:Where( { m => m.Kind == Kind.Property .or. m.Kind == Kind.Access}):ToArray()

   STATIC METHOD GetMember(SELF type as IXType, strName as STRING) AS IXTypeMember[]
      return type:Members:Where( { m => m.Name == strName}):ToArray()

   STATIC METHOD GetMethod(SELF type as IXType, strName as STRING) AS IXTypeMember[]
      return type:Members:Where( { m => m.Name == strName .and. m.Kind == Kind.Method}):ToArray()

   STATIC METHOD GetProperty(SELF type as IXType, strName as STRING) AS IXTypeMember[]
      return type:Members:Where( { m => m.Name == strName .and. (m.Kind == Kind.Property .or. m.Kind == Kind.Access .or. m.Kind == Kind.Assign)}):ToArray()

   STATIC METHOD GetField(SELF type as IXType, strName as STRING) AS IXTypeMember[]
      return type:Members:Where( { m => m.Name == strName .and. m.Kind == Kind.Field}):ToArray()


	END CLASS
END NAMESPACE 



