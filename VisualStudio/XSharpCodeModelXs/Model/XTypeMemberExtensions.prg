//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Linq
USING System.Collections.Generic

 
BEGIN NAMESPACE XSharpModel
	 
	STATIC CLASS TypeMemberExtensions
         
         STATIC METHOD GetProtoType(SELF tm as IXTypeMember) AS STRING
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
					desc := tm:Parent:Name + vars
				ELSE
					desc := tm:Name + vars
				ENDIF 
				desc := desc +  XLiterals.AsKeyWord + tm:TypeName
				RETURN desc
      
        STATIC METHOD GetComboProtoType(SELF tm as IXTypeMember) AS STRING
 				VAR vars := ""
				VAR desc := ""
				IF tm:Kind:HasParameters()
					IF ( tm:Kind == Kind.@@Constructor )
						vars := "{" + tm:ComboParameterList + "}"
					ELSE
						vars := "(" + tm:ComboParameterList + ")"
					ENDIF 
				ENDIF
				IF ( tm:Kind == Kind.@@Constructor )
					desc := tm:Parent:Name + vars
				ELSE
					desc := tm:Name + vars
				ENDIF
   			desc := desc +  XLiterals.AsKeyWord + tm:TypeName
				RETURN desc
      
      STATIC METHOD GetDescription(SELF tm as IXTypeMember) AS STRING
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
				RETURN desc + tm:GetProtoType()
            
       STATIC METHOD GetFullName(SELF tm as IXTypeMember) AS STRING
				IF (tm:Parent != NULL)
					RETURN tm:Parent:FullName +"." + tm:Name
				ENDIF
				RETURN tm:Name

      STATIC METHOD GetOverloads(SELF tm as IXTypeMember) AS IXTypeMember[]
         var result := List<IXTypeMember>{}
         IF tm:ParentType != NULL
            result:AddRange(tm:ParentType:Members:Where({ m => m.Kind == tm.Kind .and. String.Compare(m.Name, tm.Name, StringComparison.OrdinalIgnoreCase) == 0 }))
         ENDIF
         RETURN result:ToArray()         

	END CLASS
END NAMESPACE 



