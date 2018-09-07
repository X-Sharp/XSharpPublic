//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{Prototype,nq}")];
	CLASS XVariable INHERIT XElement
		// Fields
		PRIVATE _isParameter AS LOGIC
		PRIVATE _isTyped     AS LOGIC
		PRIVATE _typeName AS STRING
		STATIC INITONLY PUBLIC VarType := "$VAR$" AS STRING
		STATIC INITONLY PUBLIC UsualType := "USUAL" AS STRING
		// Methods
		CONSTRUCTOR(parent AS XElement, name AS STRING, kind AS Kind,  ;
			span AS TextRange, position AS TextInterval, typeName AS STRING,  isParameter := false AS LOGIC)
			SUPER(name, kind, Modifiers.None, Modifiers.None, span, position)
			SELF:_typeName		:= typeName
			SELF:_isParameter	:= isParameter
			SELF:_isTyped		:= !String.IsNullOrEmpty(typeName)
			SUPER:Parent := parent
		
		
		// Properties
		PROPERTY Description AS STRING
			GET
				//
				LOCAL prefix AS STRING
				IF (SELF:_isParameter)
					//
					prefix := "PARAMETER "
				ELSE
					//
					prefix := "LOCAL "
				ENDIF
				VAR result := prefix + SELF:Prototype
				IF (_isTyped)
					result += AsKeyWord + SELF:TypeName + IIF(SELF:IsArray,"[]","")
				ENDIF
				RETURN result				
			END GET
		END PROPERTY
		
		PROPERTY IsArray AS LOGIC AUTO 
		PROPERTY IsTyped AS LOGIC GET _isTyped
		PROPERTY ParamType AS ParamType AUTO
		PROPERTY Prototype AS STRING GET SUPER:Name
		PROPERTY ParamTypeDesc AS STRING
			GET
				SWITCH ParamType
				CASE ParamType.Ref
					RETURN RefKeyWord 
				CASE ParamType.Out
					RETURN OutKeyWord 
				CASE ParamType.Params
					RETURN ParamsKeyWord 
				OTHERWISE
					RETURN AsKeyWord 
				END SWITCH
			END GET
		END PROPERTY
		
		PROPERTY TypeName AS STRING
			GET
				IF IsTyped
					RETURN SELF:_typeName
				ELSE
					RETURN UsualType
				ENDIF
			END GET
			SET
				SELF:_typeName := VALUE
				_isTyped := String.IsNullOrEmpty(_typeName)
			END SET
		END PROPERTY
		PROPERTY ShortTypeName AS STRING
			GET
				VAR cType := SELF:TypeName
				VAR nPos := cType:LastIndexOf(".")
				IF (nPos >= 0)
					cType := cType:SubString(nPos+1)
				ENDIF
				RETURN cType
			END GET
		END PROPERTY		
		
	END CLASS
	
END NAMESPACE 

