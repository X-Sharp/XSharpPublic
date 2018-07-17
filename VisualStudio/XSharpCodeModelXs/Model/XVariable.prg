//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using XSharpModel
using System.Diagnostics
begin namespace XSharpModel
	[DebuggerDisplay("{Prototype,nq}")];
	class XVariable inherit XElement
		// Fields
		private _isParameter as logic
		private _isTyped     as logic
		private _typeName as string
		static initonly public VarType := "$VAR$" as string
		static initonly public UsualType := "USUAL" as string
		// Methods
		constructor(parent as XElement, name as string, kind as Kind,  ;
			span as TextRange, position as TextInterval, typeName as string,  isParameter := false as logic)
			super(name, kind, Modifiers.None, Modifiers.None, span, position)
			self:_typeName		:= typeName
			self:_isParameter	:= isParameter
			self:_isTyped		:= !String.IsNullOrEmpty(typeName)
			super:Parent := parent
		
		
		// Properties
		property Description as string
			get
				//
				local prefix as string
				if (self:_isParameter)
					//
					prefix := "PARAMETER "
				else
					//
					prefix := "LOCAL "
				endif
				var result := prefix + self:Prototype
				if (_isTyped)
					result += AsKeyWord + self:TypeName + iif(self:IsArray,"[]","")
				endif
				return result				
			end get
		end property
		
		property IsArray as logic auto 
		property IsTyped as logic get _isTyped
		property ParamType as ParamType auto
		PROPERTY Prototype AS STRING GET SUPER:Name
		PROPERTY ParamTypeDesc AS STRING
			GET
				SWITCH ParamType
				CASE ParamType.Ref
					return RefKeyWord 
				case ParamType.Out
					return OutKeyWord 
				CASE ParamType.Params
					return ParamsKeyWord 
				otherwise
					return AsKeyWord 
				END SWITCH
			end get
		end property
		
		property TypeName as string
			get
				if IsTyped
					return self:_typeName
				else
					return UsualType
				endif
			end get
			set
				self:_typeName := value
				_isTyped := String.IsNullOrEmpty(_typeName)
			end set
		end property
		property ShortTypeName as string
			GET
				VAR cType := SELF:TypeName
				VAR nPos := cType:LastIndexOf(".")
				IF (nPos >= 0)
					cType := cType:SubString(nPos+1)
				ENDIF
				return cType
			end get
		end property		
		
	end class
	
end namespace 

