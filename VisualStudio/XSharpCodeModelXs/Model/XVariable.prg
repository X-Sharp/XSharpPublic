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
		private _typeName as string
		static initonly public VarType := "$VAR$" as string
		static initonly public UsualType := "USUAL" as string
		// Methods
		constructor(parent as XElement, name as string, kind as Kind,  ;
			span as TextRange, position as TextInterval, typeName as string,  isParameter := false as logic)
			super(name, kind, Modifiers.None, Modifiers.None, span, position)
			if String.IsNullOrEmpty(typeName)
				typeName := UsualType
			endif
			self:_typeName := typeName
			self:_isParameter := isParameter
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
				var textArray1 := <string>{prefix, self:Prototype, " as ", self:TypeName, iif(self:IsArray,"[]","")}
				return String.Concat(textArray1)
			end get
		end property
		
		property IsArray as logic auto 
		
		property Prototype as string get super:NAme
		
		property TypeName as string
			get
				return self:_typeName
			end get
			set
				if (String.IsNullOrEmpty(value))
					value := UsualType
				endif
				self:_typeName := value
			end set
		end property
		
		
	end class
	
end namespace 

