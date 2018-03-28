//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Diagnostics
using XSharpModel
begin namespace XSharpModel
	[DebuggerDisplay("{Prototype,nq}")];
		class XTypeMember inherit XElement
		// Fields
		private _locals as List<XVariable>
		private _parameters as List<XVariable>
		private _typeName as string
		
		// Methods
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, span as TextRange, position as TextInterval, isStatic as logic)
			
			super(name, kind, modifiers, visibility, span, position)
			//
			self:Parent := null
			self:_parameters := List<XVariable>{}
			self:_locals := List<XVariable>{}
			self:_typeName := ""
			super:_isStatic := isStatic
		
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, span as TextRange, position as TextInterval, typeName as string, isStatic as logic)
			self(name, kind, modifiers, visibility, span, position, isStatic)
			//
			self:_typeName := typeName
		
		method Namesake() as List<XTypeMember>
			var list := List<XTypeMember>{}
			if (self:Parent != null)
				foreach var oMember in ((XType) self:Parent):Members
					if String.Compare(oMember:FullName, self:FullName, true) == 0 .AND. String.Compare(oMember:Prototype, self:Prototype, true) > 0
						//// 
						list:Add(oMember)
					endif
				next
			endif
			return list
		//
		// Properties
		virtual property Description as string
			get
				//
				var str := ""
				if (super:Modifiers != Modifiers.None)
					//
					str := String.Concat(str, super:Modifiers:ToString(), " ")
				endif
				var str2 := String.Concat(str, super:Visibility:ToString(), " ")
				if (super:Kind != Kind.Field)
					//
					str2 := String.Concat(str2, super:Kind:DisplayName(), " ")
					if (super:Kind == Kind.VODefine)
						//
						return String.Concat(str2, super:Name, self:Suffix)
					endif
				endif
				return String.Concat(str2, self:Prototype)
			end get
		end property
		
		virtual property FullName as string
			get
				//
				if (self:Parent != null)
					//
					return String.Concat(self:Parent:FullName, ".", super:Name)
				endif
				return super:Name
			end get
		end property
		
		property HasParameters as logic
			get
				//
				return super:Kind:HasParameters() .AND. self:Parameters:Count > 0
			end get
		end property
		
		property IsArray as logic auto 
		
		property Locals as List<XVariable>
			get
				//
				return self:_locals
			end get
		end property
		
		new property Parent as XTYPE
			get
				//
				return (XType) super:parent
			end get
			set
				//
				super:parent := value
			end set
		end property
		property ParameterList as string
			get
				//
				var str := ""
				foreach variable as XVariable in self:Parameters
					//
					if (str:Length > 1)
						//
						str := String.Concat(str, ", ")
					endif
					str := String.Concat(str, variable:Name, " as ", variable:TypeName)
				next
				return str
			end get
		end property
		
		property Parameters as List<XVariable>
			get
				//
				return self:_parameters
			end get
		end property
		
		
		virtual property Prototype as string
			get
				//
				var str := ""
				if self:Kind:HasParameters()
					//
					str := String.Concat("(", self:ParameterList, ")")
				endif
				var str2 := String.Concat(super:Name, str)
				if self:Kind:HasReturnType()
					//
					str2 := String.Concat(str2, " AS ", self:TypeName)
				endif
				return str2
			end get
		end property
		
		property Suffix as string auto 
		
		property TypeName as string
			get
				//
				return self:_typeName
			end get
		end property
		
		
	end class
	
end namespace 

