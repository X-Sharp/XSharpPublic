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
		private _parameters as List<XVariable>
		private _typeName as string
		
		#region constructors
			
			private  constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, span as TextRange, position as TextInterval, typeName as string, isStatic as logic)
				super(name, kind, modifiers, visibility, span, position)
				self:Parent := null
				self:_parameters := List<XVariable>{}
				self:_typeName := ""
				self:_typeName := typeName
				self:_isStatic := isStatic

			static method create(oElement as EntityObject, oInfo as ParseResult, oFile as XFile, oType as XType) as XTypeMember
				local cName := oElement:cName as string
				local kind  := Etype2Kind(oElement:eType) as Kind
				local cType := oElement:cRetType as string
				local mods  := oElement:eModifiers:ToModifiers() as Modifiers
				local vis   := oElement:eAccessLevel:ToModifiers() as Modifiers
				local span   as textRange
				local intv   as TextInterval
				local isStat := oElement:lStatic as logic
				mods &=  ~Modifiers.VisibilityMask	// remove lower 2 nibbles which contain visibility
				CalculateRange(oElement, oInfo, OUT span, OUT intv)
				local result := XTypeMember{cName, kind, mods, vis, span, intv, cType, isStat} as XTypeMember
				result:File := oFile
				if oElement:aParams != null
					foreach oParam as EntityParamsObject in oElement:aParams
						local oVar as XVariable
						span := TextRange{oElement:nStartLine, oParam:nCol, oElement:nStartLine, oParam:nCol+oParam:cName:Length}
						intv := TextInterval{oElement:nOffSet+oParam:nCol, oElement:nOffSet+oParam:nCol+oParam:cName:Length}
						oVar := XVariable{result, oParam:cName, Kind.Local,  span, intv, oParam:cType, TRUE}
						oVar:ParamType := oParam:nParamType
						result:AddParameter(oVar)
					next
				endif
				return result
			
			
		#endregion



		method AddParameter(oVar as XVariable) as VOID
			oVar:Parent := self
			oVar:File := self:File
			_parameters:Add(oVar)
			return

		method Namesake() as List<XTypeMember>
			var _namesake := List<XTypeMember>{}
			if (self:Parent != null)
				foreach  oMember as XTypeMember in ((XType) self:Parent):Members
					if String.Compare(oMember:FullName, self:FullName, true) == 0 .AND. String.Compare(oMember:Prototype, self:Prototype, true) > 0
						//// 
						_namesake:Add(oMember)
					endif
				next
			endif
			return _namesake
		//
		#region Properties
		property Description as string
			get
				var modVis := ""
				if (super:Modifiers != Modifiers.None)
					modVis := modVis + super:ModifiersKeyword
				endif
				var desc := modVis + VisibilityKeyword
				if (super:Kind != Kind.Field)
					desc := desc + super:KindKeyword
					if (super:Kind == Kind.VODefine)
						return desc + super:Name 
					endif
				endif
				return desc + self:Prototype
			end get
		end property
		
		property FullName as string
			get
				//
				if (self:Parent != null)
					//
					return self:Parent:FullName +"." + super:Name
				endif
				return super:Name
			end get
		end property
		
		property HasParameters as logic get self:Kind:HasParameters() .AND. self:_parameters:Count > 0
		property ParameterCount  as int get self:_parameters:Count
		
		property IsArray as logic auto 
		
		
		new property Parent as XType get (XType) super:parent  set super:parent := value

		property ParameterList as string
			get
				var parameters := ""
				foreach variable as XVariable in self:Parameters
					if (parameters:Length > 0)
						parameters := parameters + ", "
					ENDIF
					parameters += variable:Name 
					IF variable:IsTyped
						parameters += variable:ParamTypeDesc + variable:TypeName
					endif
				next
				return parameters
			end get
		end property

		property ComboParameterList as string
			get
				var parameters := ""
				foreach variable as XVariable in self:Parameters
					if (parameters:Length > 0)
						parameters := parameters + ", "
					ENDIF
					var cType := variable:ShortTypeName
					IF variable:IsTyped .and. variable:ParamType != ParamType.As
						parameters += variable:ParamTypeDesc + cType
					ELSE
						parameters += cType
					endif
				next
				return parameters
			end get
		end property
		
		property Parameters as IEnumerable<XVariable> 
		get  
			return self:_parameters
		end get
		end property
		
		property Prototype as string
			get
				var vars := ""
				if self:Kind:HasParameters()
					vars := "(" + self:ParameterList + ")"
				endif
				var desc := super:Name + vars
				if self:Kind:HasReturnType() .and. ! String.IsNullOrEmpty(self:TypeName)
					desc := desc + AsKeyWord + self:TypeName
				endif
				return desc
			end get
		end property
		
		property ComboPrototype as string
			get
				var vars := ""
				if self:Kind:HasParameters()
					vars := "(" + self:ComboParameterList + ")"
				endif
				var desc := super:Name + vars
				if self:Kind:HasReturnType() .and. ! String.IsNullOrEmpty(self:TypeName)
					desc := desc + AsKeyWord + self:TypeName
				endif
				return desc
			end get
		end property		
		property TypeName as string get self:_typeName
		#endregion
	end class
	
end namespace 

