//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using EnvDTE
using LanguageService.CodeAnalysis.Text
using System.Diagnostics
using System.Collections.Immutable

begin namespace XSharpModel
	[DebuggerDisplay("{FullName,nq}")];
	class XType inherit XElement
		// Fields
		private _isPartial as logic
		private _members as List<XTypeMember>
		private _nameSpace as string
		private _parentName as string
		const public GlobalName := "(Global Scope)" as string
		
		// Methods
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, span as TextRange, position as TextInterval)
			super(name, kind, modifiers, visibility, span, position)
			//
			self:_members := List<XTypeMember>{}
			self:_parentName := "System.Object"
			self:_nameSpace := ""
			if modifiers:HasFlag(Modifiers.Static)
				super:_isStatic := true
			endif
			if modifiers:HasFlag(Modifiers.Partial)
				//
				self:_isPartial := true
			endif
		
		method AddMember(oMember as XTypeMember) as void
			begin lock self:_members
				self:_members:Add(oMember)
			end lock
		
		method AddMembers(members as IEnumerable<XTypeMember>) as void
			begin lock self:_members
				self:_members:AddRange(members)
			end lock
		
		static method CreateGlobalType(xfile as XFile) as XType
			//
			var oType := XType{GlobalName, Kind.Class, Modifiers.None, Modifiers.Public, TextRange{1, 1, 1, 1}, TextInterval{}} 
			oType:IsPartial:=true
			oType:IsStatic:=true
			oType:File:=xfile
			return oType
		
		method Duplicate() as XType
			var type := XType{super:Name, super:Kind, super:Modifiers, super:Visibility, super:Range, super:Interval} 
			type:AddMembers(self:Members)
			return type
		
		method GetMember(elementName as string) as IImmutableList<XTypeMember>
			var list := List<XTypeMember>{} 
			foreach oMember as XTypeMember in self:Members
				if nameEquals(oMember:Name, elementName) 
					list:Add(oMember)
				endif
			next
			return ImmutableArray.ToImmutableArray<XTypeMember>(list)
		
		static method IsGlobalType(type as XType) as logic
			return type:Name == GlobalName
		
		method Merge(otherType as XType) as XType
			local type as XType
			type := self:Duplicate()
			if (String.Compare(otherType:File:FullPath, super:File:FullPath, System.StringComparison.OrdinalIgnoreCase) != 0) .OR. (super:Range:StartLine != otherType:Range:StartLine)
				self:IsPartial := true
				if otherType != null
					type:AddMembers(otherType:Members)
					if type:Parent == null .AND. otherType:Parent != null
						type:Parent := otherType:Parent
					else
						if type:ParentName == null .AND. otherType:ParentName != null
							type:ParentName := otherType:ParentName
						endif
					endif
				endif
			endif
			return type
		
		
		property NameSpace as string get _namespace set _namespace := value
		private method nameEquals(name as string, compareWith as string) as logic
			return name:ToLower():CompareTo(compareWith:ToLower()) == 0
		
		
		// Properties
		property Clone as XType
			get
				if (self:IsPartial)
					return super:File:Project:LookupFullName(self:FullName, true)
				endif
				return self
			end get
		end property
		
		virtual property Description as string
			get
				var str := ""
				if (super:Kind == Kind.Class)
					if (super:Modifiers != Modifiers.None)
						str := String.Concat(str, super:Modifiers:ToString(), " ")
					endif
					str := String.Concat(str, super:Visibility:ToString(), " ")
				endif
				var str2 := str
				if (super:Kind == Kind.Keyword)
					return String.Concat(super:Name, " ", super:Kind:ToString())
				endif
				return String.Concat(String.Concat(str2, super:Kind:ToString(), " "), self:Prototype)
			end get
		end property
		
		virtual property FullName as string
			get
				if (! String.IsNullOrEmpty(self:_nameSpace))
					return String.Concat(self:NameSpace, ".", super:Name)
				endif
				return super:Name
			end get
		end property
		
		property IsPartial as logic get self:_isPartial set self:_isPartial := value
		
		property IsType as logic
			get
				//

				switch super:Kind
					case Kind.Enum 
					case Kind.VOStruct 
					case Kind.Union 
					case Kind.Class 
					case Kind.Structure 
					case Kind.Interface 
						//
						return true
				end switch
				return false
			end get
		end property
		
		property Members as IImmutableList<XTypeMember>
			get
				//
				begin lock self:_members
					//
					return ImmutableArray.ToImmutableArray<XTypeMember>(self:_members)
				end lock
			end get
		end property
		
		
		
		virtual property ParentName as string
			get
				if (super:Parent != null)
					return super:Parent:Name
				endif
				if (self:_parentName != null)
					return self:_parentName
				endif
				return null
			end get
			set
				if (super:Parent != null)
					throw System.Exception{"Cannot set ParentName if Parent is not null"}
				endif
				self:_parentName := value
			end set
		end property
		
		
	end class
	
end namespace 

