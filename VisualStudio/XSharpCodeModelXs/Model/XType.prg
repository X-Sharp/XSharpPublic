//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Collections.Immutable
using System.Linq
using System.Text
using System.Threading.Tasks
using EnvDTE
using LanguageService.CodeAnalysis.Text
using System.Diagnostics
using System.Collections.Immutable

begin namespace XSharpModel
    /// <summary>
    /// Model for Namespace, Class, Interface, Structure, Enum
    /// </summary>
	[DebuggerDisplay("{FullName,nq}")];
	class XType inherit XElement
		private _isPartial as logic
		private _members as List<XTypeMember>
		private _nameSpace as string
		private _parentName as string
		
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, ;
			span as TextRange, position as TextInterval)
			super(name, kind, modifiers, visibility, span, position)

			self:_members := List<XTypeMember>{}
			self:_parentName := "System.Object"
			self:_nameSpace := ""
			if modifiers:HasFlag(Modifiers.Static)
				self:_isStatic := true
			endif
			if modifiers:HasFlag(Modifiers.Partial)
				self:_isPartial := true
			endif
		

		static method create(oFile as XFile, oElement as EntityObject, oInfo as ParseResult) as XType
			local cName := oElement:cName as STRING
			local kind  := Etype2Kind(oElement:eType) as Kind
			local mods  := oElement:eModifiers:ToModifiers() as Modifiers
			local vis   := oElement:eAccessLevel:ToModifiers() as Modifiers
			local span  as TextRange
			local intv  as TextInterval
			local oResult as XType
			
			CalculateRange(oElement, oInfo, out span, out intv)
			oResult := XType{cName, kind, mods, vis, span, intv}
			oResult:File := oFile
			if oElement:eType:IsClass()
				foreach var oMember in oElement:aChildren
					local xMember as XTypeMember
					xMember := XTypeMember.create(oMember, oInfo, oFile)
					xMember:Parent := oResult
					oResult:AddMember(xMember)
				next
			endif
			return oResult


		method AddMember(oMember as XTypeMember) as void
			begin lock self:_members
				self:_members:Add(oMember)
			end lock
		
		method AddMembers(members as IEnumerable<XTypeMember>) as void
			begin lock self:_members
				self:_members:AddRange(members)
			end lock
		property Members as IImmutableList<XTypeMember>
			get
				//
				begin lock self:_members
					//
					return self:_members:ToImmutableArray()
				end lock
			end get
		end property
		
		
		method GetMember(elementName as string) as IImmutableList<XTypeMember>
			var tempMembers := List<XTypeMember>{} 
			foreach x as XTypeMember in self:Members
				if nameEquals(x:Name, elementName) 
					tempMembers:Add(x)
				endif
			next
			return tempMembers:ToImmutableArray();

		private method nameEquals(name as string, compareWith as string) as logic
			return String.Compare(name, compareWith, StringComparison.OrdinalIgnoreCase) == 0
	
	
		property FullName as string
			get
				if (! String.IsNullOrEmpty(self:_nameSpace))
					return self:NameSpace + "." + super:Name
				endif
				return super:Name
			end get
		end property
			
		
		
		
	    /// <summary>
        /// Merge two XType Objects : Used to create the resulting  XType from partial classes
        /// </summary>
        /// <param name="otherType"></param>
		method Merge(otherType as XType) as XType
			local clone as XType
			clone := self:Duplicate()
			if (String.Compare(otherType:File:FullPath, super:File:FullPath, System.StringComparison.OrdinalIgnoreCase) != 0) .OR. (super:Range:StartLine != otherType:Range:StartLine)
				self:IsPartial := true
				if otherType != null
					clone:AddMembers(otherType:Members)
					if clone:Parent == null .AND. otherType:Parent != null
						clone:Parent := otherType:Parent
					else
						if clone:ParentName == null .AND. otherType:ParentName != null
							clone:ParentName := otherType:ParentName
						endif
					endif
				endif
			endif
			return clone
		
		
		property NameSpace as string get _namespace set _namespace := value
		
		
        /// <summary>
        /// If this XType is a Partial type, return a Copy of it, merged with all other informations
        /// coming from other files.
        /// </summary>

		property Clone as XType
			get
				if (self:IsPartial)
					return super:File:Project:LookupFullName(self:FullName, true)
				endif
				return self
			end get
		end property
		
		property Description as string
			get
				var modVis := ""
				if (super:Kind == Kind.Class)
					if (super:Modifiers != Modifiers.None)
						modVis := modVis + super:Modifiers:ToString()+  " "
					endif
					modVis := modVis + super:Visibility:ToString()+ " "
				endif
			
				if (super:Kind == Kind.Keyword)
					return super:Name + " " + super:Kind:ToString()
				endif
				return modVis + super:Kind:ToString() + " " + self:Prototype
			end get
		end property
		
		
		property IsPartial as logic get self:_isPartial set self:_isPartial := value
		
		property IsType as logic
			get
				switch super:Kind
					case Kind.Class 
					case Kind.Structure 
					case Kind.VOStruct 
					case Kind.Union 
					case Kind.Interface 
					case Kind.Enum
						return true
				end switch
				return false
			end get
		end property
		
        /// <summary>
        /// Duplicate the current Object, so we have the same properties in another object
        /// </summary>
        /// <returns></returns>
		method Duplicate() as XType
			var temp := XType{super:Name, super:Kind, super:Modifiers, super:Visibility, super:Range, super:Interval} 
			temp:AddMembers(self:Members)
			return temp
		
		
		property ParentName as string
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
		
		
		static method CreateGlobalType(xfile as XFile) as XType
			var globalType := XType{GlobalName, Kind.Class, Modifiers.None, Modifiers.Public, TextRange{1, 1, 1, 1}, TextInterval{}} 
			globalType:IsPartial:=true
			globalType:IsStatic:=true
			globalType:File:=xfile
			return globalType

		static method IsGlobalType(type as XType) as logic
			return type:Name == XType.GlobalName

	end class
	
end namespace 

