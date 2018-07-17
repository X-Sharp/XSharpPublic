//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Diagnostics
using System.Linq
using System.Text
using System.Threading.Tasks
using XSharpModel
begin namespace XSharpModel
	[DebuggerDisplay("{FullName,nq}")];
	class CompletionType
		// Fields
		private _file	 as XFile
		private _stype	 as System.Type	
		private _xtype	 as XType
		
		// Methods
		constructor()
			super()
			self:_stype := null
			self:_xtype := null
			self:_file := null
		
		
		constructor(sType as System.Type)
			self()
			self:_stype := sType
		
		constructor(element as XElement)
			self()
			local oMember as XTypeMember
			local parent as XTypeMember
			self:_file := element:file
			if element is XType
				self:_xtype := (XType)element 
			else
				if element is XTypeMember

					oMember := (XTypeMember)(element)
					self:CheckType(oMember:TypeName, oMember:file, oMember:Parent:NameSpace)
				else

					if element:Parent is XType
	
						self:_xtype := (XType)(element:Parent)
					else
	
						parent := (XTypeMember)(element:Parent)
						if (parent != null)
		
							self:CheckType(parent:TypeName, parent:file, parent:Parent:NameSpace)
						endif
					endif
				endif
			endif
		
		constructor(xType as XType)
			self()
			self:_xtype := xType
			self:_file  := xType:File
		
		constructor(element as XTypeMember)
			self()
			self:_file := element:file
			if element:Kind:HasReturnType()
				self:CheckType(element:TypeName, element:file, element:Parent:NameSpace)
			else
				self:_xtype := (XType) element:Parent
			endif
		
		constructor(xvar as XVariable, defaultNS as string)
			self()
			local parent as XTypeMember
			parent := (XTypeMember)(xvar:Parent)
			self:_file := xvar:file
			if (parent != null)
				//
				if (! String.IsNullOrEmpty(parent:Parent:NameSpace))

					defaultNS := ((XType)parent:Parent):NameSpace
				endif
				self:CheckType(xvar:TypeName, parent:file, defaultNS)
			endif
		
		constructor(typeName as string, xFile as XFile, usings as IList<string>)
			self()
			self:_file := xFile
			self:CheckType(typeName, xFile, usings)
		
		constructor(typeName as string, xFile as XFile, defaultNS as string)
			self()
			self:CheckType(typeName, xFile, defaultNS)
		
		private method CheckProjectType(typeName as string, xprj as XProject, usings as IList<string>) as void
			local xType as XType
			xType := xprj:Lookup(typeName, true)
			if xType == null .AND. usings != null

				foreach name as string in usings:Expanded()
					var fqn := name + "." + typeName
					xType := xprj:Lookup(fqn, true)
					if (xType != null)
						exit
					endif
				next
			ENDIF
			if xType != null
				self:_xtype := xType
			endif
		
		private method CheckSystemType(typeName as string, usings as IList<string>) as void
			local sType as System.Type
			if self:_file != null
				typeName := typeName:GetSystemTypeName()
				sType := self:_file:Project:FindSystemType(typeName, usings)
			endif
			if sType != null
				self:_stype := sType
			endif
		
		private method CheckType(typeName as string, xFile as XFile, usings as IList<string>) as void
			//
			self:_file := xFile
			LOCAL stype AS System.Type
			// prevent lookup from simple types
			stype := SimpleTypeToSystemType(typeName)
			IF sType != null
				_sType := sType
			elseif self:_file?:Project != null
				//
				self:CheckProjectType(typeName, xFile:Project, usings)
				if ! self:IsInitialized

					self:CheckSystemType(typeName, usings)
					if ! self:IsInitialized
	
						foreach prj as XProject in xFile:Project:ReferencedProjects
							self:CheckProjectType(typeName, prj, usings)
							if self:IsInitialized
								exit
							endif
						next
					endif
				endif
			endif
		
		private method CheckType(typeName as string, xFile as XFile, defaultNS as string) as void
			local usings as List<string>
			usings := List<string>{xFile:Usings}
			if ! String.IsNullOrEmpty(defaultNS)
				usings:Add(defaultNS)
			endif
			self:CheckType(typeName, xFile, usings)
		
		internal method SimpleTypeToSystemType(kw as string) as System.Type
			//
			if (kw != null)
				//
				switch kw:ToLowerInvariant()
					CASE "object"
					case "system.object"
						RETURN TYPEOF(OBJECT)

					CASE "string"
					case "system.string"
						return typeof(string)
					
					case "dword"
					CASE "uint32"
					case "system.uint32"
						return typeof(dword)
					CASE "int64"
					case "system.int64"
						return typeof(int64)
					
					case "int16"
					case "shortint"
					CASE "short"
					case "system.int16"
						return typeof(short)
					case "longint"
					case "long"
					case "int"
					CASE "int32"
					case "system.int32"
						return typeof(long)
					case "void"
					CASE "system.void"
						return typeof(void)
					CASE "byte"
					case "system.byte"
						return typeof(byte)
					
					case "word"
					case "uint16"
					CASE "system.uint16"
						RETURN TYPEOF(WORD)

					CASE "char"
					CASE "system.char"
						return typeof(char)
					
					case "real4"
						return typeof(real4)
					
					case "real8"
						return typeof(real8)
					
					case "uint64"
					case "system.uint64"
						return typeof(uint64)
					
					CASE "logic"
					case "system.boolean"
						return typeof(logic)
					
					case "sbyte"
					case "system.sbyte"
						return typeof(SByte)
					
				end switch
			endif
			return null
		
		// Properties
		property File as XFile get self:_file
		
		property FullName as string
			get
				if (self:_xtype != null)
					return self:_xtype:FullName
				endif
				if (self:_stype != null)
					return self:_stype:GetXSharpTypeName()
				endif
				return null
			end get
		end property
		
		property IsInitialized as logic get self:_stype != null .OR. self:_xtype != null 
		
		property ParentType as CompletionType
			get
				if (self:_stype != null)
					return CompletionType{self:_stype:BaseType}
				endif
				if (self:_xtype != null)

					if (self:_xtype:Parent != null)
	
						return CompletionType{self:_xtype:Parent}
					endif
					if (self:_xtype:ParentName  !=null)
	
						var defaultNS := ""
						if (! String.IsNullOrEmpty(self:_xtype:NameSpace))
		
							defaultNS := self:_xtype:NameSpace
						endif
						return CompletionType{self:_xtype:ParentName, self:_xtype:File, defaultNS}
					endif
				endif
				return CompletionType{"System.Object", null, ""}
			end get
		end property
		
		property SType as System.Type get self:_stype
		property XType as XType get self:_xtype
		
	end class
	
end namespace 


