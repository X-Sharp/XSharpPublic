//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Concurrent
using System.Collections.Generic
using System
using System.Linq
using System.Diagnostics
using XSharpModel
using System.Collections.Immutable
using static XSharpModel.XFileTypeHelpers
begin namespace XSharpModel
	[DebuggerDisplay("{FullPath,nq}")];
	class XFile
		// Fields
		private _globalType		as XType
		private _lastWritten	as System.DateTime
		private _lock			as object
		private _parsed			as logic
		private _type			as XFileType
		private _typeList		as ConcurrentDictionary<string, XType>
		private _entityList		as IList<XElement>
		private _usings			as List<string>
		private _usingStatics	as List<string>
		private filePath as string
		private _project as XProject
		
		// Methods
		constructor(fullPath as string)
			super()
			//
			self:filePath := fullPath
			self:_type := GetFileType(fullPath)
			self:InitTypeList()
			self:_parsed := ! self:HasCode
			self:_lock := object{}
			self:_lastWritten := System.DateTime.MinValue

		
		method FirstMember() as XTypeMember
			if (! self:HasCode)
				return null
			endif
			if (self:TypeList == null || self:TypeList:Count == 0)
				return null
			endif
			begin lock self:_lock
				var element := self:TypeList:FirstOrDefault()
				return element:Value:Members:FirstOrDefault()
			end
		///
		/// <Summary>Find member in file based on 0 based line number</Summary>
		///
		///
		method FindMemberAtRow(nLine as long) as XElement
			local oResult := null_object as XElement
			nLine += 1
			foreach oMember as XElement in _entityList
				if oMember:Range:StartLine > nLine
					exit
				endif
				if oMember:Range:StartLine <= nLine .and. oMember:Range:EndLine >= nLine
					oResult := oMember
					exit
				endif
			next
			return oResult
		///
		/// <Summary>Find member in file based on 0 based position</Summary>
		///
		///
		method FindMemberAtPosition(nPos as long) as XElement
			local oResult := null_object as XElement
			foreach oMember as XElement in _entityList
				if oMember:Interval:Start > nPos
					exit
				endif
				if oMember:Interval:Start <= nPos .and. oMember:Interval:Stop >= nPos
					oResult := oMember
					exit
				endif
			next
			return oResult

		
		method InitTypeList() as void
			if self:HasCode
				self:_typeList		:= ConcurrentDictionary<string, XType>{System.StringComparer.InvariantCultureIgnoreCase}
				self:_globalType	:= XType.CreateGlobalType(self)
				self:_typeList:TryAdd(self:_globalType:Name, self:_globalType)
				self:_usings		:= List<string>{}
				self:_usingStatics	:= List<string>{}
				self:_entityList    := List<XElement>{}
			endif
		
		method SetTypes(types as IDictionary<string, XType>, usings as IList<string>, ;
			staticusings as IList<string>, aEntities as IList<XElement>) as void
			if self:HasCode
				System.Diagnostics.Trace.WriteLine(String.Concat("-->> XFile.SetTypes() ", System.IO.Path.GetFileName(self:SourcePath)))
				begin lock self
					self:_typeList:Clear()
					self:_usings:Clear()
					self:_usingStatics:Clear()
					foreach type as KeyValuePair<string, XType> in types
						self:_typeList:TryAdd(type:Key, type:Value)
						if (XType.IsGlobalType(type:Value))
							self:_globalType := type:Value
						endif
					next
					self:_usings:AddRange(usings)
					self:_usings:AddRange(staticusings)
					self:_entityList := aEntities
				end lock
				System.Diagnostics.Trace.WriteLine(String.Concat("<<-- XFile.SetTypes() ", System.IO.Path.GetFileName(self:SourcePath), " ", self:_typeList:Count:ToString()))
			endif
		
		method BuildTypes(oInfo as ParseResult) as void
			local aTypes	      as Dictionary<string, XType>
			local aUsings		  as List<string>
			local aUsingStatics   as List<String>
			local oType		      as XType
			local aEntities		  as List<XElement>
			aTypes  := Dictionary<string, XType>{}
			aUsings			:= List<string>{}
			aUsingStatics	:= List<String>{}
			foreach oElement as EntityObject in oInfo:Types
				oType   := XType.create(self, oElement,oInfo)
				aTypes:Add( oType:Name, oType)
			next
			foreach oLine as LineObject in oInfo:SpecialLines
				if oLine:eType == LineType.Using
					local cName as string
					cName := oLine:cArgument
					if cName:ToLower():StartsWith("static")
						aUsingStatics:Add(cName:Substring(6))
					else
						aUsings:Add(cName)
					endif
				endif
			next
			// get our objects in file order from the oInfo:Entities list
			aEntities := List<XELement>{}
			foreach oElement as EntityObject in oInfo:Entities
				if oElement:oCargo != null_object
					aEntities:add ( (XElement) oElement:oCargo)
				endif
			next
			self:SetTypes(aTypes, aUsings, aUsingStatics, aEntities:ToImmutableArray())
			return


		method WaitParsing() as void
			//
			if self:HasCode
				
				System.Diagnostics.Trace.WriteLine("-->> XFile.WaitParsing()")
				begin lock self:_lock
					
					if ! self:Parsed
						begin using var walker := SourceWalker{self}
							try
								
								var info := walker:Parse()
								BuildTypes(info)						
							catch exception as System.Exception
								Support.Debug(String.Concat("XFile.WaitParsing", exception:Message), Array.Empty<object>())
							end try
						end using
					endif
				end lock
				System.Diagnostics.Trace.WriteLine("<<-- XFile.WaitParsing()")
			endif
		
		
		// Properties
		property AllUsingStatics as System.Collections.Immutable.ImmutableList<string>
			get
				
				if (! self:HasCode)
					
					return null
				endif
				System.Diagnostics.Trace.WriteLine("-->> XFile.AllUsingStatics")
				var statics := List<string>{}
				begin lock self:_lock
					
					statics:AddRange(self:_usingStatics)
					if (((self:Project != null) .AND. (self:Project:ProjectNode != null)) .AND. self:Project:ProjectNode:ParseOptions:IsDialectVO)
						
						foreach asm as AssemblyInfo in self:Project:AssemblyReferences
							
							var globalclass := asm:GlobalClassName
							if (! String.IsNullOrEmpty(globalclass))
								
								statics:AddUnique(globalclass)
							endif
						next
					endif
				end lock
				System.Diagnostics.Trace.WriteLine("<<-- XFile.AllUsingStatics")
				return statics:ToImmutableList()
			end get
		end property
		
		property ContentHashCode as dword
			get
				if (! self:HasCode .or. self:TypeList == null)
					return 0
				endif
				begin lock self:_lock
					
					var hash := 0U
					foreach type as XType in self:TypeList:Values
						
						foreach xmem as XTypeMember in type:Members
							
							hash := hash + (dword)xmem:Prototype:GetHashCode() 
						next
					next
					return hash
				end lock
			end get
		end property
		
		property FullPath as string get self:filePath set self:filePath := value
		property GlobalType as XType get self:_globalType
		property HasCode as logic get self:IsSource .OR. self:IsXaml
		property HasParseErrors as logic auto
		property IsSource as logic get self:_type == XFileType.SourceCode
		property IsXaml as logic get self:_type == XFileType.XAML
		property LastWritten as System.DateTime get self:_lastWritten set self:_lastWritten := value
		property Name as string get System.IO.Path.GetFileNameWithoutExtension(self:filePath)
		
		property Parsed as logic
			get
				System.Diagnostics.Trace.WriteLine("-->> XFile.Parsed")
				local flag as logic
				begin lock self:_lock
					
					flag := self:_parsed
				end lock
				System.Diagnostics.Trace.WriteLine("<<-- XFile.Parsed")
				return flag
			end get
		end property
		
		property Project as XProject
			get
				
				if self:_project == null
					self:_project := XSolution.OrphanedFilesProject
					self:_project:AddFile(self:filePath)
				endif
				return self:_project
			end get
			set
				self:_project := value
			end set
		end property
		
		property SourcePath as string
			get
				
				if (self:IsXaml)
					
					return self:XamlCodeBehindFile
				endif
				return self:FullPath
			end get
		end property
		
		property TypeList as System.Collections.Immutable.IImmutableDictionary<string, XType>
			get
				if ! self:HasCode
					return null
				endif
				begin lock self:_lock
					return System.Collections.Immutable.ImmutableDictionary.ToImmutableDictionary<string, XType>(self:_typeList, System.StringComparer.OrdinalIgnoreCase)
				end lock
			end get
		end property
		
		property Usings as ImmutableList<string>
			get
				if ! self:HasCode
					return null
				endif
				System.Diagnostics.Trace.WriteLine("-->> XFile.Usings")
				local list as ImmutableList<string>
				begin lock self:_lock
					list := self:_usings:ToImmutableList()
				end lock
				System.Diagnostics.Trace.WriteLine("<<-- XFile.Usings")
				return list
			end get
		end property
		
		property XamlCodeBehindFile as string
			get
				var projectNode := self:Project:ProjectNode
				return System.IO.Path.ChangeExtension(System.IO.Path.Combine(projectNode:IntermediateOutputPath, System.IO.Path.GetFileName(self:FullPath)), ".g.prg")
			end get
		end property
		
		property XFileType as XFileType get self:_type
		
	end class
	
end namespace 


