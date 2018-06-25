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
		private _entityList		as List<XElement>
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

		delegate FindMemberComparer (oElement as XELement, nValue as long ) as long
		method FindMember(oDel as FindMemberComparer, nValue as LONG) as XElement
			local oResult := null_object as XElement
			local oLast as XElement
			// perform binary search to speed up things
			var current := 0
            var bottom := 0
            var top := _entityList:Count
			oLast := _entityList:FirstOrDefault()
            do while top - bottom > 1
				// determine middle
                current := (bottom + top) / 2
				var oElement := _entityList[current]
				var result := oDel(oElement, nValue)
				if result == 0
					// found
					return oElement
				elseif result = 1 // element is after the search point
					top := current
				else		// element is before the search point
					oLast := oElement
					bottom := current
				endif
			enddo
			if oResult == null
				oResult := oLast	// the last entity we saw before the selected line
			endif
			return oResult

		private method CompareByLine(oElement as XELement, nLine as LONG) as LONG
			local nResult as long
			local nStart, nEnd as long
			nStart := oElement:Range:StartLine 
			nEnd   := oElement:Range:EndLine 
			if oElement is XType
				var oType := oElement astype XType
				if oType:Members:Count > 0
					nEnd := oType:Members[0]:Range:StartLine-1
				endif
			endif
			if nStart <= nLine .and. nEnd>= nLine
				nResult := 0
			elseif nStart > nLine
				nResult := 1
			else 
				nResult := -1
			endif
			return nResult

		method FindMemberAtRow(nLine as long) as XElement
			nLine += 1
			return self:FindMember(CompareByLine, nLine)
								
		///
		/// <Summary>Find member in file based on 0 based position</Summary>
		///
		///
		private method CompareByPosition(oElement as XELement, nPos as LONG) as LONG
			local nResult as long
			local nStart, nEnd as long
			nStart := oElement:Interval:Start 
			nEnd   := oElement:Interval:Stop 
			if oElement is XType
				var oType := oElement astype XType
				if oType:Members:Count > 0
					nEnd := oType:Members[0]:Interval:Start-2
				endif
			endif
			if nStart <= nPos .and. nEnd >= nPos
				nResult := 0
			elseif nStart > nPos
				nResult := 1
			else 
				nResult := -1
			endif
			return nResult

		method FindMemberAtPosition(nPos as long) as XElement
			return self:FindMember(CompareByPosition, nPos)

		
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
					self:_usingStatics:AddRange(staticusings)
					self:_entityList:Clear()
					self:_entityList:AddRange(aEntities)
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
				self:Project:RemoveMergedType(oType)
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
								var lines := System.IO.File.ReadAllLines(self:SourcePath)								
								var info := walker:Parse(lines, false)
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
		property AllUsingStatics as IList<string>
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
				return statics
			end get
		end property
		
		property ContentHashCode as dword
			get
				if ! self:HasCode .or. self:TypeList == null
					return 0
				endif
				begin lock self:_lock
					var hash := 0U
					foreach type as XType in self:TypeList:Values
						FOREACH xmem AS XTypeMember IN type:Members
							begin unchecked
								hash += (DWORD)xmem:Prototype:GetHashCode() 
								hash += (DWORD) xmem:Range:StartLine
							end unchecked
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
		END PROPERTY

		property TypeList as IDictionary<string, XType>
			get
				if ! self:HasCode
					return null
				endif
				begin lock self:_lock
					return self:_typeList
				end lock
			end get
		end property
		
		property Usings as IList<string>
			get
				if ! self:HasCode
					return null
				endif
				return _usings:ToArray()
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


