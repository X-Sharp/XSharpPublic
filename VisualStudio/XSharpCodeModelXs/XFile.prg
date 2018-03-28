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
		private _globalType as XType
		private _hasLocals as logic
		private _lastWritten as System.DateTime
		private _lock as object
		private _parsed as logic
		private _type as XFileType
		private _typeList as ConcurrentDictionary<string, XType>
		private _usings := List<string>{} as List<string>
		private _usingStatics := List<string>{} as List<string>
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
		
		method GetLocals(currentBuffer as string) as void
			local walker as SourceWalker
			walker := SourceWalker{self, currentBuffer}
			begin using walker
				var xTree := walker:Parse()
				walker:BuildModel(xTree, true)
			end using
		
		
		method InitTypeList() as void
			if self:HasCode
				self:_typeList := ConcurrentDictionary<string, XType>{System.StringComparer.InvariantCultureIgnoreCase}
				self:_globalType := XType.CreateGlobalType(self)
				self:_typeList:TryAdd(self:_globalType:Name, self:_globalType)
				self:_usings := List<string>{}
				self:_usingStatics := List<string>{}
			endif
		
		method SetTypes(types as IDictionary<string, XType>, usings as IList<string>, staticusings as IList<string>, hasLocals as logic) as void
			if self:HasCode
				System.Diagnostics.Trace.WriteLine(String.Concat("-->> XFile.SetTypes() ", System.IO.Path.GetFileName(self:SourcePath)))
				begin lock self
					self:_typeList:Clear()
					self:_usings:Clear()
					self:_usingStatics:Clear()
					self:_hasLocals := hasLocals
					foreach pair as KeyValuePair<string, XType> in types
						self:_typeList:TryAdd(pair:Key, pair:Value)
						if (XType.IsGlobalType(pair:Value))
							self:_globalType := pair:Value
						endif
					next
					self:_usings:AddRange(usings)
					self:_usings:AddRange(staticusings)
				end lock
				System.Diagnostics.Trace.WriteLine(String.Concat("<<-- XFile.SetTypes() ", System.IO.Path.GetFileName(self:SourcePath), " ", self:_typeList:Count:ToString()))
			endif
		
		method WaitParsing() as void
			local walker as SourceWalker
			//
			if self:HasCode
				
				System.Diagnostics.Trace.WriteLine("-->> XFile.WaitParsing()")
				begin lock self:_lock
					
					if (! self:Parsed)
						
						walker := SourceWalker{self}
						begin using walker
							
							try
								
								var xTree := walker:Parse()
								walker:BuildModel(xTree, false)
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
				var list := List<string>{}
				begin lock self:_lock
					
					list:AddRange(self:_usingStatics)
					if (((self:Project != null) .AND. (self:Project:ProjectNode != null)) .AND. self:Project:ProjectNode:ParseOptions:IsDialectVO)
						
						foreach info as AssemblyInfo in self:Project:AssemblyReferences
							
							var globalClassName := info:GlobalClassName
							if (! String.IsNullOrEmpty(globalClassName))
								
								list:AddUnique(globalClassName)
							endif
						next
					endif
				end lock
				System.Diagnostics.Trace.WriteLine("<<-- XFile.AllUsingStatics")
				return list:ToImmutableList()
			end get
		end property
		
		property ContentHashCode as dword
			get
				if (! self:HasCode .or. self:TypeList == null)
					return 0
				endif
				begin lock self:_lock
					
					var num2 := 0U
					foreach type as XType in self:TypeList:Values
						
						foreach xmem as XTypeMember in type:Members
							
							num2 := num2 + (dword)xmem:Prototype:GetHashCode() 
						next
					next
					return num2
				end lock
			end get
		end property
		
		property FullPath as string get self:filePath set self:filePath := value
		property GlobalType as XType get self:_globalType
		property HasCode as logic get self:IsSource .OR. self:IsXaml
		property HasLocals as logic get self:_hasLocals
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

