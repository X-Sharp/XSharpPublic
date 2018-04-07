//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Collections.Immutable
using System
using System.Diagnostics
using System.Reflection
begin namespace XSharpModel
	[DebuggerDisplay("{DisplayName,nq}")];
	class AssemblyInfo
		// Fields
		private _aExtensions as ImmutableList<MethodInfo>
		private _assembly as Assembly
		private _aTypes as IDictionary<string, System.Type>
		private _failed as long
		private _fullName as string
		private _globalClassName as string
		private _HasExtensions as logic
		private _implicitNamespaces as ImmutableList<string>
		private _LoadedTypes as logic
		private _Modified as System.DateTime
		private _nameSpaces as System.Collections.Hashtable
		private _nameSpaceTexts as ImmutableList<string>
		private _projects as ImmutableList<XProject>
		private _reference as VSLangProj.Reference
		private _zeroNamespace as AssemblyInfo.NameSpaceContainer
		
		// Methods
		constructor()
			super()
			//
			self:_fullName := ""
			self:_failed := 0
			self:_assembly := null
			self:_projects := ImmutableList<XProject>.Empty
			self:_clearInfo()
		
		constructor(reference as VSLangProj.Reference)
			self()
			self:_reference := reference
		
		constructor(_cFileName as string, _dModified as System.DateTime)
			self()
			self:FileName := _cFileName
			self:Modified := _dModified
			self:UpdateAssembly()
		
		private method _clearInfo() as void
			//
			self:_aTypes := Dictionary<string, System.Type>{System.StringComparer.OrdinalIgnoreCase}
			self:_aExtensions := ImmutableList<MethodInfo>.Empty
			self:_nameSpaces := System.Collections.Hashtable{System.StringComparer.OrdinalIgnoreCase}
			self:_nameSpaceTexts := ImmutableList<string>.Empty
			self:_implicitNamespaces := ImmutableList<string>.Empty
			self:_zeroNamespace := AssemblyInfo.NameSpaceContainer{"_"}
			self:_LoadedTypes := false
			self:_HasExtensions := false
		
		method AddProject(project as XProject) as void
			if (! self:_projects:Contains(project))
				self:_projects := self:_projects:Add(project)
			endif
		
		private method CurrentDomain_AssemblyResolve(sender as object, args as System.ResolveEventArgs) as Assembly
			var folders := List<string>{}	// list of folders that we have tried
			var folderPath := System.IO.Path.GetDirectoryName(self:FileName)
			var name := AssemblyName{args:Name}:Name + ".dll"
			var assemblyPath := System.IO.Path.Combine(folderPath, name)
			if (System.IO.File.Exists(assemblyPath))
				var assembly := AssemblyInfo.LoadAssemblyFromFile(assemblyPath)
				if (assembly != null)
					return assembly
				endif
			endif
			folders:Add(folderPath)
			foreach path as string in SystemTypeController.AssemblyFileNames
				folderPath := System.IO.Path.GetDirectoryName(path)
				if ! folders:Contains(folderPath)
					assemblyPath := System.IO.Path.Combine(folderPath, name)
					if System.IO.File.Exists(assemblyPath)
						var asm := Assembly.LoadFrom(path)
						if asm != null
							return asm
						endif
					endif
					folders:Add(folderPath)
				endif
			next
			return null
		
		method GetType(name as string) as System.Type
			if self:IsModifiedOnDisk
				self:LoadAssembly()
			endif
			if self:_assembly != null .AND. self:_aTypes:Count == 0
				self:UpdateAssembly()
			endif
			if self:_aTypes:ContainsKey(name)
				return self:Types:Item[name]
			endif
			return null
		
		private method GetTypeTypesFromType(oType as System.Type) as AssemblyInfo.TypeTypes
			if oType:IsValueType
				return AssemblyInfo.TypeTypes.Structure
			endif
			if oType:IsInterface
				return AssemblyInfo.TypeTypes.Interface
			endif
			if typeof(System.Delegate):IsAssignableFrom(oType)
				return AssemblyInfo.TypeTypes.Delegate
			endif
			return AssemblyInfo.TypeTypes.Class
		
		private static method HasExtensionAttribute(memberInfo as MemberInfo) as logic
			try
				var customAttributes := memberInfo:GetCustomAttributes(false)
				foreach var custattr in customAttributes
					if custattr:ToString() == "System.Runtime.CompilerServices.ExtensionAttribute"
						return true
					endif
				next
			catch 
				// Failed to retrieve custom attributes
			end try
			return false
		
		internal method LoadAssembly() as void
			if String.IsNullOrEmpty(self:FileName) .AND. self:_reference != null
				self:FileName := self:_reference:Path
			endif
			if System.IO.File.Exists(self:FileName)
				self:_assembly := AssemblyInfo.LoadAssemblyFromFile(self:FileName)
				if self:_assembly != null
					self:_fullName := self:_assembly:FullName
					self:Modified := System.IO.File.GetLastWriteTime(self:FileName)
				endif
				self:_clearInfo()
			endif
		
		static method LoadAssemblyFromFile(fileName as string) as Assembly
			local result  as Assembly
			if System.IO.File.Exists(fileName)
				try
					var input := System.IO.FileStream{fileName, System.IO.FileMode.Open, System.IO.FileAccess.Read}
					var rawAssembly := System.IO.BinaryReader{input}:ReadBytes((long)input:Length )
					if rawAssembly:Length != input:Length
							// Error ?
					endif
					input:Close()
					var cPdb := System.IO.Path.ChangeExtension(fileName, ".pdb")
					var cTmp := System.IO.Path.ChangeExtension(fileName, ".p$$")
					local renamed := false as logic
					if System.IO.File.Exists(cPdb)
						renamed := true
						if (System.IO.File.Exists(cTmp))
							System.IO.File.Delete(cTmp)
						endif
						System.IO.File.Move(cPdb, cTmp)
					endif
					result := Assembly.Load(rawAssembly)
					if (renamed .AND. System.IO.File.Exists(cTmp))
						//
						System.IO.File.Move(cTmp, cPdb)
					endif
					input:Dispose()
					return result
				catch exception as System.Exception
					//
					Support.Debug("Generic exception:", Array.Empty<object>())
					Support.Debug(exception:Message, Array.Empty<object>())
				end try
			endif
			return null
		
		method RemoveProject(project as XProject) as void
			//
			if self:_projects:Contains(project)
				self:_projects := self:_projects:Remove(project)
			endif

		internal method UpdateAssembly() as void
			local aTypes as Dictionary<string, System.Type>
			local nspace as string
			local fullName as string
			local simpleName as string
			local message as string
			local types := null as System.Type[]
			local index as long
			//
			if (self:_failed > 3)
				return
			endif
			aTypes := Dictionary<string, System.Type>{System.StringComparer.OrdinalIgnoreCase}
			_aExtensions := ImmutableList<MethodInfo>.Empty
			fullName := ""
			simpleName := ""
			self:_nameSpaces:Clear()
			_nameSpaceTexts := ImmutableList<String>.Empty
			self:_zeroNamespace:Clear()
			self:_globalClassName := ""
			self:_HasExtensions := false
			self:LoadAssembly()

			var currentDomain := System.AppDomain.CurrentDomain
			currentDomain:AssemblyResolve += System.ResolveEventHandler{ self, @CurrentDomain_AssemblyResolve() }
			try
				if self:_assembly != null
					var customAttributes := self:_assembly:GetCustomAttributes(false)
					local found := 0 as int
					foreach var custattr in customAttributes
						//
						var type := custattr:GetType()
						switch custattr:ToString()
							case "Vulcan.Internal.VulcanClassLibraryAttribute"
								self:_globalClassName := type:GetProperty("globalClassName"):GetValue(custattr, null):ToString()
								var defaultNs := type:GetProperty("defaultNamespace"):GetValue(custattr, null):ToString()
								if ! String.IsNullOrEmpty(defaultNs)
									self:_implicitNamespaces := self:_implicitNamespaces:Add(defaultNs)
								endif
								found += 1
							case "System.Runtime.CompilerServices.ExtensionAttribute"
								self:_HasExtensions  := true
								found += 2
							case "Vulcan.VulcanImplicitNamespaceAttribute"
								var ns := type:GetProperty("Namespace"):GetValue(custattr, null):ToString()
								if ! String.IsNullOrEmpty(ns)
									self:_implicitNamespaces := self:_implicitNamespaces:Add(ns)
								endif
								found += 4
						end switch
						// All attributes found then get out of here
						if found == 7
							exit
						endif
					next
						
				endif
			catch e as Exception
				// failed to load custom attributes					
				Support.Debug(e:Message)
			end try
			try
				if self:_assembly != null
					types := self:_assembly:GetTypes()
				endif
				self:_failed := 0
			catch exception as ReflectionTypeLoadException
				//
				Support.Debug("Cannot load types from "+ self:_assembly:GetName():Name)
				Support.Debug("Exception details:")
				message := null
				foreach var exception2 in exception:LoaderExceptions
					if exception2:Message != message
						Support.Debug(exception2:Message)
						message := exception2:Message
					endif
				next
				Support.Debug("Types loaded:")
				foreach var t in exception:Types
					if t != null
						Support.Debug(t:FullName)
					endif
				next
				self:_assembly := null
				self:_failed++
			catch ex as System.Exception
				//
				Support.Debug("Generic exception:")
				Support.Debug(ex:Message)
			end try
			currentDomain:AssemblyResolve -= System.ResolveEventHandler{ self, @CurrentDomain_AssemblyResolve() }
				
			if types != null .and. types:Length != 0 .AND. (aTypes:Count == 0  .or. ! _LoadedTypes)
				try
					foreach var type in types
						fullName := type:FullName
						if ! fullName:StartsWith("$") .AND. ! fullName:StartsWith("<")
							if self:_HasExtensions .AND. AssemblyInfo.HasExtensionAttribute(type)
								var methods := type:GetMethods(BindingFlags.Public | BindingFlags.Static)
								foreach info as MethodInfo in methods
									if AssemblyInfo.HasExtensionAttribute(info)
										_aExtensions := _aExtensions:Add(info)
									endif
								next
							endif
							//
		                    // Nested Type ?
							if fullName:Contains("+")
								fullName := fullName:Replace('+', '.')
							endif
							// Generic ?
							if fullName:Contains("`")
								fullName := fullName:Substring(0, fullName:IndexOf("`") + 2)
							endif
		                    // Add to the FullyQualified name
							if ! aTypes:ContainsKey(fullName)
								aTypes:Add(fullName, type)
							endif
	                        // Now, with Standard name
							simpleName := type:Name:Replace('+', '.')
							// Not Empty namespace, not a generic, not nested, not starting with underscore
							if String.IsNullOrEmpty(type:Namespace) .AND. simpleName:IndexOf('`') == -1 .AND. ;
								simpleName:IndexOf('<') == -1 .AND. ! simpleName:StartsWith("_")
								self:_zeroNamespace:AddType(simpleName, self:GetTypeTypesFromType(type))
							endif
	                        // Public Type, not Nested and no Underscore
							if type:IsPublic .and. simpleName.IndexOf('+') == -1  .and. simpleName.IndexOf('_') == -1
								// Get the Namespace
								nspace := type:Namespace
								// and the normal name
								simpleName := type:Name
								simpleName := simpleName:Replace('+', '.')
								// Generic ?
								index := simpleName:IndexOf('`')
								if index != -1
									simpleName := simpleName:Substring(0, index)
								endif
								if nspace?:Length > 0
									local container as AssemblyInfo.NameSpaceContainer
									if ! self:_nameSpaces:ContainsKey(nspace)
										container := AssemblyInfo.NameSpaceContainer{nspace}
										container:AddType(simpleName, self:GetTypeTypesFromType(type))
										self:_nameSpaces:Add(nspace, container)
										self:_nameSpaceTexts := self:_nameSpaceTexts:Add(nspace)
									else
										container := (AssemblyInfo.NameSpaceContainer)self:_nameSpaces[nspace]
										container:AddType(simpleName, self:GetTypeTypesFromType(type))
									endif
									do while nspace:Contains(".")
										nspace := nspace:Substring(0, nspace:LastIndexOf('.'))
										if ! self:_nameSpaceTexts:Contains(nspace)
											self:_nameSpaceTexts := self:_nameSpaceTexts:Add(nspace)
										endif
									enddo
								endif
							endif
						endif
					next
					self:_LoadedTypes := true
					self:_aTypes := aTypes.ToImmutableDictionary(System.StringComparer.OrdinalIgnoreCase) 
					self:_failed := 0
					self:_assembly := null
				catch e as System.Exception
					//
					Support.Debug("Generic exception:")
					Support.Debug(e:Message)
					self:_clearInfo()
				end try
			endif
		*/
		
		// Properties
		property DisplayName as string
			get
				if (String.IsNullOrEmpty(self:fileName))
					return "(Empty)"
				endif
				return System.IO.Path.GetFileName(self:fileName)
			end get
		end property
		
		property FileName as string auto
		
		property FullName as string get self:_fullName
		property GlobalClassName as string get self:_globalClassName
		
		property HasProjects as logic get self:_projects:Count > 0
		
		property ImplicitNamespaces as IList<string>
			get
				//
				return self:_implicitNamespaces
			end get
		end property
		
		property IsModifiedOnDisk as logic
			get
				if String.IsNullOrEmpty(self:fileName)
					return false
				endif
				if ! System.IO.File.Exists(self:fileName)
					return false
				endif
				return (System.IO.File.GetLastWriteTime(self:fileName) != self:Modified)
			end get
		end property
		
		property Modified as System.DateTime get self:_Modified set self:_Modified := value
		
		property Namespaces as IList<string>
			get
				//
				return self:_nameSpaceTexts
			end get
		end property
		
		property Types as IDictionary<string, System.Type>
			get
				//
				return self:_aTypes
			end get
		end property
		
		
		// Nested Types
		internal class NameSpaceContainer
			// Fields
			internal _NameSpace := "" as string
			internal _Types as SortedList<string, AssemblyInfo.TypeTypes>
			
			constructor(_cNameSpace as string);super()
				//
				self:_NameSpace := _cNameSpace
				self:_Types := SortedList<string, AssemblyInfo.TypeTypes>{}
			
			method AddType(typeName as string, type as AssemblyInfo.TypeTypes) as void
				//
				if (! self:_Types:ContainsKey(typeName))
					//
					self:_Types:Add(typeName, type)
				endif
			
			method Clear() as void
				//
				self:_Types:Clear()
			
			
		end class
		
		internal enum TypeTypes as long
			member @@All:=0xff
			member @@Class:=1
			member @@Delegate:=8
			member @@Interface:=4
			member @@None:=0
			member @@Structure:=2
		end enum
		
		
	end class
	
end namespace 

