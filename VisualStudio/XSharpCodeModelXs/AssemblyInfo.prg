using System.Collections.Generic
using System.Collections.Immutable
using System
using System.Diagnostics
using System.Reflection
begin namespace XSharpModel
	[DebuggerDisplay("{DisplayName,nq}")];
		class AssemblyInfo
		// Fields
		private _aExtensions as IList<MethodInfo>
		private _assembly as Assembly
		private _aTypes as IDictionary<string, System.Type>
		private _failed as long
		private _fullName as string
		private _globalClassName as string
		private _HasExtensions as logic
		private _implicitNamespaces as List<string>
		private _LoadedTypes as logic
		private _Modified as System.DateTime
		private _nameSpaces as System.Collections.Hashtable
		private _nameSpaceTexts as List<string>
		private _projects as IList<XProject>
		private _reference as VSLangProj.Reference
		private _zeroNamespace as AssemblyInfo.NameSpaceContainer
		
		// Methods
		constructor()
			super()
			//
			self:_fullName := ""
			self:_failed := 0
			self:_assembly := null
			self:_projects := List<XProject>{}
			self:_clearInfo()
		
		constructor(reference as VSLangProj.Reference)
			self()
			//
			self:_reference := reference
		
		constructor(_cFileName as string, _dModified as System.DateTime)
			self()
			//
			self:FileName := _cFileName
			self:Modified := _dModified
			self:UpdateAssembly()
		
		private method _clearInfo() as void
			//
			self:_aTypes := Dictionary<string, System.Type>{System.StringComparer.OrdinalIgnoreCase}
			self:_aExtensions := List<MethodInfo>{}
			self:_nameSpaces := System.Collections.Hashtable{System.StringComparer.OrdinalIgnoreCase}
			self:_nameSpaceTexts := List<string>{}
			self:_implicitNamespaces := List<string>{}
			self:_zeroNamespace := AssemblyInfo.NameSpaceContainer{"_"}
			self:_LoadedTypes := false
			self:_HasExtensions := false
		
		method AddProject(project as XProject) as void
			//
			if (! self:_projects:Contains(project))
				//
				self:_projects:Add(project)
			endif
		
		private method CurrentDomain_AssemblyResolve(sender as object, args as System.ResolveEventArgs) as Assembly
			local list as List<string>
			local directoryName as string
			local str2 as string
			local path as string
			local assembly as Assembly
			local assembly3 as Assembly
			//
			list := List<string>{}
			directoryName := System.IO.Path.GetDirectoryName(self:FileName)
			str2 := AssemblyName{args:Name}:Name + ".dll"
			path := System.IO.Path.Combine(directoryName, str2)
			if (System.IO.File.Exists(path))
				//
				assembly := AssemblyInfo.LoadAssemblyFromFile(path)
				if (assembly != null)
					//
					return assembly
				endif
			endif
			list:Add(directoryName)
			foreach str4 as string in SystemTypeController.AssemblyFileNames
				//
				directoryName := System.IO.Path.GetDirectoryName(str4)
				if (! list:Contains(directoryName))
					//
					path := System.IO.Path.Combine(directoryName, str2)
					if (System.IO.File.Exists(path))
						//
						assembly3 := Assembly.LoadFrom(path)
						if (assembly3 != null)
							//
							return assembly3
						endif
					endif
					list:Add(directoryName)
				endif
			next
			return null
		
		method GetType(name as string) as System.Type
			//
			if (self:IsModifiedOnDisk)
				//
				self:LoadAssembly()
			endif
			if ((self:_assembly != null) .AND. (self:_aTypes:Count == 0))
				//
				self:UpdateAssembly()
			endif
			if (self:_aTypes:ContainsKey(name))
				//
				return self:Types:Item[name]
			endif
			return null
		
		private method GetTypeTypesFromType(oType as System.Type) as AssemblyInfo.TypeTypes
			//
			if (oType:IsValueType)
				//
				return AssemblyInfo.TypeTypes.Structure
			endif
			if (oType:IsInterface)
				//
				return AssemblyInfo.TypeTypes.Interface
			endif
			if (typeof(System.Delegate):IsAssignableFrom(oType))
				//
				return AssemblyInfo.TypeTypes.Delegate
			endif
			return AssemblyInfo.TypeTypes.Class
		
		private static method HasExtensionAttribute(oInfo as MemberInfo) as logic
			local customAttributes as object[]
			local i as long
			//
			try
				//
				customAttributes := oInfo:GetCustomAttributes(false)
				i := 1
				while ((i <= customAttributes:Length))
					//
					if (customAttributes[(i - 1) + 1]:ToString() == "System.Runtime.CompilerServices.ExtensionAttribute")
						//
						return true
					endif
					i++
				enddo
			catch 
				
			end try
			return false
		
		internal method LoadAssembly() as void
			//
			if (String.IsNullOrEmpty(self:FileName) .AND. (self:_reference != null))
				//
				self:FileName := self:_reference:Path
			endif
			if (System.IO.File.Exists(self:FileName))
				//
				self:_assembly := AssemblyInfo.LoadAssemblyFromFile(self:FileName)
				self:_fullName := self:_assembly:FullName
				self:Modified := System.IO.File.GetLastWriteTime(self:FileName)
				self:_clearInfo()
			endif
		
		static method LoadAssemblyFromFile(fileName as string) as Assembly
			local input as System.IO.FileStream
			local rawAssembly as byte[]
			local path as string
			local str2 as string
			local flag2 as logic
			local assembly2 as Assembly
			//
			if (System.IO.File.Exists(fileName))
				//
				try
					//
					input := System.IO.FileStream{fileName, System.IO.FileMode.Open, System.IO.FileAccess.Read}
					rawAssembly := System.IO.BinaryReader{input}:ReadBytes((long)input:Length )
					if (rawAssembly:Length != input:Length)
						
					endif
					input:Close()
					path := System.IO.Path.ChangeExtension(fileName, ".pdb")
					str2 := System.IO.Path.ChangeExtension(fileName, ".p$$")
					flag2 := false
					if (System.IO.File.Exists(path))
						//
						flag2 := true
						if (System.IO.File.Exists(str2))
							//
							System.IO.File.Delete(str2)
						endif
						System.IO.File.Move(path, str2)
					endif
					assembly2 := Assembly.Load(rawAssembly)
					if (flag2 .AND. System.IO.File.Exists(str2))
						//
						System.IO.File.Move(str2, path)
					endif
					input:Dispose()
					return assembly2
				catch exception as System.Exception
					//
					Support.Debug("Generic exception:", Array.Empty<object>())
					Support.Debug(exception:Message, Array.Empty<object>())
				end try
			endif
			return null
		
		method RemoveProject(project as XProject) as void
			//
			if (self:_projects:Contains(project))
				//
				self:_projects:Remove(project)
			endif
		
		internal method UpdateAssembly() as void
			local num as long
			local dictionary as Dictionary<string, System.Type>
			local list as List<MethodInfo>
			local key as string
			local fullName as string
			local typeName as string
			local o as object[]
			local message as string
			local methods as MethodInfo[]
			local index as long
			local container as AssemblyInfo.NameSpaceContainer
			//
			if (self:_failed <= 3)
				//
				dictionary := Dictionary<string, System.Type>{System.StringComparer.OrdinalIgnoreCase}
				list := List<MethodInfo>{}
				key := ""
				fullName := ""
				typeName := ""
				self:_nameSpaces:Clear()
				self:_nameSpaceTexts:Clear()
				self:_zeroNamespace:Clear()
				self:_globalClassName := ""
				self:_HasExtensions := false
				self:LoadAssembly()
				try
					//
					if (self:_assembly != null)
						var customAttributes := self:_assembly:GetCustomAttributes(false)
						for num := 1 to customAttributes:Length step 1
							//
							var custattr := customAttributes[(num - 1) + 1]
							var type := custattr:GetType()
							switch custattr:ToString()
								case "Vulcan.Internal.VulcanClassLibraryAttribute"
									self:_globalClassName := type:GetProperty("globalClassName"):GetValue(custattr, null):ToString()
									var defaultNs := type:GetProperty("defaultNamespace"):GetValue(custattr, null):ToString()
									if ! String.IsNullOrEmpty(defaultNs)
										self:_implicitNamespaces:Add(defaultNs)
									endif
								case "System.Runtime.CompilerServices.ExtensionAttribute"
									self:_HasExtensions  := true
								case "Vulcan.VulcanImplicitNamespaceAttribute"
									var ns := type:GetProperty("Namespace"):GetValue(custattr, null):ToString()
									if ! String.IsNullOrEmpty(ns)
										self:_implicitNamespaces:Add(ns)
									endif
							end switch
						next
						
						
						
					endif
				catch //obj1 as Object
					
				end try
				var currentDomain := System.AppDomain.CurrentDomain
				currentDomain:AssemblyResolve += System.ResolveEventHandler{ self, @CurrentDomain_AssemblyResolve() }
				local types := null as System.Type[]
				try
					if (self:_assembly != null)
						//
						types := self:_assembly:GetTypes()
					endif
					self:_failed := 0
				catch exception as ReflectionTypeLoadException
					//
					o := <object>{self:_assembly:GetName():Name}
					Support.Debug("Cannot load types from {0}", o)
					Support.Debug("Exception details:")
					message := null
					foreach var exception2 in exception:LoaderExceptions
						if (exception2:Message != message)
							Support.Debug(exception2:Message)
							message := exception2:Message
						endif
					next
					Support.Debug("Types loaded:", Array.Empty<object>())
					foreach var t in exception:Types
						//
						if (t != null)
							//
							Support.Debug(t:FullName)
						endif
					next
					self:_assembly := null
					self:_failed++
				catch exception3 as System.Exception
					//
					Support.Debug("Generic exception:")
					Support.Debug(exception3:Message)
				end try
				currentDomain:AssemblyResolve -= System.ResolveEventHandler{ self, @CurrentDomain_AssemblyResolve() }
				
				if (((types != null) .AND. (types:Length != 0)) .AND. (iif((dictionary != null),(dictionary:Count == 0),false) | ! self:_LoadedTypes))
					//
					try
						//
						num := 1
						while ((num <= types:Length))
							//
							fullName := types[(num - 1) + 1]:FullName
							if (! fullName:StartsWith("$") .AND. ! fullName:StartsWith("<"))
								//
								if (self:_HasExtensions .AND. AssemblyInfo.HasExtensionAttribute(types[(num - 1) + 1]))
									//
									methods := types[(num - 1) + 1]:GetMethods((BindingFlags.Public | BindingFlags.Static))
									foreach info as MethodInfo in methods
										//
										if (AssemblyInfo.HasExtensionAttribute(info))
											//
											list:Add(info)
										endif
									next
								endif
								if (fullName:Contains("+"))
									fullName := fullName:Replace('+', '.')
								endif
								if (fullName:Contains("`"))
									fullName := fullName:Substring(0, (fullName:IndexOf("`") + 2))
								endif
								if (! dictionary:ContainsKey(fullName))
									dictionary:Add(fullName, types[(num - 1) + 1])
								endif
								typeName := types[(num - 1) + 1]:Name:Replace('+', '.')
								if (((String.IsNullOrEmpty(types[(num - 1) + 1]:Namespace) .AND. (typeName:IndexOf('`') == -1)) .AND. ((typeName:IndexOf('+') == -1) .AND. (typeName:IndexOf('<') == -1))) .AND. ! typeName:StartsWith("_"))
									//
									self:_zeroNamespace:AddType(typeName, self:GetTypeTypesFromType(types[(num - 1) + 1]))
								endif
								if ((types[(num - 1) + 1]:IsPublic .AND. (typeName:IndexOf('+') == -1)) .AND. (typeName:IndexOf('_') == -1))
									//
									key := types[(num - 1) + 1]:Namespace
									typeName := types[(num - 1) + 1]:Name:Replace('+', '.')
									index := typeName:IndexOf('`')
									if (index != -1)
										//
										typeName := typeName:Substring(0, index)
									endif
									if ((key != null) .AND. (key:Length > 0))
										//
										if (! self:_nameSpaces:ContainsKey(key))
											//
											container := AssemblyInfo.NameSpaceContainer{key}
											container:AddType(typeName, self:GetTypeTypesFromType(types[(num - 1) + 1]))
											self:_nameSpaces:Add(key, container)
											self:_nameSpaceTexts:Add(key)
										else
											//
											var c:= (AssemblyInfo.NameSpaceContainer)self:_nameSpaces:Item[key]
											c:AddType(typeName, self:GetTypeTypesFromType(types[(num - 1) + 1]))
										endif
										while (key:Contains("."))
											//
											key := key:Substring(0, key:LastIndexOf('.'))
											if (! self:_nameSpaceTexts:Contains(key))
												//
												self:_nameSpaceTexts:Add(key)
											endif
										enddo
									endif
								endif
							endif
							num++
						enddo
						self:_LoadedTypes := true
						self:_aTypes := (IDictionary<string, System.Type>)ImmutableDictionary.ToImmutableDictionary<string, System.Type>(dictionary, System.StringComparer.OrdinalIgnoreCase) 
						self:_aExtensions := (IList<MethodInfo>)ImmutableList.ToImmutableList<MethodInfo>(list) 
						self:_failed := 0
						self:_assembly := null
					catch exception4 as System.Exception
						//
						Support.Debug("Generic exception:", Array.Empty<object>())
						Support.Debug(exception4:Message, Array.Empty<object>())
						self:_clearInfo()
					end try
				endif
			endif
		
		
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
		
		property ImplicitNamespaces as ImmutableList<string>
			get
				//
				return ImmutableList.ToImmutableList<string>(self:_implicitNamespaces)
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
		
		property Namespaces as ImmutableList<string>
			get
				//
				return ImmutableList.ToImmutableList<string>(self:_nameSpaceTexts)
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
			
			// Methods
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

