//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Concurrent
using System.Collections.Generic
using System
begin namespace XSharpModel
	class SystemTypeController
		// Fields
		static private assemblies := ConcurrentDictionary<string, AssemblyInfo>{StringComparer.OrdinalIgnoreCase} as ConcurrentDictionary<string, XSharpModel.AssemblyInfo>
		static private _mscorlib := null as AssemblyInfo
		
		// Methods
		static method Clear() as void
			//
			SystemTypeController.assemblies:Clear()
		
		static method FindAssemblyByLocation(location as string) as string
			//
			if (SystemTypeController.assemblies:ContainsKey(location))
				//
				return SystemTypeController.assemblies:Item[location]:FullName
			endif
			return null
		
		static method FindAssemblyByName(fullName as string) as string
			local info as AssemblyInfo
			//
			foreach pair as KeyValuePair<string, AssemblyInfo> in SystemTypeController.assemblies
				//
				info := pair:Value
				if (String.Compare(info:FullName, fullName, System.StringComparison.OrdinalIgnoreCase) == 0)
					//
					return info:FullName
				endif
			next
			return null
		
		static method FindAssemblyLocation(fullName as string) as string
			local info as AssemblyInfo
			//
			foreach pair as KeyValuePair<string, AssemblyInfo> in SystemTypeController.assemblies
				//
				info := pair:Value
				if (! String.IsNullOrEmpty(info:FullName) .AND. (String.Compare(info:FullName, fullName, System.StringComparison.OrdinalIgnoreCase) == 0))
					//
					return pair:Key
				endif
			next
			return null
		
		method FindType(typeName as string, usings as IReadOnlyList<string>, assemblies as IReadOnlyList<AssemblyInfo>) as System.Type
			local strArray as string[]
			local num as long
			local index as long
			local str2 as string
			local str3 as string
			local num3 as long
			local strArray2 as string[]
			local type as System.Type
			//
			if (typeName:EndsWith(">"))
				//
				if (typeName:Length <= (typeName:Replace(">", ""):Length + 1))
					//
					strArray := typeName:Split("<,>":ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
					num := (strArray:Length - 1)
					typeName := strArray[ 1] + "`" + num:ToString()
				else
					//
					index := typeName:IndexOf("<")
					str2 := typeName:Substring(0, index)
					str3 := typeName:Substring((index + 1))
					str3 := str3:Substring(0, (str3:Length - 1)):Trim()
					index := str3:IndexOf("<")
					while index >= 0
						num3 := str3:LastIndexOf(">")
						str3 := str3:Substring(0, index) + str3:Substring((num3 + 1)):Trim()
						index := str3:IndexOf("<")
					enddo
					strArray2 := str3:Split(",":ToCharArray())
					typeName := str2 + "`" + strArray2:Length:ToString()
				endif
			endif
			type := SystemTypeController.Lookup(typeName, assemblies)
			if (type != null)
				//
				return type
			endif
			if (usings != null)
				//
				foreach str4 as string in usings:Expanded()
					//
					type := SystemTypeController.Lookup(String.Concat(str4, ".", typeName), assemblies)
					if (type != null)
						//
						return type
					endif
				next
			endif
			if (assemblies != null)
				//
				foreach info as AssemblyInfo in assemblies
					//
					if (info:ImplicitNamespaces != null)
						//
						foreach str6 as string in info:ImplicitNamespaces
							//
							type := SystemTypeController.Lookup(String.Concat(str6, ".", typeName), assemblies)
							if (type != null)
								//
								return type
							endif
						next
					endif
				next
			endif
			type := SystemTypeController.Lookup(String.Concat("Functions.", typeName), assemblies)
			if (type != null)
				//
				return type
			endif
			return null
		
		method GetNamespaces(assemblies as IList<AssemblyInfo>) as System.Collections.Immutable.ImmutableList<string>
			local list as List<string>
			//
			list := List<string>{}
			foreach info as AssemblyInfo in assemblies
				//
				foreach str as string in info:Namespaces
					//
					ListExtensions.AddUnique(list, str)
				next
			next
			return System.Collections.Immutable.ImmutableList.ToImmutableList<string>(list)
		
		static method LoadAssembly(cFileName as string) as AssemblyInfo
			local info as AssemblyInfo
			local lastWriteTime as System.DateTime
			local key as string
			//
			lastWriteTime := System.IO.File.GetLastWriteTime(cFileName)
			if (SystemTypeController.assemblies:ContainsKey(cFileName))
				info := SystemTypeController.assemblies:Item[cFileName]
			else
				info := AssemblyInfo{cFileName, System.DateTime.MinValue}
				SystemTypeController.assemblies:TryAdd(cFileName, info)
			endif
			if (cFileName:EndsWith("mscorlib.dll", System.StringComparison.OrdinalIgnoreCase))
				SystemTypeController.mscorlib := AssemblyInfo{cFileName, System.DateTime.MinValue}
			endif
			if (System.IO.Path.GetFileName(cFileName):ToLower() == "system.dll")
				key := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(cFileName), "mscorlib.dll")
				if (! SystemTypeController.assemblies:ContainsKey(key) .AND. System.IO.File.Exists(key))
					SystemTypeController.LoadAssembly(key)
				endif
			endif
			return info
		
		static method LoadAssembly(reference as VsLangProj.Reference) as AssemblyInfo
			local path as string
			path := reference:Path
			if (String.IsNullOrEmpty(path))
				return AssemblyInfo{reference}
			endif
			return SystemTypeController.LoadAssembly(path)
		
		static method Lookup(typeName as string, theirassemblies as IReadOnlyList<AssemblyInfo>) as System.Type
			local type as System.Type
			//
			type := null
			foreach info as AssemblyInfo in theirassemblies
				//
				if (info:Types:Count == 0)
					//
					info:UpdateAssembly()
				endif
				if (info:Types:TryGetValue(typeName, out type))
					//
					exit
					
				endif
				if (info != null)
					//
					type := info:GetType(typeName)
				endif
				if (type != null)
					//
					exit
					
				endif
			next
			if ((type == null) .AND. (SystemTypeController.mscorlib != null))
				//
				type := SystemTypeController.mscorlib:GetType(typeName)
			endif
			return type
		
		static method RemoveAssembly(cFileName as string) as void
			local info as AssemblyInfo
			//
			if (SystemTypeController.assemblies:ContainsKey(cFileName))
				//
				SystemTypeController.assemblies:TryRemove(cFileName, out info)
			endif
		
		static method UnloadUnusedAssemblies() as void
			local list as List<string>
			local info as AssemblyInfo
			//
			list := List<string>{}
			foreach pair as KeyValuePair<string, AssemblyInfo> in SystemTypeController.assemblies
				//
				if (! pair:Value:HasProjects)
					//
					list:Add(pair:Key)
				endif
			next
			foreach str as string in list
				//
				SystemTypeController.assemblies:TryRemove(str, out info)
			next
			if (SystemTypeController.assemblies:Count == 0)
				//
				SystemTypeController.mscorlib := null
			endif
			System.GC.Collect()
		
		
		// Properties
		static property AssemblyFileNames as System.Collections.Immutable.ImmutableList<string>
			get
				//
				return System.Collections.Immutable.ImmutableList.ToImmutableList<string>(SystemTypeController.assemblies:Keys)
			end get
		end property
		
		static property MsCorLib as AssemblyInfo get SystemTypeController._mscorlib set SystemTypeController._mscorlib := value
		
		
	end class
	
end namespace 

