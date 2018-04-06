//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Concurrent
using System.Collections.Generic
using System.Collections.Immutable
using System
using System.IO
begin namespace XSharpModel
    /// <summary>
    /// We have one SystemTypeController in memory : It will handle all references types for all projects
    /// Assemblies are stored inside a List of AssemblyInfo
    /// </summary>
	class SystemTypeController
		#region fields
		static private assemblies  	as ConcurrentDictionary<string, XSharpModel.AssemblyInfo>
		static private _mscorlib 	as AssemblyInfo
		#endregion

		static constructor
			assemblies := ConcurrentDictionary<string, AssemblyInfo>{StringComparer.OrdinalIgnoreCase} 
			_mscorlib := null 
			return 
			
		#region properties
		// Properties
		static property AssemblyFileNames as ImmutableList<string>
			get
				return SystemTypeController.assemblies:Keys:ToImmutableList()
			end get
		end property
		static property MsCorLib as AssemblyInfo get _mscorlib set _mscorlib := value
		#endregion

		// Methods
		static method Clear() as void
			assemblies:Clear()
			_mscorlib := null
		
		static method FindAssemblyByLocation(location as string) as string
			if assemblies:ContainsKey(location)
				return assemblies:Item[location]:FullName
			endif
			return null
		
		static method FindAssemblyByName(fullName as string) as string
			foreach var item in assemblies
				var asm := item:Value
				if String.Compare(asm:FullName, fullName, StringComparison.OrdinalIgnoreCase) == 0
					return asm:FullName
				endif
			next
			return null
		
		static method FindAssemblyLocation(fullName as string) as string
			local info as AssemblyInfo
			//
			foreach var pair in assemblies
				//
				info := pair:Value
				if (! String.IsNullOrEmpty(info:FullName) .AND. (String.Compare(info:FullName, fullName, System.StringComparison.OrdinalIgnoreCase) == 0))
					//
					return pair:Key
				endif
			next
			return null
		
		method FindType(typeName as string, usings as IList<string>, assemblies as IList<AssemblyInfo>) as System.Type
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
			type := Lookup(typeName, assemblies)
			if type != null
				return type
			endif
			if usings != null
				foreach strUsing as string in usings:Expanded()
					type := Lookup(strUsing + "." + typeName, assemblies)
					if type != null
						return type
					endif
				next
			endif
			if assemblies != null
				foreach var asm in assemblies
					if (asm:ImplicitNamespaces != null)
						foreach strNs as string in asm:ImplicitNamespaces
							var fullname := strNs + "." + typeName
							type := Lookup(fullName, assemblies)
							if type != null
								return type
							endif
						next
					endif
				next
			endif
            // Also Check into the Functions Class for Globals/Defines/...
			type := Lookup("Functions." + typeName, assemblies)
			return type
		
		method GetNamespaces(assemblies as IList<AssemblyInfo>) as ImmutableList<string>
			var list := List<string>{}
			foreach var info in assemblies
				foreach str as string in info:Namespaces
					list:AddUnique( str)
				next
			next
			return list:ToImmutableList()
		
		static method LoadAssembly(cFileName as string) as AssemblyInfo
			local info as AssemblyInfo
			local lastWriteTime as System.DateTime
			local key as string
			//
			lastWriteTime := File.GetLastWriteTime(cFileName)
			if (assemblies:ContainsKey(cFileName))
				info := assemblies:Item[cFileName]
			else
				info := AssemblyInfo{cFileName, System.DateTime.MinValue}
				assemblies:TryAdd(cFileName, info)
			endif
			if (cFileName:EndsWith("mscorlib.dll", System.StringComparison.OrdinalIgnoreCase))
				mscorlib := AssemblyInfo{cFileName, System.DateTime.MinValue}
			endif
			if (Path.GetFileName(cFileName):ToLower() == "system.dll")
				key := Path.Combine(Path.GetDirectoryName(cFileName), "mscorlib.dll")
				if (! assemblies:ContainsKey(key) .AND. File.Exists(key))
					LoadAssembly(key)
				endif
			endif
			return info
		
		static method LoadAssembly(reference as VsLangProj.Reference) as AssemblyInfo
			local path as string
			path := reference:Path
			if (String.IsNullOrEmpty(path))
				return AssemblyInfo{reference}
			endif
			return LoadAssembly(path)
		
		static method Lookup(typeName as string, theirassemblies as IList<AssemblyInfo>) as System.Type
			local sType as System.Type
			sType := null
			foreach var assembly in theirassemblies
				if (assembly:Types:Count == 0)
					assembly:UpdateAssembly()
				endif
				if assembly:Types:TryGetValue(typeName, out sType) .and. sType != NULL
					exit
				endif
				if (assembly != null)
					sType := assembly:GetType(typeName)
				endif
				if sType != null
					exit
				endif
			next
			if sType == null .AND. mscorlib != null
                // check mscorlib 
				sType := mscorlib:GetType(typeName)
			endif
			return sType
		
		static method RemoveAssembly(cFileName as string) as void
			local info as AssemblyInfo
			if assemblies:ContainsKey(cFileName)
				assemblies:TryRemove(cFileName, out info)
			endif
		
		static method UnloadUnusedAssemblies() as void
			local unused as List<string>
			unused := List<string>{}
            // collect list of assemblies which are no longer in use
			foreach var asm in assemblies
				if ! asm:Value:HasProjects 
					unused:Add(asm:Key)
				endif
			next
			foreach key as string in unused
				local info as AssemblyInfo
				assemblies:TryRemove(key, out info)
			next
            // when no assemblies left, then unload mscorlib
			if (assemblies:Count == 0)
				mscorlib := null
			endif
			GC.Collect()
		
		
		
	end class
	
end namespace 

