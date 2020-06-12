//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.Linq
USING System
USING System.IO
USING System.Reflection


BEGIN NAMESPACE XSharpModel
	/// <summary>
		/// We have one SystemTypeController in memory : It will handle all references types for all projects
		/// Assemblies are stored inside a List of AssemblyInfo
	/// </summary>
	CLASS SystemTypeController
		#region fields
		STATIC PRIVATE assemblies  AS ConcurrentDictionary<STRING, XSharpModel.AssemblyInfo>
		STATIC PRIVATE _mscorlib 	AS AssemblyInfo
		#endregion
		
		STATIC CONSTRUCTOR
			assemblies := ConcurrentDictionary<STRING, AssemblyInfo>{StringComparer.OrdinalIgnoreCase}
			_mscorlib := NULL
			RETURN
			
			#region properties
			// Properties
			STATIC PROPERTY AssemblyFileNames AS IList<STRING>
				GET
					RETURN SystemTypeController.assemblies:Keys:ToArray()
				END GET
			END PROPERTY
			STATIC PROPERTY mscorlib AS AssemblyInfo GET _mscorlib SET _mscorlib := VALUE
		#endregion
		
		// Methods
		STATIC METHOD Clear() AS VOID
			assemblies:Clear()
			_mscorlib := NULL
		
		STATIC METHOD FindAssemblyByLocation(location AS STRING) AS STRING
			IF assemblies:ContainsKey(location)
				RETURN assemblies:Item[location]:FullName
			ENDIF
			RETURN NULL

		STATIC METHOD FindAssembly(fullName AS STRING) AS AssemblyInfo
			FOREACH VAR item IN assemblies
				VAR asm := item:Value
				IF String.Compare(asm:FullName, fullName, StringComparison.OrdinalIgnoreCase) == 0
					RETURN asm
				ENDIF
			NEXT
			RETURN NULL


		STATIC METHOD FindAssemblyByName(fullName AS STRING) AS STRING
			FOREACH VAR item IN assemblies
				VAR asm := item:Value
				IF String.Compare(asm:FullName, fullName, StringComparison.OrdinalIgnoreCase) == 0
					RETURN asm:FullName
				ENDIF
			NEXT
			RETURN NULL
		
		STATIC METHOD FindAssemblyLocation(fullName AS STRING) AS STRING
			LOCAL info AS AssemblyInfo
			FOREACH VAR pair IN assemblies
				info := pair:Value
				IF ! String.IsNullOrEmpty(info:FullName) .AND. (String.Compare(info:FullName, fullName, System.StringComparison.OrdinalIgnoreCase) == 0)
					RETURN pair:Key
				ENDIF
			NEXT
			RETURN NULL
		
      STATIC METHOD FindType(typeName as STRING, assemblyName as STRING) AS XTypeReference
         var assemblies := List<AssemblyInfo>{}
         var asm := FindAssembly(assemblyName)
         IF asm != null
            assemblies:Add(asm)
            FOREACH var name in asm:ReferencedAssemblies
               var asm2 := FindAssembly(name)
               if asm2 != null
                  assemblies:Add(asm2)
               endif
            NEXT
         ENDIF
         IF assemblies:Count > 0
            RETURN FindType(typeName, List<String>{}, assemblies)
         ENDIF
         return NULL


		STATIC METHOD FindType(typeName AS STRING, usings AS IList<STRING>, assemblies AS IList<AssemblyInfo>) AS XTypeReference
			LOCAL result := NULL AS XTypeReference
			TRY
				WriteOutputMessage("--> FindType() "+typeName)
				IF typeName:EndsWith(">") .AND.  typeName:Contains("<") .AND. typeName:Length > 2
					IF typeName:Length <= (typeName:Replace(">", ""):Length + 1)
						VAR elements := typeName:Split("<,>":ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
						VAR num := (elements:Length - 1)
						typeName := elements[ 1] + "`" + num:ToString()
					ELSE
						VAR pos		   := typeName:IndexOf("<")
						VAR baseName   := typeName:Substring(0, pos)
						VAR typeParams := typeName:Substring(pos + 1)
						typeParams	   := typeParams:Substring(0, typeName:Length - 1):Trim()
						pos			   := typeParams:IndexOf("<")
						WHILE pos >= 0
							VAR pos2  := typeParams:LastIndexOf(">")
							typeParams := typeParams:Substring(0, pos) + typeParams:Substring(pos2 + 1):Trim()
							pos := typeParams:IndexOf("<")
						ENDDO
						VAR elements := typeParams:Split(",":ToCharArray())
						typeName := baseName + "`" + elements:Length:ToString()
					ENDIF
				ENDIF
				result := Lookup(typeName, assemblies)
				IF result != NULL
					RETURN result
				ENDIF
				IF usings != NULL
					FOREACH name AS STRING IN usings:Expanded()
						result := Lookup(name + "." + typeName, assemblies)
						IF result != NULL
							RETURN result
						ENDIF
					NEXT
				ENDIF
				IF assemblies != NULL
					FOREACH VAR asm IN assemblies
						IF asm:ImplicitNamespaces != NULL
							FOREACH strNs AS STRING IN asm:ImplicitNamespaces
								VAR fullName := strNs + "." + typeName
								result := Lookup(fullName, assemblies)
								IF result != NULL
									RETURN result
								ENDIF
							NEXT
						ENDIF
					NEXT
				ENDIF
				// Also Check into the Functions Class for Globals/Defines/...
				result := Lookup("Functions." + typeName, assemblies)
			CATCH e AS Exception
				XSolution.WriteException(e)
				result := NULL
			FINALLY
				WriteOutputMessage("<-- FindType() "+typeName+" " + IIF(result != NULL, result:FullName, "* not found *"))
			END TRY
			RETURN result
			
		STATIC METHOD GetNamespaces(assemblies AS IList<AssemblyInfo>) AS IList<STRING>
			VAR list := List<STRING>{}
			FOREACH VAR info IN assemblies
				FOREACH str AS STRING IN info:Namespaces
					list:AddUnique( str)
				NEXT
			NEXT
			RETURN list:AsReadOnly()
			
		STATIC METHOD LoadAssembly(cFileName AS STRING) AS AssemblyInfo
			LOCAL info AS AssemblyInfo
			LOCAL lastWriteTime AS System.DateTime
			LOCAL key AS STRING
			//
			//WriteOutputMessage("<<-- LoadAssembly(string) "+cFileName)
			lastWriteTime := File.GetLastWriteTime(cFileName)
			IF assemblies:ContainsKey(cFileName)
				info := assemblies:Item[cFileName]
				//WriteOutputMessage("     ... assembly "+cFileName+" found in cache")
			ELSE
				info := AssemblyInfo{cFileName, System.DateTime.MinValue}
				assemblies:TryAdd(cFileName, info)
			ENDIF
			IF cFileName:EndsWith("mscorlib.dll", System.StringComparison.OrdinalIgnoreCase)
				mscorlib := AssemblyInfo{cFileName, System.DateTime.MinValue}
			ENDIF
			IF Path.GetFileName(cFileName):ToLower() == "system.dll"
				key := Path.Combine(Path.GetDirectoryName(cFileName), "mscorlib.dll")
				IF ! assemblies:ContainsKey(key) .AND. File.Exists(key)
					//WriteOutputMessage("LoadAssembly() load mscorlib from same location as system.DLL")
					LoadAssembly(key)
				ENDIF
			ENDIF
			//WriteOutputMessage(">>-- LoadAssembly(string) "+cFileName)
			RETURN info
		
		
		STATIC METHOD Lookup(typeName AS STRING, theirassemblies AS IList<AssemblyInfo>) AS XTypeReference
			FOREACH VAR assembly IN theirassemblies
				assembly:Refresh()
            IF assembly:Types:ContainsKey(typeName)
               RETURN assembly:Types[typeName]
				ENDIF
				VAR type := assembly:GetType(typeName)
				IF type != NULL
               RETURN type
				ENDIF
			NEXT
			IF mscorlib != NULL
				// check mscorlib
				RETURN mscorlib:GetType(typeName)
         ENDIF
         RETURN NULL
         
		STATIC METHOD RemoveAssembly(cFileName AS STRING) AS VOID
			LOCAL info AS AssemblyInfo
			IF assemblies:ContainsKey(cFileName)
				assemblies:TryRemove(cFileName, OUT info)
			ENDIF
		
		STATIC METHOD UnloadUnusedAssemblies() AS VOID
			LOCAL unused AS List<STRING>
			unused := List<STRING>{}
			// collect list of assemblies which are no longer in use
			FOREACH VAR asm IN assemblies
				IF ! asm:Value:HasProjects
					unused:Add(asm:Key)
				ENDIF
			NEXT
			FOREACH key AS STRING IN unused
				LOCAL info AS AssemblyInfo
				assemblies:TryRemove(key, OUT info)
			NEXT
			// when no assemblies left, then unload mscorlib
			IF assemblies:Count == 0
				mscorlib := NULL
			ENDIF
			GC.Collect()
		
		STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
			XSolution.WriteOutputMessage("XModel.Typecontroller "+message)
		
		STATIC METHOD LookForExtensions(typeName AS STRING, theirassemblies AS IList<AssemblyInfo>) AS List<IXMember>
			// First Search for a system Type
			var ext := List<IXMember>{} 
			var type := Lookup( typeName, theirassemblies )
			IF type != NULL
				ext := LookForExtensions( type, theirassemblies )
			ENDIF
			RETURN ext
		
		STATIC METHOD LookForExtensions( systemType AS IXType, theirassemblies AS IList<AssemblyInfo>) AS List<IXMember>
			var ext := List<IXMember>{} 
			FOREACH VAR assembly IN theirassemblies
				assembly:Refresh()
				IF assembly:HasExtensions
					FOREACH VAR extMet IN assembly:Extensions
						var parms := extMet:Parameters:ToArray()
						IF ( parms:Length > 0 )
							VAR p := parms[__ARRAYBASE__]
                     if systemType:MatchesTypeName(p.TypeName)
								ext:Add( extMet )
							ENDIF
						ENDIF
					NEXT
				ENDIF
			NEXT
			RETURN ext			
		
	END CLASS
	
END NAMESPACE

