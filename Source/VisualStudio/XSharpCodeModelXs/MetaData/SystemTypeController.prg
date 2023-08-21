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
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel
	/// <summary>
		/// We have one SystemTypeController in memory : It will handle all references types for all projects
		/// Assemblies are stored inside a List of AssemblyInfo
	/// </summary>
	CLASS SystemTypeController
		#region fields
		STATIC PRIVATE assemblies  AS ConcurrentDictionary<STRING, XAssembly>
		STATIC PRIVATE _mscorlib 	AS XAssembly
		#endregion

		STATIC CONSTRUCTOR
			assemblies := ConcurrentDictionary<STRING, XAssembly>{StringComparer.OrdinalIgnoreCase}
			_mscorlib := NULL
			RETURN

			#region properties
			// Properties
			STATIC PROPERTY AssemblyFileNames AS IList<STRING>
				GET
					RETURN SystemTypeController.assemblies:Keys:ToArray()
				END GET
			END PROPERTY
			STATIC PROPERTY mscorlib AS XAssembly GET _mscorlib SET _mscorlib := VALUE
		#endregion

		// Methods
		STATIC METHOD Clear() AS VOID
			assemblies:Clear()
			_mscorlib := NULL
			//GC.Collect()

		STATIC METHOD FindAssemblyByLocation(location AS STRING) AS STRING
			IF assemblies:ContainsKey(location)
				RETURN assemblies:Item[location]:FullName
			ENDIF
			RETURN NULL

		STATIC METHOD FindAssembly(fullName AS STRING) AS XAssembly
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
			LOCAL info AS XAssembly
			FOREACH VAR pair IN assemblies
				info := pair:Value
				IF ! String.IsNullOrEmpty(info:FullName) .AND. (String.Compare(info:FullName, fullName, System.StringComparison.OrdinalIgnoreCase) == 0)
					RETURN pair:Key
				ENDIF
			NEXT
			RETURN NULL

      STATIC METHOD FindType(typeName as STRING, assemblyName as STRING) AS XPETypeSymbol
         VAR assemblies := List<XAssembly>{}
         var asm := FindAssembly(assemblyName)
         IF asm != null .and. asm:Loaded
            assemblies:Add(asm)
            FOREACH var name in asm:ReferencedAssemblies
               var asm2 := FindAssembly(name)
               if asm2 != null .and. asm2:Loaded
                  assemblies:Add(asm2)
               endif
            NEXT
         ENDIF
         IF assemblies:Count > 0
            RETURN FindType(typeName, List<String>{}, assemblies)
         ENDIF
         return NULL


        STATIC METHOD GetTickedTypeName(typeName as STRING) AS STRING
           IF typeName:EndsWith(">") .AND.  typeName:Contains("<") .AND. typeName:Length > 2
				IF typeName:Length <= (typeName:Replace(">", ""):Length + 1)
					VAR elements := typeName:Split(<CHAR>{'<',',','>'}, System.StringSplitOptions.RemoveEmptyEntries)
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
					VAR elements := typeParams:Split(<Char>{','})
					typeName := baseName + "`" + elements:Length:ToString()
				ENDIF
            ENDIF
            IF typeName:EndsWith("]") .and. typeName:Contains("[")
                RETURN KnownTypes.SystemArray
            ENDIF
            RETURN typeName

		STATIC METHOD FindType(typeName AS STRING, usings AS IList<STRING>, assemblies AS IList<XAssembly>) AS XPETypeSymbol
			LOCAL result := NULL AS XPETypeSymbol
            STATIC LOCAL lastSearch := "" AS STRING
            // Empty TypeName  or unwanted recursion
            IF String.IsNullOrEmpty( typeName ) .or. typeName == lastSearch
                RETURN result
            ENDIF
			TRY
                lastSearch := typeName
                IF XSettings.EnableTypelookupLog
				WriteOutputMessage("--> FindType() "+typeName)
                ENDIF
				typeName := GetTickedTypeName(typeName)
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
				XSettings.Exception(e,__FUNCTION__)
				result := NULL
            FINALLY
                lastSearch := ""
                IF XSettings.EnableTypelookupLog
				    WriteOutputMessage("<-- FindType() "+typeName+" " + IIF(result != NULL, result:FullName, "* not found *"))
                ENDIF
            END TRY
			RETURN result

		STATIC METHOD LoadAssembly(cFileName AS STRING) AS XAssembly
			LOCAL info AS XAssembly
			LOCAL lastWriteTime AS System.DateTime
			LOCAL key AS STRING
			//
			//WriteOutputMessage("<<-- LoadAssembly(string) "+cFileName)
			lastWriteTime := File.GetLastWriteTime(cFileName)
			IF assemblies:ContainsKey(cFileName)
				info := assemblies:Item[cFileName]
				//WriteOutputMessage("     ... assembly "+cFileName+" found in cache")
			ELSE
				info := XAssembly{cFileName, System.DateTime.MinValue}
				assemblies:TryAdd(cFileName, info)
			ENDIF
			IF cFileName:EndsWith("mscorlib.dll", System.StringComparison.OrdinalIgnoreCase)
				mscorlib := XAssembly{cFileName, System.DateTime.MinValue}
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


		STATIC METHOD Lookup(typeName AS STRING, theirassemblies AS IList<XAssembly>) AS XPETypeSymbol
			FOREACH VAR assembly IN theirassemblies
				assembly:Refresh()
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
			LOCAL info AS XAssembly
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
				LOCAL info AS XAssembly
				assemblies:TryRemove(key, OUT info)
			NEXT
			// when no assemblies left, then unload mscorlib
			IF assemblies:Count == 0
				mscorlib := NULL
			ENDIF
			//GC.Collect()

		STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
			XSettings.Information("XModel.Typecontroller "+message)

		STATIC METHOD LookForExtensions(typeName AS STRING, theirassemblies AS IList<XAssembly>) AS IList<IXMemberSymbol>
			// First Search for a system Type
         VAR pos := typeName:IndexOf('<')
         IF pos > 0
            typeName := typeName:Substring(0, pos)+"<>"

         ENDIF
         VAR result := List<IXMemberSymbol>{}
			FOREACH VAR assembly IN theirassemblies:ToList()
				assembly:Refresh()
				IF assembly:HasExtensions
               VAR ext := assembly:FindExtensionMethodsForType(typeName)
               result:AddRange(ext)
				ENDIF
			NEXT
			RETURN result

	END CLASS

END NAMESPACE

