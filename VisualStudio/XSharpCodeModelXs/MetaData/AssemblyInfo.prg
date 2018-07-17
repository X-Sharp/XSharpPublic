//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Collections.Immutable
USING System
USING System.IO
USING System.Diagnostics
USING System.Reflection
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{DisplayName,nq}")];
CLASS AssemblyInfo
		// Fields
		PRIVATE _aExtensions AS List<MethodInfo>
		PRIVATE _assembly AS Assembly
		PRIVATE _aTypes AS IDictionary<STRING, System.Type>
		PRIVATE _fullName AS STRING
		PRIVATE _globalClassName AS STRING
		PRIVATE _HasExtensions AS LOGIC
		PRIVATE _implicitNamespaces AS ImmutableList<STRING>
		PRIVATE _LoadedTypes AS LOGIC
		PRIVATE _Modified AS System.DateTime
		PRIVATE _nameSpaces AS System.Collections.Hashtable
		PRIVATE _nameSpaceTexts AS ImmutableList<STRING>
		PRIVATE _projects AS ImmutableList<XProject>
		PRIVATE _reference AS VSLangProj.Reference
		PRIVATE _zeroNamespace AS AssemblyInfo.NameSpaceContainer
		
		PRIVATE STATIC WorkFolder AS STRING
		PRIVATE STATIC FailedAssemblies as Dictionary<string, int>

		PUBLIC STATIC PROPERTY DisableAssemblyReferences AS LOGIC AUTO
		PUBLIC STATIC PROPERTY DisableForeignProjectReferences AS LOGIC AUTO
		PUBLIC STATIC PROPERTY DisableXSharpProjectReferences AS LOGIC AUTO
		STATIC CONSTRUCTOR
			// Clear temp files from previous run 
			FailedAssemblies := Dictionary<string, int>{StringComparer.OrdinalIgnoreCase}
			VAR cFolder := Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
			cFolder := System.IO.Path.Combine(cFolder, "XSharp\Temp")
			IF ! System.IO.Directory.Exists(cFolder)
				System.IO.Directory.CreateDirectory(cFolder)
			ENDIF			
			WorkFolder := cFolder
			LOCAL oDir := DirectoryInfo{cFolder} AS DirectoryInfo
			FOREACH VAR oFile IN oDir:GetFiles()
				TRY
					File.SetAttributes(oFile:FullName, FileAttributes.Normal  )
					File.Delete(oFile:FullName)
				CATCH e AS Exception
					XSolution.WriteException(e)
				END TRY
			NEXT
			DisableXSharpProjectReferences := FALSE
			DisableAssemblyReferences := FALSE
			DisableForeignProjectReferences := FALSE
			RETURN
			
			
			// Methods
		CONSTRUCTOR()
			SUPER()
			SELF:_fullName := ""
			SELF:_assembly := NULL
			SELF:_projects := ImmutableList<XProject>.Empty
			SELF:_clearInfo()
			
		CONSTRUCTOR(reference AS VSLangProj.Reference)
			SELF()
			SELF:_reference := reference
			
		CONSTRUCTOR(_cFileName AS STRING, _dModified AS System.DateTime)
			SELF()
			SELF:FileName := _cFileName
			SELF:Modified := _dModified
			SELF:UpdateAssembly()
			
		PRIVATE METHOD _clearInfo() AS VOID
			//
			SELF:_aTypes := Dictionary<STRING, System.Type>{System.StringComparer.OrdinalIgnoreCase}
			SELF:_aExtensions := List<MethodInfo>{}
			SELF:_nameSpaces := System.Collections.Hashtable{System.StringComparer.OrdinalIgnoreCase}
			SELF:_nameSpaceTexts := ImmutableList<STRING>.Empty
			SELF:_implicitNamespaces := ImmutableList<STRING>.Empty
			SELF:_zeroNamespace := AssemblyInfo.NameSpaceContainer{"_"}
			SELF:_LoadedTypes := FALSE
			SELF:_HasExtensions := FALSE
			
		METHOD AddProject(project AS XProject) AS VOID
			IF ! SELF:_projects:Contains(project)
				SELF:_projects := SELF:_projects:Add(project)
			ENDIF
			
		PRIVATE METHOD CurrentDomain_AssemblyResolve(sender AS OBJECT, args AS System.ResolveEventArgs) AS Assembly
			VAR folders := List<STRING>{}	// list of folders that we have tried
			VAR folderPath := System.IO.Path.GetDirectoryName(SELF:FileName)
			VAR name := AssemblyName{args:Name}:Name + ".dll"
			VAR assemblyPath := System.IO.Path.Combine(folderPath, name)
			WriteOutputMessage("--> CurrentDomain_AssemblyResolve : "+assemblyPath) 
			IF System.IO.File.Exists(assemblyPath)
				VAR assembly := AssemblyInfo.LoadAssemblyFromFile(assemblyPath)
				IF assembly != NULL
					RETURN assembly
				ENDIF
			ENDIF
			folders:Add(folderPath)
			FOREACH path AS STRING IN SystemTypeController.AssemblyFileNames
				folderPath := System.IO.Path.GetDirectoryName(path)
				IF ! folders:Contains(folderPath)
					assemblyPath := System.IO.Path.Combine(folderPath, name)
					IF System.IO.File.Exists(assemblyPath)
						VAR asm := Assembly.LoadFrom(assemblyPath)
						IF asm != NULL
							RETURN asm
						ENDIF
					ENDIF
					folders:Add(folderPath)
				ENDIF
			NEXT
			WriteOutputMessage("<-- CurrentDomain_AssemblyResolve : "+assemblyPath)
			RETURN NULL
			
		METHOD GetType(name AS STRING) AS System.Type
			IF SELF:IsModifiedOnDisk
				SELF:LoadAssembly()
			ENDIF
			IF SELF:_assembly != NULL .AND. SELF:_aTypes:Count == 0
				SELF:UpdateAssembly()
			ENDIF
			IF SELF:_aTypes:ContainsKey(name)
				RETURN SELF:Types:Item[name]
			ENDIF
			RETURN NULL
			
		PRIVATE METHOD GetTypeTypesFromType(oType AS System.Type) AS AssemblyInfo.TypeTypes
			IF oType:IsValueType
				RETURN AssemblyInfo.TypeTypes.Structure
			ENDIF
			IF oType:IsInterface
				RETURN AssemblyInfo.TypeTypes.Interface
			ENDIF
			IF TYPEOF(System.Delegate):IsAssignableFrom(oType)
				RETURN AssemblyInfo.TypeTypes.Delegate
			ENDIF
			RETURN AssemblyInfo.TypeTypes.Class
			
		PRIVATE STATIC METHOD HasExtensionAttribute(memberInfo AS MemberInfo) AS LOGIC
			TRY
				VAR customAttributes := memberInfo:GetCustomAttributes(FALSE)
				FOREACH VAR custattr IN customAttributes
					IF custattr:ToString() == "System.Runtime.CompilerServices.ExtensionAttribute"
						RETURN TRUE
					ENDIF
				NEXT
			CATCH 
				// Failed to retrieve custom attributes
			END TRY
			RETURN FALSE
			
		INTERNAL METHOD LoadAssembly() AS VOID
			WriteOutputMessage("--> LoadAssembly : "+SELF:FileName)
			IF String.IsNullOrEmpty(SELF:FileName) .AND. SELF:_reference != NULL
				SELF:FileName := SELF:_reference:Path
			ENDIF
			IF System.IO.File.Exists(SELF:FileName)
				SELF:_assembly := AssemblyInfo.LoadAssemblyFromFile(SELF:FileName)
				IF SELF:_assembly != NULL
					SELF:_fullName  := SELF:_assembly:FullName
					SELF:Modified   := System.IO.File.GetLastWriteTime(SELF:FileName)
				ENDIF
				SELF:_clearInfo()
			ENDIF
			WriteOutputMessage("<-- LoadAssembly : "+SELF:FileName)
			
		STATIC METHOD FindPdbName(fileName AS STRING) AS STRING
			WriteOutputMessage("--> FindPDBName : "+fileName)
			TRY
				IF System.IO.File.Exists(fileName)
					VAR pdb			:= System.IO.Path.ChangeExtension(fileName,".pdb")
					IF System.IO.File.Exists(pdb)
						return pdb
					ENDIF
					// ok when it is not in the DLL folder but referenced in the DLL then extract this information
					VAR text        := System.IO.File.ReadAllText(fileName)
					VAR pdbIndex	:= text:IndexOf(".pdb", StringComparison.InvariantCultureIgnoreCase)
					IF pdbIndex > 0
						VAR lastTerminatorIndex := text:Substring(0, pdbIndex).LastIndexOf('\0')
						RETURN text:Substring(lastTerminatorIndex + 1, pdbIndex - lastTerminatorIndex + 3)
					ENDIF
				ENDIF
			FINALLY
				WriteOutputMessage("<-- FindPDBName : "+fileName)
			END TRY
			RETURN string.Empty
			
			
		STATIC METHOD LoadAssemblyFromFile(fileName AS STRING) AS Assembly
			LOCAL result  AS Assembly
			LOCAL iAttempts AS INT
			TRY
				WriteOutputMessage("--> LoadAssemblyFromFile : "+fileName)
				IF FailedAssemblies:ContainsKey(fileName)
					iAttempts := FailedAssemblies[fileName]
					IF iAttempts > 3
						WriteOutputMessage("    ... Cannot load assembly, giving up after a few retries:")
						WriteOutputMessage("    ...."  +fileName+" "+ iAttempts:ToString())
						RETURN NULL
					endif
				endif
				IF System.IO.File.Exists(fileName)
					LOCAL cPDB		AS STRING
					LOCAL cPdbCopy	AS STRING
					local PdbRenamed   := FALSE as LOGIC
					TRY
						VAR temp		:=  System.IO.Path.Combine(WorkFolder, System.IO.Path.GetFileName(fileName))
						System.IO.File.Copy(fileName, temp,true)
						VAR rawAssembly := System.IO.File.ReadAllBytes(temp)
						cPdb		:= FindPdbName(temp)
						System.IO.File.Delete(temp)
						cPdbCopy := System.IO.Path.ChangeExtension(temp, ".p$$")
						IF !String.IsNullOrEmpty(cPdb) .and. System.IO.File.Exists(cPdb)
							PdbRenamed := TRUE
							IF System.IO.File.Exists(cPdbCopy)
								System.IO.File.Delete(cPdbCopy)
							ENDIF
							System.IO.File.Move(cPdb, cPdbCopy)
						ENDIF
						result := Assembly.Load(rawAssembly)
						IF FailedAssemblies:ContainsKey(fileName)
							FailedAssemblies:Remove(fileName)
						ENDIF
						RETURN result
					CATCH exception AS System.IO.FileLoadException
						// files with unmanaged code produce the exception:
						// Attempt to load an unverifiable executable with fixups (IAT with more than 2 sections or a TLS section.) (Exception from HRESULT: 0x80131019)
						XSolution.WriteException(exception)
					CATCH exception AS System.Exception
						// Other exception
						XSolution.WriteException(exception)
					FINALLY
						IF PdbRenamed .AND. System.IO.File.Exists(cPdbCopy)
							System.IO.File.Move(cPdbCopy, cPdb)
						ENDIF
					END TRY
					TRY
						// Generate a random name and copy the DLL to prevent locking the original file
						VAR temp := filename
						DO WHILE File.Exists(temp)
							temp := System.IO.Path.Combine(WorkFolder, System.IO.Path.GetRandomFileName())
						ENDDO
						// Need to rename the PDB again. Was undone in the Finally above
						IF PdbRenamed .AND. System.IO.File.Exists(cPdb)
							System.IO.File.Move(cPdb, cPdbCopy)
						ENDIF
						System.IO.File.Copy(filename, temp,TRUE)
						File.SetAttributes(temp, FileAttributes.Normal | FileAttributes.Temporary )
						result := Assembly.LoadFrom(temp)
						IF FailedAssemblies:ContainsKey(fileName)
							FailedAssemblies:Remove(fileName)
						ENDIF
						RETURN result
					CATCH e AS Exception
						XSolution.WriteException(e)
					FINALLY
						IF PdbRenamed .AND. System.IO.File.Exists(cPdbCopy)
							TRY
								System.IO.File.Move(cPdbCopy, cPdb)
							CATCH e AS Exception
								XSolution.WriteException(e)
							END TRY
						ENDIF
					END TRY
					IF FailedAssemblies:ContainsKey(fileName)
						FailedAssemblies[fileName] := FailedAssemblies[fileName] +1
					ELSE
						FailedAssemblies:Add(fileName, 1)
					ENDIF
				ELSE
					// FileName does not exist. No need to change the failedAssemblies list
				ENDIF
			FINALLY
				WriteOutputMessage("<-- LoadAssemblyFromFile : "+fileName)
			END TRY
			RETURN NULL
			
		METHOD RemoveProject(project AS XProject) AS VOID
			//
			IF SELF:_projects:Contains(project)
				SELF:_projects := SELF:_projects:Remove(project)
			ENDIF
		
		METHOD Refresh() AS VOID
			VAR currentDT  := System.IO.File.GetLastWriteTime(SELF:FileName)
			IF currentDT != SELF:Modified
				WriteOutputMessage("AssemblyInfo.Refresh() Assembly was changed: "+SELF:FileName )
				SELF:UpdateAssembly()
			ENDIF
			
		INTERNAL METHOD UpdateAssembly() AS VOID
			LOCAL aTypes AS Dictionary<STRING, System.Type>
			LOCAL nspace AS STRING
			LOCAL fullName AS STRING
			LOCAL simpleName AS STRING
			LOCAL message AS STRING
			LOCAL types := NULL AS System.Type[]
			LOCAL index AS LONG
			TRY
				WriteOutputMessage("-->AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
				aTypes		 := Dictionary<STRING, System.Type>{System.StringComparer.OrdinalIgnoreCase}
				_aExtensions := List<MethodInfo>{}
				fullName := ""
				simpleName := ""
				SELF:_nameSpaces:Clear()
				_nameSpaceTexts := ImmutableList<STRING>.Empty
				SELF:_zeroNamespace:Clear()
				SELF:_globalClassName := ""
				SELF:_HasExtensions := FALSE
				SELF:LoadAssembly()
				VAR currentDomain := System.AppDomain.CurrentDomain
				currentDomain:AssemblyResolve += System.ResolveEventHandler{ SELF, @CurrentDomain_AssemblyResolve() }
				TRY
					IF SELF:_assembly != NULL
						VAR customAttributes := SELF:_assembly:GetCustomAttributes(FALSE)
						LOCAL found := 0 AS INT
						FOREACH VAR custattr IN customAttributes
							//
							VAR type := custattr:GetType()
							SWITCH custattr:ToString()
								CASE "Vulcan.Internal.VulcanClassLibraryAttribute"
									SELF:_globalClassName := type:GetProperty("globalClassName"):GetValue(custattr, NULL):ToString()
									VAR defaultNs := type:GetProperty("defaultNamespace"):GetValue(custattr, NULL):ToString()
									IF ! String.IsNullOrEmpty(defaultNs)
										SELF:_implicitNamespaces := SELF:_implicitNamespaces:Add(defaultNs)
										WriteOutputMessage("   ...implicit Namespace found: "+defaultNs)
									ENDIF
									found += 1
								CASE "XSharp.Internal.ClassLibraryAttribute"
									SELF:_globalClassName := type:GetProperty("GlobalClassName"):GetValue(custattr, NULL):ToString()
									WriteOutputMessage("   ...Globals Classname found: "+_globalClassName )

									VAR defaultNs := type:GetProperty("DefaultNamespace"):GetValue(custattr, NULL):ToString()
									IF ! String.IsNullOrEmpty(defaultNs)
										SELF:_implicitNamespaces := SELF:_implicitNamespaces:Add(defaultNs)
										WriteOutputMessage("   ...implicit Namespace found: "+defaultNs)
									ENDIF
									found += 1
								CASE "System.Runtime.CompilerServices.ExtensionAttribute"
									SELF:_HasExtensions  := TRUE
									found += 2
								CASE "Vulcan.VulcanImplicitNamespaceAttribute"
								CASE "XSharp.ImplicitNamespaceAttribute"
									VAR ns := type:GetProperty("Namespace"):GetValue(custattr, NULL):ToString()
									IF ! String.IsNullOrEmpty(ns)
										SELF:_implicitNamespaces := SELF:_implicitNamespaces:Add(ns)
										WriteOutputMessage("   ...implicit Namespace found: "+ns)
									ENDIF
									found += 4
							END SWITCH
							// All attributes found then get out of here
							IF found == 7
								EXIT
							ENDIF
						NEXT
					
					ENDIF
				CATCH e AS Exception
					XSolution.WriteException(e)
				END TRY
				TRY
					IF SELF:_assembly != NULL
						types := SELF:_assembly:GetTypes()
					ENDIF
				CATCH exception AS ReflectionTypeLoadException
					//
					WriteOutputMessage("Cannot load types from "+ SELF:_assembly:GetName():Name)
					XSolution.WriteException(exception)
					message := NULL
					FOREACH VAR exception2 IN exception:LoaderExceptions
						IF exception2:Message != message
							XSolution.WriteException(exception2)
							message := exception2:Message
						ENDIF
					NEXT
					SELF:_assembly := NULL
				CATCH ex AS System.Exception
					XSolution.WriteException(ex)
				END TRY
				currentDomain:AssemblyResolve -= System.ResolveEventHandler{ SELF, @CurrentDomain_AssemblyResolve() }
			
				IF types != NULL .and. types:Length != 0 .AND. (aTypes:Count == 0  .or. ! _LoadedTypes)
					TRY
						FOREACH VAR type IN types
							fullName := type:FullName
							IF ! fullName:StartsWith("$") .AND. ! fullName:StartsWith("<")
								IF SELF:_HasExtensions .AND. AssemblyInfo.HasExtensionAttribute(type)
									VAR methods := type:GetMethods(BindingFlags.Public | BindingFlags.Static)
									FOREACH info AS MethodInfo IN methods
										IF AssemblyInfo.HasExtensionAttribute(info)
											_aExtensions:Add(info)
										ENDIF
									NEXT
								ENDIF
								//
								// Nested Type ?
								IF fullName:Contains("+")
									fullName := fullName:Replace('+', '.')
								ENDIF
								// Generic ?
								IF fullName:Contains("`")
									fullName := fullName:Substring(0, fullName:IndexOf("`") + 2)
								ENDIF
								// Add to the FullyQualified name
								IF ! aTypes:ContainsKey(fullName)
									aTypes:Add(fullName, type)
								ENDIF
								// Now, with Standard name
								simpleName := type:Name:Replace('+', '.')
								// Not Empty namespace, not a generic, not nested, not starting with underscore
								IF String.IsNullOrEmpty(type:Namespace) .AND. simpleName:IndexOf('`') == -1 .AND. ;
										simpleName:IndexOf('<') == -1 .AND. ! simpleName:StartsWith("_")
									SELF:_zeroNamespace:AddType(simpleName, SELF:GetTypeTypesFromType(type))
								ENDIF
								// Public Type, not Nested and no Underscore
								IF type:IsPublic .and. simpleName.IndexOf('+') == -1  .and. simpleName.IndexOf('_') == -1
									// Get the Namespace
									nspace := type:Namespace
									// and the normal name
									simpleName := type:Name
									simpleName := simpleName:Replace('+', '.')
									// Generic ?
									index := simpleName:IndexOf('`')
									IF index != -1
										simpleName := simpleName:Substring(0, index)
									ENDIF
									IF nspace?:Length > 0
										LOCAL container AS AssemblyInfo.NameSpaceContainer
										IF ! SELF:_nameSpaces:ContainsKey(nspace)
											container := AssemblyInfo.NameSpaceContainer{nspace}
											container:AddType(simpleName, SELF:GetTypeTypesFromType(type))
											SELF:_nameSpaces:Add(nspace, container)
											SELF:_nameSpaceTexts := SELF:_nameSpaceTexts:Add(nspace)
										ELSE
											container := (AssemblyInfo.NameSpaceContainer)SELF:_nameSpaces[nspace]
											container:AddType(simpleName, SELF:GetTypeTypesFromType(type))
										ENDIF
										DO WHILE nspace:Contains(".")
											nspace := nspace:Substring(0, nspace:LastIndexOf('.'))
											IF ! SELF:_nameSpaceTexts:Contains(nspace)
												SELF:_nameSpaceTexts := SELF:_nameSpaceTexts:Add(nspace)
											ENDIF
										ENDDO
									ENDIF
								ENDIF
							ENDIF
						NEXT
						SELF:_LoadedTypes := TRUE
						SELF:_aTypes := aTypes.ToImmutableDictionary(System.StringComparer.OrdinalIgnoreCase) 
						SELF:_assembly := NULL
					CATCH e AS System.Exception
						XSolution.WriteException(e)
						SELF:_clearInfo()
					END TRY
				ENDIF
			FINALLY
				WriteOutputMessage("<-- AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
			END TRY
			// Properties
		PROPERTY DisplayName AS STRING
			GET
				IF String.IsNullOrEmpty(SELF:fileName)
					RETURN "(Empty)"
				ENDIF
				RETURN System.IO.Path.GetFileName(SELF:fileName)
			END GET
		END PROPERTY
		
		PROPERTY FileName AS STRING AUTO
		
		PROPERTY FullName AS STRING GET SELF:_fullName
		PROPERTY GlobalClassName AS STRING GET SELF:_globalClassName
		
		PROPERTY HasProjects AS LOGIC GET SELF:_projects:Count > 0
		
		PROPERTY ImplicitNamespaces AS IList<STRING> GET SELF:_implicitNamespaces
		
		PROPERTY IsModifiedOnDisk AS LOGIC
			GET
				IF String.IsNullOrEmpty(SELF:fileName)
					RETURN FALSE
				ENDIF
				IF ! System.IO.File.Exists(SELF:fileName)
					RETURN FALSE
				ENDIF
				RETURN (System.IO.File.GetLastWriteTime(SELF:fileName) != SELF:Modified)
			END GET
		END PROPERTY
		
		PROPERTY Modified AS System.DateTime GET SELF:_Modified SET SELF:_Modified := VALUE
		PROPERTY Namespaces AS IList<STRING> GET SELF:_nameSpaceTexts
		PROPERTY RuntimeVersion AS STRING 
			GET
				SELF:UpdateAssembly()
				IF SELF:_assembly != NULL
					RETURN SELF:_assembly:ImageRuntimeVersion
				ENDIF
				RETURN ""
			END GET
		END PROPERTY
		PROPERTY Types AS IDictionary<STRING, System.Type> GET SELF:_aTypes
		

		STATIC METHOD WriteOutputMessage(message as string) AS VOID
			XSolution.WriteOutputMessage("XModel.AssemblyInfo " +message )
		
		// Nested Types
		INTERNAL CLASS NameSpaceContainer
			// Fields
			INTERNAL _NameSpace := "" AS STRING
			INTERNAL _Types AS SortedList<STRING, AssemblyInfo.TypeTypes>
			
			CONSTRUCTOR(_cNameSpace AS STRING);SUPER()
				//
				SELF:_NameSpace := _cNameSpace
				SELF:_Types := SortedList<STRING, AssemblyInfo.TypeTypes>{}
				
			METHOD AddType(typeName AS STRING, type AS AssemblyInfo.TypeTypes) AS VOID
				//
				IF ! SELF:_Types:ContainsKey(typeName)
					//
					SELF:_Types:Add(typeName, type)
				ENDIF
				
			METHOD Clear() AS VOID
				//
				SELF:_Types:Clear()
			END CLASS
				
		INTERNAL ENUM TypeTypes AS LONG
			MEMBER @@All:=0xff
			MEMBER @@Class:=1
			MEMBER @@Delegate:=8
			MEMBER @@Interface:=4
			MEMBER @@None:=0
			MEMBER @@Structure:=2
		END ENUM
		
		
	END CLASS
	
END NAMESPACE 

