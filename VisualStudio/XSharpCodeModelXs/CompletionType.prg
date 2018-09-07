//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Diagnostics
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING XSharpModel
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{FullName,nq}")];
	CLASS CompletionType
		// Fields
		PRIVATE _file	 AS XFile
		PRIVATE _stype	 AS System.Type	
		PRIVATE _xtype	 AS XType
		
		// Methods
		CONSTRUCTOR()
			SUPER()
			SELF:_stype := null
			SELF:_xtype := null
			SELF:_file := null
		
		
		CONSTRUCTOR(sType AS System.Type)
			SELF()
			SELF:_stype := sType
		
		CONSTRUCTOR(element AS XElement)
			SELF()
			LOCAL oMember AS XTypeMember
			LOCAL parent AS XTypeMember
			SELF:_file := element:file
			IF element IS XType
				SELF:_xtype := (XType)element 
			ELSE
				IF element IS XTypeMember

					oMember := (XTypeMember)(element)
					SELF:CheckType(oMember:TypeName, oMember:file, oMember:Parent:NameSpace)
				ELSE

					IF element:Parent IS XType
	
						SELF:_xtype := (XType)(element:Parent)
					ELSE
	
						parent := (XTypeMember)(element:Parent)
						IF (parent != null)
		
							SELF:CheckType(parent:TypeName, parent:file, parent:Parent:NameSpace)
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		
		CONSTRUCTOR(xType AS XType)
			SELF()
			SELF:_xtype := xType
			SELF:_file  := xType:File
		
		CONSTRUCTOR(element AS XTypeMember)
			SELF()
			SELF:_file := element:file
			IF element:Kind:HasReturnType()
				SELF:CheckType(element:TypeName, element:file, element:Parent:NameSpace)
			ELSE
				SELF:_xtype := (XType) element:Parent
			ENDIF
		
		CONSTRUCTOR(xvar AS XVariable, defaultNS AS STRING)
			SELF()
			LOCAL parent AS XTypeMember
			parent := (XTypeMember)(xvar:Parent)
			SELF:_file := xvar:file
			IF (parent != null)
				//
				IF (! String.IsNullOrEmpty(parent:Parent:NameSpace))

					defaultNS := ((XType)parent:Parent):NameSpace
				ENDIF
				SELF:CheckType(xvar:TypeName, parent:file, defaultNS)
			ENDIF
		
		CONSTRUCTOR(typeName AS STRING, xFile AS XFile, usings AS IList<STRING>)
			SELF()
			SELF:_file := xFile
			SELF:CheckType(typeName, xFile, usings)
		
		CONSTRUCTOR(typeName AS STRING, xFile AS XFile, defaultNS AS STRING)
			SELF()
			SELF:CheckType(typeName, xFile, defaultNS)
		
		PRIVATE METHOD CheckProjectType(typeName AS STRING, xprj AS XProject, usings AS IList<STRING>) AS VOID
			LOCAL xType AS XType
			xType := xprj:Lookup(typeName, true)
			IF xType == null .AND. usings != null

				FOREACH name AS STRING IN usings:Expanded()
					VAR fqn := name + "." + typeName
					xType := xprj:Lookup(fqn, true)
					IF (xType != null)
						EXIT
					ENDIF
				NEXT
			ENDIF
			IF xType != null
				SELF:_xtype := xType
			ENDIF
		
		PRIVATE METHOD CheckSystemType(typeName AS STRING, usings AS IList<STRING>) AS VOID
			LOCAL sType AS System.Type
			IF SELF:_file != null
				typeName := typeName:GetSystemTypeName()
				sType := SELF:_file:Project:FindSystemType(typeName, usings)
			ENDIF
			IF sType != null
				SELF:_stype := sType
			ENDIF
		
		PRIVATE METHOD CheckType(typeName AS STRING, xFile AS XFile, usings AS IList<STRING>) AS VOID
			//
			SELF:_file := xFile
			LOCAL stype AS System.Type
			// prevent lookup from simple types
			stype := SimpleTypeToSystemType(typeName)
			IF sType != null
				_sType := sType
			ELSEIF SELF:_file?:Project != null
				//
				SELF:CheckProjectType(typeName, xFile:Project, usings)
				IF ! SELF:IsInitialized

					SELF:CheckSystemType(typeName, usings)
					IF ! SELF:IsInitialized
	
						FOREACH prj AS XProject IN xFile:Project:ReferencedProjects
							SELF:CheckProjectType(typeName, prj, usings)
							IF SELF:IsInitialized
								EXIT
							ENDIF
						NEXT
					ENDIF
				ENDIF
			ENDIF
		
		PRIVATE METHOD CheckType(typeName AS STRING, xFile AS XFile, defaultNS AS STRING) AS VOID
			LOCAL usings AS List<STRING>
			usings := List<STRING>{xFile:Usings}
			IF ! String.IsNullOrEmpty(defaultNS)
				usings:Add(defaultNS)
			ENDIF
			SELF:CheckType(typeName, xFile, usings)
		
		INTERNAL METHOD SimpleTypeToSystemType(kw AS STRING) AS System.Type
			//
			IF (kw != null)
				//
				SWITCH kw:ToLowerInvariant()
					CASE "object"
					CASE "system.object"
						RETURN TYPEOF(OBJECT)

					CASE "string"
					CASE "system.string"
						RETURN TYPEOF(STRING)
					
					CASE "dword"
					CASE "uint32"
					CASE "system.uint32"
						RETURN TYPEOF(DWORD)
					CASE "int64"
					CASE "system.int64"
						RETURN TYPEOF(INT64)
					
					CASE "int16"
					CASE "shortint"
					CASE "short"
					CASE "system.int16"
						RETURN TYPEOF(SHORT)
					CASE "longint"
					CASE "long"
					CASE "int"
					CASE "int32"
					CASE "system.int32"
						RETURN TYPEOF(LONG)
					CASE "void"
					CASE "system.void"
						RETURN TYPEOF(VOID)
					CASE "byte"
					CASE "system.byte"
						RETURN TYPEOF(BYTE)
					
					CASE "word"
					CASE "uint16"
					CASE "system.uint16"
						RETURN TYPEOF(WORD)

					CASE "char"
					CASE "system.char"
						RETURN TYPEOF(CHAR)
					
					CASE "real4"
						RETURN TYPEOF(REAL4)
					
					CASE "real8"
						RETURN TYPEOF(REAL8)
					
					CASE "uint64"
					CASE "system.uint64"
						RETURN TYPEOF(UINT64)
					
					CASE "logic"
					CASE "system.boolean"
						RETURN TYPEOF(LOGIC)
					
					CASE "sbyte"
					CASE "system.sbyte"
						RETURN TYPEOF(SByte)
					
				END SWITCH
			ENDIF
			RETURN null
		
		// Properties
		PROPERTY File AS XFile GET SELF:_file
		
		PROPERTY FullName AS STRING
			GET
				IF (SELF:_xtype != null)
					RETURN SELF:_xtype:FullName
				ENDIF
				IF (SELF:_stype != null)
					RETURN SELF:_stype:GetXSharpTypeName()
				ENDIF
				RETURN null
			END GET
		END PROPERTY
		
		PROPERTY IsInitialized AS LOGIC GET SELF:_stype != null .OR. SELF:_xtype != null 
		
		PROPERTY ParentType AS CompletionType
			GET
				IF (SELF:_stype != null)
					RETURN CompletionType{SELF:_stype:BaseType}
				ENDIF
				IF (SELF:_xtype != null)

					IF (SELF:_xtype:Parent != null)
	
						RETURN CompletionType{SELF:_xtype:Parent}
					ENDIF
					IF (SELF:_xtype:ParentName  !=null)
	
						VAR defaultNS := ""
						IF (! String.IsNullOrEmpty(SELF:_xtype:NameSpace))
		
							defaultNS := SELF:_xtype:NameSpace
						ENDIF
						RETURN CompletionType{SELF:_xtype:ParentName, SELF:_xtype:File, defaultNS}
					ENDIF
				ENDIF
				RETURN CompletionType{"System.Object", null, ""}
			END GET
		END PROPERTY
		
		PROPERTY SType AS System.Type GET SELF:_stype
		PROPERTY XType AS XType GET SELF:_xtype
		
	END CLASS
	
END NAMESPACE 


