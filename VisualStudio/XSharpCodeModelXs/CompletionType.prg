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
			SELF:_stype := NULL
			SELF:_xtype := NULL
			SELF:_file := NULL


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
						IF (parent != NULL)

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
			IF (parent != NULL)
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
			SELF:_file := xFile
			SELF:CheckType(typeName, xFile, defaultNS)

		PRIVATE METHOD CheckProjectType(typeName AS STRING, xprj AS XProject, usings AS IList<STRING>) AS VOID
			LOCAL xType AS XType
			xType := xprj:Lookup(typeName, TRUE)
			IF xType == NULL .AND. usings != NULL

				FOREACH name AS STRING IN usings:Expanded()
					VAR fqn := name + "." + typeName
					xType := xprj:Lookup(fqn, TRUE)
					IF (xType != NULL)
						EXIT
					ENDIF
				NEXT
			ENDIF
			IF xType != NULL
				SELF:_xtype := xType
			ENDIF

		PRIVATE METHOD CheckSystemType(typeName AS STRING, usings AS IList<STRING>) AS VOID
			LOCAL sType AS System.Type
			IF SELF:_file != NULL
                VAR options := SELF:_file:Project:ProjectNode:ParseOptions
				typeName := typeName:GetSystemTypeName(options:XSharpRuntime)
				sType := SELF:_file:Project:FindSystemType(typeName, usings)
			ENDIF
			IF sType != NULL
				SELF:_stype := sType
			ENDIF

		PRIVATE METHOD CheckType(typeName AS STRING, xFile AS XFile, usings AS IList<STRING>) AS VOID
			LOCAL isArray AS LOGIC
			//
			SELF:_file := xFile
			LOCAL stype AS System.Type
			//
			IF typeName.EndsWith("[]")
				typeName := typeName.Substring(0, typeName.Length - 2)
				isArray := TRUE
			ENDIF
			// prevent lookup from simple types
			stype := SimpleTypeToSystemType(typeName)
			IF sType != NULL
				_sType := sType
			ELSEIF SELF:_file?:Project != NULL
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
            FOREACH var ns in xFile:Project:ImplicitNamespaces
                usings:AddUnique(ns)
            NEXT
            // For fully qualified typenames, search without usings first. That is usually faster
            IF typename:Contains(".")
                SELF:CheckType(typeName, xFile, List<STRING>{})
            ENDIF
            IF ! SELF:IsInitialized
                // Now check all usings
			    SELF:CheckType(typeName, xFile, usings)
            ENDIF

		INTERNAL METHOD SimpleTypeToSystemType(kw AS STRING) AS System.Type
			LOCAL typeName AS STRING
			LOCAL sType AS System.Type
			//
			IF (kw != NULL)
				//
				SWITCH kw:ToLowerInvariant()
					CASE "object"
					CASE "system.object"
						typeName := "system.object"

					CASE "string"
					CASE "system.string"
						typeName := "system.string"

					CASE "dword"
					CASE "uint32"
					CASE "system.uint32"
						typeName := "system.uint32"

					CASE "int64"
					CASE "system.int64"
						typeName := "system.int64"

					CASE "int16"
					CASE "shortint"
					CASE "short"
					CASE "system.int16"
						typeName := "system.uint16"

					CASE "longint"
					CASE "long"
					CASE "int"
					CASE "int32"
					CASE "system.int32"
						typeName := "system.int32"

					CASE "void"
					CASE "system.void"
						typeName := "system.void"

					CASE "byte"
					CASE "system.byte"
						typeName := "system.byte"

					CASE "word"
					CASE "uint16"
					CASE "system.uint16"
						typeName := "system.uint16"

					CASE "char"
					CASE "system.char"
						typeName := "system.char"

					CASE "real4"
						typeName :=  "system.single"

					CASE "real8"
						typeName :=  "system.double"

					CASE "uint64"
					CASE "system.uint64"
						typeName := "system.uint64"

					CASE "logic"
					CASE "system.boolean"
						typeName := "system.boolean"

					CASE "sbyte"
					CASE "system.sbyte"
						typeName := "system.sbyte"


					CASE "ptr"
						typeName := "system.intptr"
                    case "array"
                    case "date"
                    case "float"
                    case "symbol"
                    case "psz`"
                    case "usual"
                        typeName := kw

				END SWITCH

				IF ( String.IsNullOrEmpty( typeName ) )
					RETURN NULL
				ENDIF
				//
				IF SELF:_file != NULL
					VAR options := SELF:_file:Project:ProjectNode:ParseOptions
					typeName := typeName:GetSystemTypeName(options:XSharpRuntime)
					sType := SELF:_file:Project:FindSystemType(typeName, List<STRING>{})
				ENDIF
				//
			ENDIF
			RETURN sType

		// Properties
		PROPERTY File AS XFile GET SELF:_file

		PROPERTY IsArray AS LOGIC AUTO
		PROPERTY ElementType AS CompletionType AUTO

		PROPERTY FullName AS STRING
			GET
				IF (SELF:_xtype != NULL)
					RETURN SELF:_xtype:FullName
				ENDIF
				IF (SELF:_stype != NULL)
					RETURN SELF:_stype:GetXSharpTypeName()
				ENDIF
				RETURN NULL
			END GET
		END PROPERTY

		PROPERTY IsInitialized AS LOGIC GET SELF:_stype != NULL .OR. SELF:_xtype != NULL

		PROPERTY ParentType AS CompletionType
			GET
				IF (SELF:_stype != NULL)
					RETURN CompletionType{SELF:_stype:BaseType}
				ENDIF
				IF (SELF:_xtype != NULL)

					IF (SELF:_xtype:Parent != NULL)

						RETURN CompletionType{SELF:_xtype:Parent}
					ENDIF
					IF (SELF:_xtype:ParentName  !=NULL)

						VAR defaultNS := ""
						IF (! String.IsNullOrEmpty(SELF:_xtype:NameSpace))

							defaultNS := SELF:_xtype:NameSpace
						ENDIF
						RETURN CompletionType{SELF:_xtype:ParentName, SELF:_xtype:File, defaultNS}
					ENDIF
				ENDIF
				RETURN CompletionType{"System.Object", NULL, ""}
			END GET
		END PROPERTY

		PROPERTY SType AS System.Type GET SELF:_stype
		PROPERTY XType AS XType GET SELF:_xtype

	END CLASS

END NAMESPACE


