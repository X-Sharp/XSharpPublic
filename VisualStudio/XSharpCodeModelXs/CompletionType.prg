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
		PRIVATE _file	  AS XFile
		PRIVATE _type	  AS IXType
		PROPERTY IsArray AS LOGIC AUTO
      

		// Methods
		CONSTRUCTOR()
			SUPER()
			SELF:_type := NULL
			SELF:_file := NULL


		CONSTRUCTOR(element AS IXElement, oFile as XFile)
			SELF()
    	   SELF:_file := oFile
 			IF element IS IXType VAR oType
				SELF:_type := oType
			ELSEIF element IS IXTypeMember VAR oMember
				SELF:CheckType(oMember:TypeName, oMember:Parent:NameSpace)
			ELSEIF element:Parent IS IXType VAR oParent
			   SELF:_type := oParent
			ELSE
				VAR parent := (IXTypeMember)(element:Parent)
				IF (parent != NULL)
					SELF:CheckType(parent:TypeName, parent:Parent:NameSpace)
				ENDIF
         ENDIF
 
		CONSTRUCTOR(type AS IXType, oFile as XFile)
			SELF()
			SELF:_type  := type
         SELF:_file  := oFile         

   	CONSTRUCTOR(type AS IXType)
			SELF(type, null)
         if type is XElement VAR xel
            _file := xel:File
         endif

		CONSTRUCTOR(element AS IXTypeMember)
			SELF()
         if element is XElement VAR xel
			   SELF:_file := xel:File
         ENDIF
			IF element:Kind:HasReturnType()
				SELF:CheckType(element:TypeName, element:Parent:NameSpace)
			ELSE
				SELF:_type := (IXType) element:Parent
			ENDIF

		CONSTRUCTOR(xvar AS IXVariable, defaultNS AS STRING)
			SELF()
			LOCAL parent AS IXTypeMember
			parent := (IXTypeMember)(xvar:Parent)
         IF xvar is XElement var xel
			   SELF:_file := xel:File
         ENDIF
			IF (parent != NULL)
				IF (! String.IsNullOrEmpty(parent:Parent:NameSpace))
					defaultNS := parent:Parent:NameSpace
            ENDIF
				SELF:CheckType(xvar:TypeName,  defaultNS)
			ENDIF

		CONSTRUCTOR(typeName AS STRING, xFile AS XFile, usings AS IList<STRING>)
			SELF()
			SELF:_file := xFile
			SELF:CheckType(typeName, usings)

		CONSTRUCTOR(typeName AS STRING, xFile AS XFile, defaultNS AS STRING)
			SELF()
			SELF:_file := xFile
			SELF:CheckType(typeName, defaultNS)

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
				SELF:_type := xType
			ENDIF

		PRIVATE METHOD CheckSystemType(typeName AS STRING, usings AS IList<STRING>) AS VOID
			LOCAL sType AS XTypeRef
			IF SELF:_file != NULL
            VAR options := SELF:_file:Project:ParseOptions
				typeName    := typeName:GetSystemTypeName(options:XSharpRuntime)
				sType       := SELF:_file:Project:FindSystemType(typeName, usings)
			ENDIF
			IF sType != NULL
				SELF:_type := sType
			ENDIF

		PRIVATE METHOD CheckType(typeName AS STRING, usings AS IList<STRING>) AS VOID
			//
			//
			IF typeName.EndsWith("[]")
				typeName := typeName.Substring(0, typeName.Length - 2)
            IsArray := TRUE
			ENDIF
			// prevent lookup from simple types
			VAR stype := SimpleTypeToSystemType(typeName)
			IF stype != NULL
				_type := stype
			ELSEIF SELF:_file?:Project != NULL
				//
				SELF:CheckProjectType(typeName, SELF:_file:Project, usings)
				IF ! SELF:IsInitialized

					SELF:CheckSystemType(typeName, usings)
					IF ! SELF:IsInitialized

						FOREACH prj AS XProject IN SELF:_file:Project:ReferencedProjects
							SELF:CheckProjectType(typeName, prj, usings)
							IF SELF:IsInitialized
								EXIT
							ENDIF
						NEXT
					ENDIF
				ENDIF
			ENDIF

		PRIVATE METHOD CheckType(typeName AS STRING, defaultNS AS STRING) AS VOID
			LOCAL usings AS List<STRING>
			usings := List<STRING>{SELF:_file:Usings}
			IF ! String.IsNullOrEmpty(defaultNS)
				usings:Add(defaultNS)
			ENDIF
            FOREACH var ns in SELF:_file:Project:ImplicitNamespaces
                usings:AddUnique(ns)
            NEXT
            // For fully qualified typenames, search without usings first. That is usually faster
            IF typeName:Contains(".")
                SELF:CheckType(typeName, List<STRING>{})
            ENDIF
            IF ! SELF:IsInitialized
                // Now check all usings
			    SELF:CheckType(typeName, usings)
            ENDIF

		INTERNAL METHOD SimpleTypeToSystemType(kw AS STRING) AS XTypeRef
			LOCAL typeName AS STRING
			LOCAL sType := NULL AS XTypeRef
			//
			IF (kw != NULL)
				//
				SWITCH kw:ToLowerInvariant()
					CASE "object"
					CASE "system.object"
						typeName := "System.Object"

					CASE "string"
					CASE "system.string"
						typeName := "System.String"

					CASE "dword"
					CASE "uint32"
					CASE "system.uint32"
						typeName := "System.UInt32"

					CASE "int64"
					CASE "system.int64"
						typeName := "System.Int64"

					CASE "int16"
					CASE "shortint"
					CASE "short"
					CASE "system.int16"
						typeName := "system.Int16"

					CASE "longint"
					CASE "long"
					CASE "int"
					CASE "int32"
					CASE "system.int32"
						typeName := "System.Int32"

					CASE "void"
					CASE "system.void"
						typeName := "System.Void"

					CASE "byte"
					CASE "system.byte"
						typeName := "System.Byte"

					CASE "word"
					CASE "uint16"
					CASE "system.uint16"
						typeName := "System.UInt16"

					CASE "char"
					CASE "system.char"
						typeName := "System.Char"

					CASE "real4"
						typeName :=  "System.Single"

					CASE "real8"
						typeName :=  "System.Double"

					CASE "uint64"
					CASE "system.uint64"
						typeName := "system.UInt64"

					CASE "logic"
					CASE "system.boolean"
						typeName := "System.Boolean"

					CASE "sbyte"
					CASE "system.sbyte"
						typeName := "System.SByte"


					CASE "ptr"
						typeName := "System.IntPtr"
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
					VAR options := SELF:_file:Project:ParseOptions
					typeName    := typeName:GetSystemTypeName(options:XSharpRuntime)
					sType       := SELF:_file:Project:FindSystemType(typeName, List<STRING>{})
				ENDIF
				//
			ENDIF
			RETURN sType

		// Properties
		PROPERTY File AS XFile GET SELF:_file

		PROPERTY ElementType AS CompletionType AUTO

		PROPERTY FullName AS STRING GET SELF:_type:FullName

		PROPERTY IsInitialized AS LOGIC GET SELF:_type != NULL 

		PROPERTY ParentType AS CompletionType
			GET
				IF (SELF:_type != NULL)
					RETURN CompletionType{SELF:_type:BaseType,_file, ""}
				ENDIF
				IF (SELF:_type != NULL)
//
//					IF (SELF:_type:Parent != NULL)
//
//						RETURN CompletionType{SELF:_type:Parent,_file}
//					ENDIF
//					IF (SELF:_type:ParentName  !=NULL)
//
						VAR defaultNS := ""
						IF (! String.IsNullOrEmpty(SELF:_type:NameSpace))

							defaultNS := SELF:_type:NameSpace
						ENDIF
						RETURN CompletionType{SELF:_type:BaseType, SELF:_file, defaultNS}
					ENDIF
//				ENDIF
				RETURN CompletionType{"System.Object", NULL, ""}
			END GET
		END PROPERTY

		PROPERTY Type  AS IXType      GET SELF:_type
		PROPERTY XType AS XType       GET SELF:_type ASTYPE XType
      PROPERTY XTypeRef AS XTypeRef GET SELF:_type ASTYPE XTypeRef

	END CLASS

END NAMESPACE


