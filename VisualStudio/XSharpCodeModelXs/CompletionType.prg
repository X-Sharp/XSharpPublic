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
		PROPERTY InSource as LOGIC GET _type IS XSourceElement
      

		// Methods
		CONSTRUCTOR()
			SUPER()
			SELF:_type := NULL
			SELF:_file := NULL


		CONSTRUCTOR(element AS IXEntity, oFile as XFile)
			SELF()
    	   SELF:_file := oFile
 			IF element IS IXType VAR oType
				SELF:_type := oType
			ELSEIF element IS IXMember VAR oMember
				SELF:CheckType(oMember:TypeName, oMember:Parent:Namespace)
			ELSEIF element:Parent IS IXType VAR oParent
			   SELF:_type := oParent
			ELSE
				VAR parent := (IXMember)(element:Parent)
				IF (parent != NULL)
					SELF:CheckType(parent:TypeName, parent:Parent:Namespace)
				ENDIF
         ENDIF
 
		CONSTRUCTOR(type AS IXType, oFile as XFile)
			SELF()
			SELF:_type  := type
         SELF:_file  := oFile         

   	CONSTRUCTOR(type AS IXType)
			SELF(type, null)
         if type is XSourceElement VAR xel
            _file := xel:File
         endif

		CONSTRUCTOR(element AS IXMember)
			SELF()
         if element is XSourceElement VAR xel
			   SELF:_file := xel:File
         ENDIF
			IF element:Kind:HasReturnType()
            if element is XMemberDefinition
				   SELF:CheckType(element:TypeName, element:Parent:Namespace)
            else
               var entref  := (XMemberReference) element
               var type    := SystemTypeController.FindType(entref:OriginalTypeName, entref:Assembly:FullName)
               _type       := type
            ENDIF
			ELSE
				SELF:_type := (IXType) element:Parent
			ENDIF

		CONSTRUCTOR(xvar AS IXVariable, defaultNS AS STRING)
			SELF()
			LOCAL parent AS IXMember
			parent := (IXMember)(xvar:Parent)
         IF xvar is XSourceElement var xel
			   SELF:_file := xel:File
         ENDIF
			IF (parent != NULL)
				IF (! String.IsNullOrEmpty(parent:Parent:Namespace))
					defaultNS := parent:Parent:Namespace
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
			LOCAL xType AS XTypeDefinition
         IF String.IsNullOrEmpty(typeName)
            RETURN
         ENDIF
			xType := xprj:Lookup(typeName, usings:Expanded())
			IF xType != NULL
				SELF:_type := xType
			ENDIF

		PRIVATE METHOD CheckSystemType(typeName AS STRING, usings AS IList<STRING>) AS VOID
			LOCAL sType AS XTypeReference
         IF String.IsNullOrEmpty(typeName)
            RETURN
         ENDIF
			IF SELF:_file != NULL .AND. SELF:_file:Project != NULL
            VAR options := SELF:_file:Project:ParseOptions
				typeName    := typeName:GetSystemTypeName(options:XSharpRuntime)
				sType       := SELF:_file:Project:FindSystemType(typeName, usings:ToArray())
			ENDIF
			IF sType != NULL
				SELF:_type := sType
			ENDIF

		PRIVATE METHOD CheckType(typeName AS STRING, usings AS IList<STRING>) AS VOID
         IF String.IsNullOrEmpty(typeName)
            RETURN
         ENDIF
			IF typeName.EndsWith("[]")
				typeName := typeName.Substring(0, typeName.Length - 2)
            IsArray := TRUE
			ENDIF
			// prevent lookup from simple types
			VAR type := SimpleTypeToSystemType(typeName)
			IF type != NULL
				_type := type
			ELSEIF SELF:_file?:Project != NULL
				SELF:CheckProjectType(typeName, SELF:_file:Project, usings)
				IF ! SELF:IsInitialized
					SELF:CheckSystemType(typeName, usings)
				ENDIF
			ENDIF
		PRIVATE METHOD CheckType(typeName AS STRING, defaultNS AS STRING) AS VOID
         IF String.IsNullOrEmpty(typeName)
            RETURN
         ENDIF
			LOCAL usings AS List<STRING>
         usings := List<STRING>{}
         IF SELF:_file != NULL
			   usings:AddRange(SELF:_file:Usings)
            IF SELF:_file?:Project != NULL
               FOREACH VAR ns IN SELF:_file:Project:ImplicitNamespaces
                     usings:AddUnique(ns)
               NEXT
            ENDIF
         ENDIF
			IF ! String.IsNullOrEmpty(defaultNS)
				usings:Add(defaultNS)
			ENDIF
         // Now check all usings
			SELF:CheckType(typeName, usings)
         
		INTERNAL METHOD SimpleTypeToSystemType(kw AS STRING) AS IXType
			LOCAL typeName AS STRING
			LOCAL sType := NULL AS IXType
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
               case "psz"
               case "usual"
                   typeName := kw

				END SWITCH

				IF ( String.IsNullOrEmpty( typeName ) )
					RETURN NULL
				ENDIF
           
				IF SELF:_file != NULL .AND. SELF:_file:Project != NULL
					VAR options := SELF:_file:Project:ParseOptions
					typeName    := typeName:GetSystemTypeName(options:XSharpRuntime)
					sType       := SELF:_file:Project:FindSystemType(typeName, List<STRING>{})
               IF (sType == NULL)
                  // This could happen if we're in the Runtime solution
                  sType    := SELF:_file:Project:Lookup(typeName)
               ENDIF
				ENDIF
			ENDIF
			RETURN sType 

		// Properties
		PROPERTY File AS XFile GET SELF:_file

		PROPERTY ElementType AS CompletionType AUTO

		PROPERTY FullName AS STRING GET SELF:_type?:FullName

		PROPERTY IsInitialized AS LOGIC GET SELF:_type != NULL 

		PROPERTY ParentType AS CompletionType
			GET
				IF (SELF:_type != NULL)
					RETURN CompletionType{SELF:_type:BaseType,_file, ""}
				ENDIF
				IF (SELF:_type != NULL)
					VAR defaultNS := ""
					IF (! String.IsNullOrEmpty(SELF:_type:Namespace))

						defaultNS := SELF:_type:Namespace
					ENDIF
					RETURN CompletionType{SELF:_type:BaseType, SELF:_file, defaultNS}
				ENDIF
				RETURN CompletionType{"System.Object", NULL, ""}
			END GET
		END PROPERTY

		PROPERTY Type     AS IXType            GET SELF:_type
		PROPERTY XTypeDef AS XTypeDefinition   GET SELF:_type ASTYPE XTypeDefinition
      PROPERTY XTypeRef AS XTypeReference    GET SELF:_type ASTYPE XTypeReference
         
      PROPERTY BaseType as STRING
         GET
            IF SELF:IsArray
               RETURN "System.Array"
            ENDIF
            RETURN SELF:Type:BaseType
         END GET
      END PROPERTY

	END CLASS

END NAMESPACE


