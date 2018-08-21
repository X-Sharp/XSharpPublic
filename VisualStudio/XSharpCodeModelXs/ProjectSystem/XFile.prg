//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System
USING System.Linq
USING System.Diagnostics
USING XSharpModel
USING System.Collections.Immutable
USING STATIC XSharpModel.XFileTypeHelpers
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{FullPath,nq}")];
	CLASS XFile
		// Fields
		PRIVATE _globalType		AS XType
		PRIVATE _lastWritten	AS System.DateTime
		PRIVATE _lock			AS OBJECT
		PRIVATE _parsed			AS LOGIC
		PRIVATE _type			AS XFileType
		PRIVATE _typeList		AS ConcurrentDictionary<STRING, XType>
		PRIVATE _entityList		AS List<XElement>
		PRIVATE _usings			AS List<STRING>
		PRIVATE _usingStatics	AS List<STRING>
		PRIVATE filePath AS STRING
		PRIVATE _project AS XProject

		// Methods
		CONSTRUCTOR(fullPath AS STRING)
			SUPER()
			//
			SELF:filePath := fullPath
			SELF:_type := GetFileType(fullPath)
			SELF:InitTypeList()
			SELF:_parsed := ! SELF:HasCode
			SELF:_lock := OBJECT{}
			SELF:_lastWritten := System.DateTime.MinValue

		PROPERTY EntityList AS 	List<XElement> GET _entityList
		METHOD FirstMember() AS XTypeMember
			IF (! SELF:HasCode)
				RETURN NULL
			ENDIF
			IF (SELF:TypeList == NULL || SELF:TypeList:Count == 0)
				RETURN NULL
			ENDIF
			BEGIN LOCK SELF:_lock
				VAR element := SELF:TypeList:FirstOrDefault()
				RETURN element:Value:Members:FirstOrDefault()
			END
			///
			/// <Summary>Find member in file based on 0 based line number</Summary>
			///
			///

		DELEGATE FindMemberComparer (oElement AS XELement, nValue AS LONG ) AS LONG
		METHOD FindMember(oDel AS FindMemberComparer, nValue AS LONG) AS XElement
			LOCAL oResult := NULL_OBJECT AS XElement
			LOCAL oLast AS XElement
			// perform binary search to speed up things
			VAR current := 0
			VAR bottom := 0
			VAR top := _entityList:Count
			oLast := _entityList:FirstOrDefault()
			DO WHILE top - bottom > 1
				// determine middle
				current := (bottom + top) / 2
				VAR oElement := _entityList[current]
				VAR result := oDel(oElement, nValue)
				IF result == 0
					// found
					RETURN oElement
				ELSEIF result = 1 // element is after the search point
					top := current
				ELSE		// element is before the search point
					oLast := oElement
					bottom := current
				ENDIF
			ENDDO
			IF oResult == NULL
				oResult := oLast	// the last entity we saw before the selected line
			ENDIF
			RETURN oResult

		PRIVATE METHOD CompareByLine(oElement AS XELement, nLine AS LONG) AS LONG
			LOCAL nResult AS LONG
			LOCAL nStart, nEnd AS LONG
			nStart := oElement:Range:StartLine
			nEnd   := oElement:Range:EndLine
			IF oElement IS XType
				VAR oType := oElement ASTYPE XType
				IF oType:Members:Count > 0
					nEnd := oType:Members[0]:Range:StartLine-1
				ENDIF
			ENDIF
			IF nStart <= nLine .AND. nEnd>= nLine
				nResult := 0
			ELSEIF nStart > nLine
				nResult := 1
			ELSE
				nResult := -1
			ENDIF
			RETURN nResult

		METHOD FindMemberAtRow(nLine AS LONG) AS XElement
			nLine += 1
			RETURN SELF:FindMember(CompareByLine, nLine)

			///
			/// <Summary>Find member in file based on 0 based position</Summary>
			///
			///
		PRIVATE METHOD CompareByPosition(oElement AS XELement, nPos AS LONG) AS LONG
			LOCAL nResult AS LONG
			LOCAL nStart, nEnd AS LONG
			nStart := oElement:Interval:Start
			nEnd   := oElement:Interval:Stop
			IF oElement IS XType
				VAR oType := oElement ASTYPE XType
				IF oType:Members:Count > 0
					nEnd := oType:Members[0]:Interval:Start-2
				ENDIF
			ENDIF
			IF nStart <= nPos .AND. nEnd >= nPos
				nResult := 0
			ELSEIF nStart > nPos
				nResult := 1
			ELSE
				nResult := -1
			ENDIF
			RETURN nResult

		METHOD FindMemberAtPosition(nPos AS LONG) AS XElement
			RETURN SELF:FindMember(CompareByPosition, nPos)


		METHOD InitTypeList() AS VOID
			IF SELF:HasCode
				SELF:_typeList		:= ConcurrentDictionary<STRING, XType>{System.StringComparer.InvariantCultureIgnoreCase}
				SELF:_globalType	:= XType.CreateGlobalType(SELF)
				SELF:_typeList:TryAdd(SELF:_globalType:Name, SELF:_globalType)
				SELF:_usings		:= List<STRING>{}
				SELF:_usingStatics	:= List<STRING>{}
				SELF:_entityList    := List<XElement>{}
			ENDIF

		METHOD SetTypes(types AS IDictionary<STRING, XType>, usings AS IList<STRING>, ;
			staticusings AS IList<STRING>, aEntities AS IList<XElement>) AS VOID
			IF SELF:HasCode
				WriteOutputMessage("-->> SetTypes() "+ SELF:SourcePath)
				BEGIN LOCK SELF
					FOREACH VAR type IN _typeList
						SELF:Project:RemoveType(type:Value)
					NEXT
					SELF:_typeList:Clear()
					SELF:_usings:Clear()
					SELF:_usingStatics:Clear()
					FOREACH type AS KeyValuePair<STRING, XType> IN types
						SELF:_typeList:TryAdd(type:Key, type:Value)
						IF (XType.IsGlobalType(type:Value))
							SELF:_globalType := type:Value
						ENDIF
						SELF:Project:AddType(type:Value)
					NEXT
					SELF:_usings:AddRange(usings)
					SELF:_usingStatics:AddRange(staticusings)
					SELF:_entityList:Clear()
					SELF:_entityList:AddRange(aEntities)
				END LOCK
				WriteOutputMessage(String.Format("<<-- SetTypes() {0} (Types: {1}, Entities: {2})", SELF:SourcePath, _typeList:Count, SELF:_entityList:Count))
			ENDIF

		METHOD BuildTypes(oInfo AS ParseResult) AS VOID
			LOCAL aTypes	      AS Dictionary<STRING, XType>
			LOCAL aUsings		  AS List<STRING>
			LOCAL aUsingStatics   AS List<STRING>
			LOCAL oType		      AS XType
			LOCAL aEntities		  AS List<XElement>
			aTypes			:= Dictionary<STRING, XType>{}
			aUsings			:= List<STRING>{}
			aUsingStatics	:= List<STRING>{}
			FOREACH oElement AS EntityObject IN oInfo:Types
				oType   := XType.create(SELF, oElement,oInfo)
				IF !aTypes:ContainsKey(oType:FullName)
					aTypes:Add( oType:FullName, oType)
				ELSE
					// this should only happen if there are two PARTIAL CLASS parts in the same file
					// now merge the second in the first
					LOCAL oType2 := aTypes[oType:FullName] AS XType
					IF oType2:File == oType:File
						oType2 := oType2:Merge(oType)
						aTypes[oType:FullName] := oType2
					ENDIF
				ENDIF
				SELF:Project:RemoveMergedType(oType:FullName)
			NEXT
			FOREACH oLine AS LineObject IN oInfo:SpecialLines
				IF oLine:eType == LineType.Using
					LOCAL cName AS STRING
					cName := oLine:cArgument
					IF cName:ToLower():StartsWith("static")
						aUsingStatics:Add(cName:Substring(6))
					ELSE
						aUsings:Add(cName)
					ENDIF
				ENDIF
			NEXT
			// get our objects in file order from the oInfo:Entities list
			aEntities := List<XELement>{}
			FOREACH oElement AS EntityObject IN oInfo:Entities
				IF oElement:oCargo != NULL_OBJECT
					aEntities:add ( (XElement) oElement:oCargo)
				ENDIF
			NEXT
			SELF:SetTypes(aTypes, aUsings, aUsingStatics, aEntities:ToImmutableArray())
			RETURN


		METHOD WaitParsing() AS VOID
			//
			IF SELF:HasCode

				WriteOutputMessage("-->> WaitParsing()")
				BEGIN LOCK SELF:_lock

					IF ! SELF:Parsed
						BEGIN USING VAR walker := SourceWalker{SELF}
							TRY
								VAR lines := System.IO.File.ReadAllLines(SELF:SourcePath)
								VAR info := walker:Parse(lines, FALSE)
								BuildTypes(info)
							CATCH exception AS System.Exception
								XSolution.WriteException(exception)
							END TRY
						END USING
					ENDIF
				END LOCK
				WriteOutputMessage("<<-- WaitParsing()")
			ENDIF


			// Properties
		PROPERTY AllUsingStatics AS IList<STRING>
			GET

				IF (! SELF:HasCode)

					RETURN NULL
				ENDIF
				WriteOutputMessage("-->> AllUsingStatics")
				VAR statics := List<STRING>{}
				BEGIN LOCK SELF:_lock

					statics:AddRange(SELF:_usingStatics)
					IF (((SELF:Project != NULL) .AND. (SELF:Project:ProjectNode != NULL)) .AND. SELF:Project:ProjectNode:ParseOptions:IsDialectVO)

						FOREACH asm AS AssemblyInfo IN SELF:Project:AssemblyReferences

							VAR globalclass := asm:GlobalClassName
							IF (! String.IsNullOrEmpty(globalclass))

								statics:AddUnique(globalclass)
							ENDIF
						NEXT
					ENDIF
				END LOCK
				WriteOutputMessage("<<-- AllUsingStatics")
				RETURN statics
			END GET
		END PROPERTY

		PROPERTY ContentHashCode AS DWORD
			GET
				IF ! SELF:HasCode .OR. SELF:TypeList == NULL
					RETURN 0
				ENDIF
				BEGIN LOCK SELF:_lock
					VAR hash := 0U
					FOREACH type AS XType IN SELF:TypeList:Values
						FOREACH xmem AS XTypeMember IN type:Members
							BEGIN UNCHECKED
								hash += (DWORD)xmem:Prototype:GetHashCode()
								hash += (DWORD) xmem:Range:StartLine
							END UNCHECKED
						NEXT
					NEXT
					RETURN hash
				END LOCK
			END GET
		END PROPERTY

		PROPERTY FullPath AS STRING GET SELF:filePath SET SELF:filePath := VALUE
		PROPERTY GlobalType AS XType GET SELF:_globalType
		PROPERTY HasCode AS LOGIC GET SELF:IsSource .OR. SELF:IsXaml
		PROPERTY HasParseErrors AS LOGIC AUTO
		PROPERTY IsSource AS LOGIC GET SELF:_type == XFileType.SourceCode
		PROPERTY IsXaml AS LOGIC GET SELF:_type == XFileType.XAML
		PROPERTY LastWritten AS System.DateTime GET SELF:_lastWritten SET SELF:_lastWritten := VALUE
		PROPERTY Name AS STRING GET System.IO.Path.GetFileNameWithoutExtension(SELF:filePath)

		PROPERTY Parsed AS LOGIC
			GET
				WriteOutputMessage("-->> Parsed")
				LOCAL flag AS LOGIC
				BEGIN LOCK SELF:_lock

					flag := SELF:_parsed
				END LOCK
				WriteOutputMessage("<<-- Parsed")
				RETURN flag
			END GET
		END PROPERTY

		PROPERTY Project AS XProject
			GET
				IF SELF:_project == NULL
					SELF:_project := XSolution.OrphanedFilesProject
					SELF:_project:AddFile(SELF:filePath)
				ENDIF
				RETURN SELF:_project
			END GET
			SET
				SELF:_project := VALUE
			END SET
		END PROPERTY

		PROPERTY SourcePath AS STRING
			GET
				IF (SELF:IsXaml)
					RETURN SELF:XamlCodeBehindFile
				ENDIF
				RETURN SELF:FullPath
			END GET
		END PROPERTY

		PROPERTY TypeList AS IDictionary<STRING, XType>
			GET
				IF ! SELF:HasCode
					RETURN NULL
				ENDIF
				BEGIN LOCK SELF:_lock
					RETURN SELF:_typeList
				END LOCK
			END GET
		END PROPERTY

		PROPERTY Usings AS IList<STRING>
			GET
				IF ! SELF:HasCode
					RETURN NULL
				ENDIF
				RETURN _usings:ToArray()
			END GET
		END PROPERTY

		PROPERTY XamlCodeBehindFile AS STRING
			GET
				VAR projectNode := SELF:Project:ProjectNode
				RETURN System.IO.Path.ChangeExtension(System.IO.Path.Combine(projectNode:IntermediateOutputPath, System.IO.Path.GetFileName(SELF:FullPath)), ".g.prg")
			END GET
		END PROPERTY

		PROPERTY XFileType AS XFileType GET SELF:_type

		METHOD WriteOutputMessage(message AS STRING) AS VOID
			XSolution.WriteOutputMessage("XModel.File "+message)

	END CLASS

END NAMESPACE


