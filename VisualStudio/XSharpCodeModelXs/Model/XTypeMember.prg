//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{Prototype,nq}")];
	CLASS XTypeMember INHERIT XElement
		// Fields
		PRIVATE _parameters AS List<XVariable>
		PRIVATE _typeName AS STRING

		#region constructors

			PRIVATE  CONSTRUCTOR(name AS STRING, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, span AS TextRange, position AS TextInterval, typeName AS STRING, isStatic AS LOGIC)
				SUPER(name, kind, modifiers, visibility, span, position)
				SELF:Parent := NULL
				SELF:_parameters := List<XVariable>{}
				SELF:_typeName := ""
				SELF:_typeName := typeName
				SELF:_isStatic := isStatic

			STATIC METHOD create(oElement AS EntityObject, oInfo AS ParseResult, oFile AS XFile, oType AS XType) AS XTypeMember
				LOCAL cName := oElement:cName AS STRING
				LOCAL kind  := Etype2Kind(oElement:eType) AS Kind
				LOCAL cType := oElement:cRetType AS STRING
				LOCAL mods  := oElement:eModifiers:ToModifiers() AS Modifiers
				LOCAL vis   := oElement:eAccessLevel:ToModifiers() AS Modifiers
				LOCAL span   AS textRange
				LOCAL intv   AS TextInterval
				LOCAL isStat := oElement:lStatic AS LOGIC
				mods &=  ~Modifiers.VisibilityMask	// remove lower 2 nibbles which contain visibility
				CalculateRange(oElement, oInfo, OUT span, OUT intv)
				LOCAL result := XTypeMember{cName, kind, mods, vis, span, intv, cType, isStat} AS XTypeMember
				result:File := oFile
				IF oElement:aParams != NULL
					FOREACH oParam AS EntityParamsObject IN oElement:aParams
						LOCAL oVar AS XVariable
						span := TextRange{oElement:nStartLine, oParam:nCol, oElement:nStartLine, oParam:nCol+oParam:cName:Length}
						intv := TextInterval{oElement:nOffSet+oParam:nCol, oElement:nOffSet+oParam:nCol+oParam:cName:Length}
						oVar := XVariable{result, oParam:cName, Kind.Local,  span, intv, oParam:cType, TRUE}
						oVar:ParamType := oParam:nParamType
						result:AddParameter(oVar)
					NEXT
				ENDIF
				RETURN result


		#endregion



		METHOD AddParameter(oVar AS XVariable) AS VOID
			oVar:Parent := SELF
			oVar:File := SELF:File
			_parameters:Add(oVar)
			RETURN

		METHOD Namesake() AS List<XTypeMember>
			VAR _namesake := List<XTypeMember>{}
			IF (SELF:Parent != NULL)
				FOREACH  oMember AS XTypeMember IN ((XType) SELF:Parent):Members
					IF String.Compare(oMember:FullName, SELF:FullName, TRUE) == 0 .AND. String.Compare(oMember:Prototype, SELF:Prototype, TRUE) > 0
						////
						_namesake:Add(oMember)
					ENDIF
				NEXT
			ENDIF
			RETURN _namesake
		//
		#region Properties
		PROPERTY Description AS STRING
			GET
				VAR modVis := ""
				IF (SUPER:Modifiers != Modifiers.None)
					modVis := modVis + SUPER:ModifiersKeyword
				ENDIF
				VAR desc := modVis + VisibilityKeyword
				IF (SUPER:Kind != Kind.Field)
					desc := desc + SUPER:KindKeyword
					IF (SUPER:Kind == Kind.VODefine)
						RETURN desc + SUPER:Name
					ENDIF
				ENDIF
				RETURN desc + SELF:Prototype
			END GET
		END PROPERTY

		PROPERTY FullName AS STRING
			GET
				//
				IF (SELF:Parent != NULL)
					//
					RETURN SELF:Parent:FullName +"." + SUPER:Name
				ENDIF
				RETURN SUPER:Name
			END GET
		END PROPERTY

		PROPERTY HasParameters AS LOGIC GET SELF:Kind:HasParameters() .AND. SELF:_parameters:Count > 0
		PROPERTY ParameterCount  AS INT GET SELF:_parameters:Count

		PROPERTY IsArray AS LOGIC AUTO


		NEW PROPERTY Parent AS XType GET (XType) SUPER:parent  SET SUPER:parent := VALUE

		PROPERTY ParameterList AS STRING
			GET
				VAR parameters := ""
				FOREACH variable AS XVariable IN SELF:Parameters
					IF (parameters:Length > 0)
						parameters := parameters + ", "
					ENDIF
					parameters += variable:Name
					IF variable:IsTyped
						parameters += variable:ParamTypeDesc + variable:TypeName
					ENDIF
				NEXT
				RETURN parameters
			END GET
		END PROPERTY

		PROPERTY ComboParameterList AS STRING
			GET
				VAR parameters := ""
				FOREACH variable AS XVariable IN SELF:Parameters
					IF (parameters:Length > 0)
						parameters := parameters + ", "
					ENDIF
					VAR cType := variable:ShortTypeName
					IF variable:IsTyped .AND. variable:ParamType != ParamType.As
						parameters += variable:ParamTypeDesc + cType
					ELSE
						parameters += cType
					ENDIF
				NEXT
				RETURN parameters
			END GET
		END PROPERTY

		PROPERTY Parameters AS IEnumerable<XVariable>
		GET
			RETURN SELF:_parameters
		END GET
		END PROPERTY

		PROPERTY Prototype AS STRING
			GET
				VAR vars := ""
				IF SELF:Kind:HasParameters()
					vars := "(" + SELF:ParameterList + ")"
				ENDIF
				VAR desc := SUPER:Name + vars
				IF SELF:Kind:HasReturnType() .AND. ! String.IsNullOrEmpty(SELF:TypeName)
					desc := desc + AsKeyWord + SELF:TypeName
				ENDIF
				RETURN desc
			END GET
		END PROPERTY

		PROPERTY ComboPrototype AS STRING
			GET
				VAR vars := ""
				IF SELF:Kind:HasParameters()
					vars := "(" + SELF:ComboParameterList + ")"
				ENDIF
				VAR desc := SUPER:Name + vars
				IF SELF:Kind:HasReturnType() .AND. ! String.IsNullOrEmpty(SELF:TypeName)
					desc := desc + AsKeyWord + SELF:TypeName
				ENDIF
				RETURN desc
			END GET
		END PROPERTY
		PROPERTY TypeName AS STRING GET SELF:_typeName
		#endregion
	END CLASS

END NAMESPACE

