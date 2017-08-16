#using System.Collections.Generic
#using System.Collections

BEGIN NAMESPACE Xide

ENUM EntityType AS Int32 // todo need to add delegate, operator
	MEMBER _None
	MEMBER _Constructor
	MEMBER _Destructor
	MEMBER _Method
	MEMBER _Access
	MEMBER _Assign
	MEMBER _Class
	MEMBER _Function
	MEMBER _Procedure
	MEMBER _Enum
	MEMBER _VOStruct
	MEMBER _Global
	MEMBER _Structure
	MEMBER _Interface
	MEMBER _Delegate
	MEMBER _Event
	MEMBER @@_Field
	MEMBER _Union
	MEMBER _Operator
	MEMBER _Local
	MEMBER _Property

	MEMBER _Define
	MEMBER _Resource
	MEMBER _TextBlock
END ENUM

[Flags];
ENUM EntityModifiers AS Int32
	MEMBER _None := 0
	MEMBER _Protected := 1
	MEMBER _Private := 2
	MEMBER _Internal := 4
	MEMBER _Virtual := 8
	MEMBER _Abstract := 16
	MEMBER _Sealed := 32
	MEMBER _Static := 64
	MEMBER _Partial := 128
	MEMBER _New := 256
END ENUM

ENUM AccessLevel
	MEMBER @@Public := 0
	MEMBER @@Protected := 1
	MEMBER @@Hidden := 2
	MEMBER @@Internal := 4
END ENUM

CLASS EntityParamsObject
	EXPORT cName AS STRING
	EXPORT cType AS STRING
	EXPORT lReference AS LOGIC
	CONSTRUCTOR(_cName AS STRING , _cType AS STRING)
		SUPER()
		SELF:cName := _cName
		SELF:cType := _cType
	RETURN
	ACCESS IntelText AS STRING
		LOCAL cRet AS STRING
		cRet := SELF:cName
		IF .not. String.IsNullOrWhiteSpace(SELF:cType)
			cRet += iif(SELF:lReference , " REF " , " AS ") + SELF:cType
		END IF
	RETURN cRet
	METHOD Clone() AS EntityParamsObject
	RETURN (EntityParamsObject)SELF:MemberwiseClone()
END CLASS

CLASS EntityObject
	EXPORT eType AS EntityType
	EXPORT cName,cInherit,cRetType,cImplements AS STRING
	EXPORT eModifiers AS EntityModifiers
	EXPORT eAccessLevel AS AccessLevel
	EXPORT cShortClassName AS STRING
	EXPORT cTypedClassName AS STRING
	EXPORT cClassNamespace AS STRING
	EXPORT aParams AS List<EntityParamsObject>
	EXPORT nLine , nCol AS INT
	EXPORT aNameSpaces AS List<STRING> // prosoxh, kai se functions etc
	EXPORT lStatic AS LOGIC
	EXPORT lPartial AS LOGIC
	EXPORT cClassType AS STRING
	EXPORT lExtension AS LOGIC

	CONSTRUCTOR()
		SUPER()
		SELF:cInherit := ""
		SELF:cImplements := ""
		SELF:cRetType := ""
		SELF:cClassType := ""
	RETURN
	
	VIRTUAL METHOD Clone() AS EntityObject
		LOCAL oEntity AS EntityObject
		oEntity := (EntityObject)SELF:MemberwiseClone()
		IF oEntity:aParams != NULL
			oEntity:aParams := List<EntityParamsObject>{}
			FOREACH oParam AS EntityParamsObject IN SELF:aParams
				oEntity:aParams:Add(oParam:Clone())
			NEXT
		END IF
		IF oEntity:aNameSpaces != NULL
			oEntity:aNameSpaces := List<STRING>{}
			FOREACH cNameSpace AS STRING IN oEntity:aNameSpaces
				oEntity:aNameSpaces:Add(cNameSpace)
			NEXT
		END IF
	RETURN oEntity
	
	PROPERTY HasParams AS LOGIC GET aParams != NULL .and. aParams:Count != 0
	
	METHOD NamespacesEqual(_aNameSpaces AS List<STRING>) AS LOGIC
		LOCAL n AS INT
		IF SELF:aNameSpaces == NULL .or. SELF:aNameSpaces:Count != _aNameSpaces:Count
			RETURN FALSE
		END IF
		FOR n := 0 UPTO SELF:aNameSpaces:Count - 1
			IF SELF:aNameSpaces[n] != _aNameSpaces[n]
				RETURN FALSE
			END IF
		NEXT
	RETURN TRUE
	METHOD SetNamespaces(_aNameSpaces AS List<STRING>) AS VOID
		LOCAL n AS INT
		IF SELF:NamespacesEqual(_aNameSpaces)
			RETURN
		END IF 
		IF SELF:aNameSpaces == NULL
			SELF:aNameSpaces := List<STRING>{_aNameSpaces:Count}
		END IF
		FOR n := 0 UPTO _aNameSpaces:Count - 1
			SELF:aNameSpaces:Add(_aNameSpaces[n])
		NEXT
	RETURN
	
	ACCESS FullClassName AS STRING
		LOCAL cRet AS STRING
		cRet := SELF:cShortClassName
		IF .not. String.IsNullOrEmpty(SELF:cClassNamespace)
			cRet := SELF:cClassNamespace + "." + cRet
		END IF
	RETURN cRet
	ACCESS FullName AS STRING
		LOCAL cRet AS STRING
		DO CASE
		CASE SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Interface .or. SELF:eType == EntityType._Structure
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. SELF:eType == EntityType._Property .or. ;
				SELF:eType == EntityType._Method .or. SELF:eType == EntityType._Field .or. SELF:eType == EntityType._Event
			cRet := SELF:FullClassName + "." + SELF:cName
		CASE SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. SELF:eType == EntityType._Global
			cRet := SELF:cName
		CASE SELF:eType == EntityType._Enum
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._Delegate
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union
			cRet := SELF:FullClassName
		OTHERWISE
			cRet := ""
		END CASE
	RETURN cRet
		
	ACCESS IsType AS LOGIC
	RETURN SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Structure .or. ;
			SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union .or. ;
			SELF:eType == EntityType._Interface .or. SELF:eType == EntityType._Delegate .or. ;
			SELF:eType == EntityType._Enum

	ACCESS IsClassOrMember AS LOGIC
	RETURN SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Method .or. ;
			SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. SELF:eType == EntityType._Property .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor

	ACCESS NonClass AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._Global

	ACCESS IsCode AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor .or. ;
			SELF:eType == EntityType._Method .or. SELF:eType == EntityType._Operator .or. SELF:eType == EntityType._Property

	ACCESS IsVO AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union .or. ;
			SELF:eType == EntityType._Global

	ACCESS IsFuncProc AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure
	ACCESS IsFuncProcGlobal AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. SELF:eType == EntityType._Global

	ACCESS StringEntityType AS STRING
	RETURN SELF:eType:ToString():Substring(1):ToUpper()

	METHOD AddParam(cParam AS STRING) AS VOID
		IF SELF:aParams == NULL
			SELF:aParams := List<EntityParamsObject>{}
		END IF
		SELF:aParams:Add(EntityParamsObject{cParam , "USUAL"})
	RETURN
	METHOD AddParam(cParam AS STRING , cType AS STRING) AS VOID
		IF SELF:aParams == NULL
			SELF:aParams := List<EntityParamsObject>{}
		END IF
		SELF:aParams:Add(EntityParamsObject{cParam , cType})
	RETURN
	METHOD SetParamType(cType AS STRING) AS VOID
		LOCAL oParam AS EntityParamsObject
		IF SELF:aParams == NULL .or. SELF:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
			RETURN
		END IF
		oParam := SELF:aParams[SELF:aParams:Count - 1]
		IF cType:Contains("&")
			cType := cType:Replace("&" , "")
			oParam:lReference := TRUE
		END IF
		oParam:cType := cType
	RETURN

	VIRTUAL METHOD ToString() AS STRING
		LOCAL cRet AS STRING
//		LOCAL n AS INT
		IF SELF:eType == EntityType._Property .or. SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor .or. SELF:eType == EntityType._Event .or. ;
			SELF:eType == EntityType._Field .or. SELF:eType == EntityType._Method
			cRet := "    "
		ELSE
			cRet := ""
		END IF
		cRet += SELF:eType:ToString() + " "
		cRet += SELF:cName
		IF .not. String.IsNullOrEmpty(SELF:cInherit)
			cRet += " (" + SELF:cInherit + ") "
		END IF
	RETURN cRet
	
END CLASS

END NAMESPACE
