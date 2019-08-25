#using System.Collections

CLASS EntityParser
	PROTECT lParsing AS LOGIC
	CONSTRUCTOR()
	RETURN

	VIRTUAL METHOD IsParsing() AS LOGIC
	RETURN SELF:lParsing

	VIRTUAL METHOD ParseModule(/*oModule AS Module*/) AS LOGIC
/*		LOCAL aSourceLines AS ICollection
		LOCAL oGlobalScope AS TypeInfo
		LOCAL aEntities AS ArrayList
		LOCAL oTypeList AS TypeList
		LOCAL oMember AS @@Member
		LOCAL oInfo AS ParseInfo
		LOCAL oType AS TypeInfo
		LOCAL lSuccess AS LOGIC
		LOCAL cName AS STRING
		LOCAL n,m AS INT

		TRY
			SELF:lParsing := TRUE
			oGlobalScope := TypeInfo{"(Global Scope)" , ElementType.Class , AccessType.Public , AccessType.Public , 0 , 0 , 0 ,0}
//			oGlobalScope:Module := oModule
			oTypeList := TypeList{}
			oTypeList:Add(oGlobalScope)

//			aSourceLines := System.IO.File.ReadAllLines(oModule:Name)
//			LOCAL d AS DateTime
//			d := DateTime.Now
			aEntities := CodeEditor.ParseEntities(aSourceLines)
//			? "Real time:" , DateTime.Now - d

			FOR n := 0 UPTO aEntities:Count - 1
				oInfo := (ParseInfo)aEntities[n]
				IF IsType(oInfo:eType)
					oType := TypeInfo{oInfo:cNameSpace + oInfo:cName , EntityToElementType(oInfo:eType) , AccessLevelToAccessType(oInfo:eAccessLevel) , AccessLevelToAccessType(oInfo:eAccessLevel) , oInfo:nLine , 0 , 0 , 0}
//					oType:Parent := oModule
					oType:ParentName := oInfo:cInherit
					oType:DisplayName := oInfo:cRetType
					oType:@@Partial := oInfo:lPartial
					oTypeList:Add(oType)
				ELSE
					DO CASE
					CASE oInfo:eType == EntityType._Constructor
						cName := ".ctor"
					CASE oInfo:eType == EntityType._Destructor
						cName := ".dtor"
					OTHERWISE
						cName := oInfo:cName
					END CASE
					oMember := @@Member{cName , EntityToElementType(oInfo:eType) , AccessLevelToAccessType(oInfo:eAccessLevel) , AccessLevelToAccessType(oInfo:eAccessLevel) , oInfo:nLine , 0 , 0 , 0}
					IF oInfo:eType == EntityType._Function .or. oInfo:eType == EntityType._Global
						oType := oGlobalScope
						IF oInfo:eType == EntityType._Global
							oMember:ElementType := ElementType.IVar
						END IF
					END IF
					IF oType != NULL
						oMember:ReturnType := oInfo:cRetType
						oType:AddMember(oMember)
						IF oInfo:aParams != NULL
							FOR m := 0 UPTO oInfo:aParams:Count - 1
								oMember:AddParameter(oInfo:aParams:GetName(m) , (STRING)oInfo:aParams:GetValue(m))
							NEXT
						END IF
					END IF
				END IF
			NEXT
//			oModule:TypeList := oTypeList
			lSuccess := TRUE

		FINALLY

			SELF:lParsing := FALSE

		END TRY

	RETURN lSuccess
*/
	RETURN TRUE
	INTERNAL STATIC METHOD IsType(eType AS EntityType) AS LOGIC
	RETURN .not. (eType == EntityType._Access .or. eType == EntityType._Assign .or. eType == EntityType._Constructor .or. eType == EntityType._Destructor .or. eType == EntityType._Event .or. eType == EntityType._IVar .or. eType == EntityType._Method .or. eType == EntityType._Function .or. eType == EntityType._Global)

/*	PROTECTED STATIC METHOD EntityToElementType(eType AS EntityType) AS ElementType
		LOCAL eRet AS ElementType
		DO CASE
		CASE eType == EntityType._Access
			eRet := ElementType.Access
		CASE eType == EntityType._Assign
			eRet := ElementType.Assign
		CASE eType == EntityType._Class
			eRet := ElementType.Class
		CASE eType == EntityType._Constructor
			eRet := ElementType.Constructor
		CASE eType == EntityType._Delegate
			eRet := ElementType.Delegate
		CASE eType == EntityType._Destructor
			eRet := ElementType.Destructor
		CASE eType == EntityType._Enum
			eRet := ElementType.Enum
		CASE eType == EntityType._Event
			eRet := ElementType.Event
		CASE eType == EntityType._Function
			eRet := ElementType.Function
		CASE eType == EntityType._Global
			eRet := ElementType.Global
		CASE eType == EntityType._Interface
			eRet := ElementType.Interface
		CASE eType == EntityType._IVar
			eRet := ElementType.IVar
		CASE eType == EntityType._Method
			eRet := ElementType.Method
		CASE eType == EntityType._Structure
			eRet := ElementType.Structure
		CASE eType == EntityType._VOStruct
			eRet := ElementType.VoStruct
		CASE eType == EntityType._Union
			eRet := ElementType.Union
		END CASE
	RETURN eRet

	PROTECTED STATIC METHOD AccessLevelToAccessType(eType AS AccessLevel) AS AccessType
		LOCAL eRet AS AccessType
		DO CASE
		CASE eType == AccessLevel.Hidden
			eRet := AccessType.Hidden
		CASE eType == AccessLevel.Internal
			eRet := AccessType.Internal
		CASE eType == AccessLevel.Protected
			eRet := AccessType.Protected
		CASE eType == AccessLevel.Public
			eRet := AccessType.Public
		CASE eType == AccessLevel.Static
			eRet := AccessType.Static
		END CASE
	RETURN eRet
*/
END CLASS	

