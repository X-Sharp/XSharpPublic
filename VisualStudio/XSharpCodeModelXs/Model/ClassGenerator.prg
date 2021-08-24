//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Based on original work for XIDE
//
using System.Reflection
using System.Linq
using System.Collections.Generic

BEGIN NAMESPACE XSharpModel

PUBLIC ENUM MethodType
	MEMBER @@Method
	MEMBER @@Access
	MEMBER @@Assign
END ENUM

CLASS XClassCreator
	STATIC PROTECT _ImplementatorMode AS LOGIC
	STATIC PROTECT _includehidden AS LOGIC
	STATIC PROTECT _useusings AS LOGIC
	STATIC PROTECT _useaccass AS LOGIC
	STATIC PROTECT _showvirtual AS LOGIC
	STATIC PROTECT _showbeginnamespace AS LOGIC
	STATIC PROTECT _indent AS STRING
	STATIC PROTECT _typeexport AS STRING
	STATIC PROTECT _fieldexport AS STRING
	STATIC PROTECT _fieldprotect AS STRING
	STATIC PROTECT _fieldprivate AS STRING
	STATIC PROTECT _propertyexport AS STRING
	STATIC PROTECT _propertyprotect AS STRING
	STATIC PROTECT _propertyprivate AS STRING
	STATIC PROTECT _methodexport AS STRING
	STATIC PROTECT _methodprotect AS STRING
	STATIC PROTECT _methodprivate AS STRING
	STATIC PROTECT _eventexport AS STRING
	STATIC PROTECT _eventprotect AS STRING
	STATIC PROTECT _eventprivate AS STRING

	STATIC PROTECT _explicitinterface AS LOGIC

	PROTECT aLines AS List<STRING>
	PROTECT aNameSpaces AS SortedList<STRING,STRING>
	PROTECT nSectionLine AS INT
    PROTECT cLocation AS STRING

	CONSTRUCTOR(oType AS Type, cLocation AS STRING)
		SELF:aLines := List<STRING>{}
        SELF:cLocation := cLocation
		SELF:aNameSpaces := SortedList<STRING,STRING>{}
		SELF:CreateCode(oType)
	RETURN

    STATIC METHOD Create(oPeType as XPETypeSymbol) AS List<STRING>
        var asm := oPeType:Assembly
        var file := asm:FileName
        var temp := System.IO.Path.GetTempFileName()
        System.IO.File.Delete(temp)
        System.IO.File.Copy(file, temp)
        var bytes := System.IO.File.ReadAllBytes(temp)
        LOCAL assembly as Assembly
        TRY
            assembly := Assembly.Load(bytes)
        CATCH
            assembly := NULL
        END TRY

        System.IO.File.Delete(temp)
        IF assembly != NULL
            var name := oPeType:FullName
            if oPeType:TypeDef:IsNested
                name := oPeType:TypeDef:DeclaringType:FullName
            ENDIF
            var type := assembly:GetType(name)
            if (type == null ) // dependencies ?
                TRY
                    assembly := Assembly.LoadFrom(file)
                    type := assembly:GetType(name)
                CATCH
                    assembly := NULL
                END TRY
            ENDIF
            RETURN Create(type,file)
        ENDIF
        var result := List<String>{}
        result:Add("// Could not load type information for type "+oPeType:FullName)
        result:Add("// from assembly "+file)
        result:Add("// ")
        result:Add("// Please send the assembly to support@xsharp.eu for investigation")
        return result

	STATIC METHOD Create(oType AS Type, cLocation as STRING) AS List<STRING>
		TRY
			var oCreator := XClassCreator{oType, cLocation}
			return  oCreator:GetLines()
        CATCH e as Exception
            var result := List<String>{}
            result:Add("// Exception occurred during reading type information: ")
            result:Add("// "+e:Message)
            return result
		END TRY


	PROTECTED METHOD GetLines() AS List<STRING>
	RETURN SELF:aLines
	PROTECTED METHOD AddLine(cLine AS STRING , nNest AS INT) AS VOID
		LOCAL n AS INT
		IF nNest > 0
			FOR n := 1 UPTO nNest
				cLine := _indent + cLine
			NEXT
		END IF
		SELF:aLines:Add(cLine)
	RETURN
	PROTECTED STATIC METHOD AddLine(cLine AS STRING , nNest AS INT , aLines AS List<STRING>) AS VOID
		LOCAL n AS INT
		IF nNest > 0
			FOR n := 1 UPTO nNest
				cLine := _indent + cLine
			NEXT
		END IF
		aLines:Add(cLine)
	RETURN
	PROTECTED METHOD BeginSection() AS VOID
		SELF:nSectionLine := SELF:aLines:Count
	RETURN
	PROTECTED METHOD EndSection(nNest AS INT) AS VOID
		IF SELF:nSectionLine+2 != SELF:aLines:Count
			SELF:AddLine("" , nNest)
        ELSE
            DO WHILE SELF:aLines:Count > SELF:nSectionLine
                SELF:aLines:RemoveAt(SELF:aLines:Count-1)
            ENDDO
		END IF
	RETURN

	PROTECTED METHOD CreateCode(oType AS Type) AS VOID
		LOCAL nNest AS INT

		SELF:aLines:Clear()
		SELF:aNameSpaces:Clear()
		IF oType == NULL
			RETURN
		END IF

		IF .not. String.IsNullOrEmpty(oType:@@Namespace) .and. _showbeginnamespace
			SELF:AddLine("BEGIN NAMESPACE " + oType:@@Namespace , nNest)
			SELF:AddLine("" , nNest)
			nNest ++
		END IF

		IF oType:IsSubclassOf(TypeOf(System.Delegate))
			SELF:AddDelegate(oType , nNest)
		ELSEIF oType:IsPublic .or. oType:IsNested
			SELF:AddType(oType , nNest)
		END IF

		IF .not. String.IsNullOrEmpty(oType:@@Namespace) .and. _showbeginnamespace
			nNest --
			SELF:AddLine("" , nNest)
			SELF:AddLine("END NAMESPACE" , nNest)
		END IF

		IF SELF:aNameSpaces:Count != 0
			LOCAL n AS INT
			SELF:aLines:Insert(0 , "")
			FOR n := aNameSpaces:Count - 1 DOWNTO 0
				SELF:aLines:Insert(0 , "using " + aNameSpaces:Values[n])
			NEXT

		END IF

		IF SELF:aLines:Count != 0
			SELF:aLines:Insert(0 , "// Metadata taken from assembly: " + oType:Assembly:FullName)
            SELF:aLines:Insert(1 , "// Location                    : " + SELF:cLocation)
			SELF:aLines:Insert(2 , "")
		END IF

	RETURN

	PROTECTED METHOD AddType(oType AS Type , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		LOCAL cType AS STRING

/*		IF oType:IsSerializable
			SELF:AddLine("[Serializable];" , nNest)
		END IF*/
		AddAttributes(oType , SELF:aLines , nNest)
		cLine := ""
		DO CASE
		CASE oType:IsPublic
			cLine += _typeexport + " "
		CASE oType:IsNotPublic
			cLine += "INTERNAL "
		END CASE

		IF oType:IsClass
			DO CASE
			CASE oType:IsSealed
				cLine += "SEALED "
			CASE oType:IsAbstract
				cLine += "ABSTRACT "
			END CASE
		END IF

		DO CASE
		CASE oType:IsEnum
			cType := "ENUM"
		CASE oType:IsInterface
			cType := "INTERFACE"
		CASE oType:IsValueType
			cType := "STRUCTURE"
		CASE oType:IsClass
			cType := "CLASS"
		END CASE
		cLine += cType + " "
		IF _showbeginnamespace .or. oType:IsNested
			cLine += AdjustKeyword(oType:Name)
		ELSE
			cLine += AdjustKeyword(oType:FullName)
		END IF
		DO CASE
		CASE oType:IsClass .and. oType:BaseType != NULL .and. oType:BaseType != TypeOf(OBJECT)
			cLine += " INHERIT " + GetTypeName(oType:BaseType , SELF:aNameSpaces)
		CASE oType:IsEnum .and. FALSE
			cLine += " AS " + GetTypeName(oType:GetFields(BindingFlags.Static + BindingFlags.Public)[1]:FieldType , SELF:aNameSpaces)
		END CASE

		LOCAL cInterfaces AS STRING
		cInterfaces := SELF:GetInterfaces(oType)
		IF cInterfaces:Length != 0
			IF oType:IsInterface
				cLine += " INHERIT "
			ELSE
				cLine += " IMPLEMENTS "
			END IF
			cLine += cInterfaces
		END IF

		SELF:AddLine(cLine , nNest)

		nNest ++
		IF oType:IsEnum
			AddFields(oType , BindingFlags.Static + BindingFlags.Public , nNest)
		ELSE
			SELF:AddLine("" , nNest)

            IF oType:GetFields():Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Fields", nNest)
			    AddFields(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			    AddFields(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			    AddFields(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			    AddFields(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
                SELF:AddLine("#endregion Fields", nNest)
			    SELF:EndSection(nNest)
            ENDIF
            IF oType:GetEvents():Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Events", nNest)
			    AddEvents(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			    AddEvents(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			    AddEvents(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			    AddEvents(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
                SELF:AddLine("#endregion Event", nNest)
			    SELF:EndSection(nNest)
            ENDIF
            IF oType:GetProperties():Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Properties", nNest)
			    AddProperties(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			    AddProperties(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			    AddProperties(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			    AddProperties(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
                SELF:AddLine("#endregion Properties", nNest)
			    SELF:EndSection(nNest)
            ENDIF
            IF oType:GetMethods():Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Methods", nNest)
			    AddMethods(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			    AddMethods(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			    AddMethods(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			    AddMethods(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
                SELF:AddLine("#endregion Methods", nNest)
			    SELF:EndSection(nNest)
            ENDIF
		END IF
		nNest --

		LOCAL aNested AS Type[]
		LOCAL n AS INT
		aNested := oType:GetNestedTypes(BindingFlags.Public)
		IF aNested:Length != 0
            SELF:AddLine("#region Nested Classes", nNest)
			FOR n := 1 UPTO aNested:Length
				SELF:AddLine("" , nNest)
				IF aNested[n]:IsSubclassOf(TypeOf(System.Delegate))
					SELF:AddDelegate(aNested[n] , nNest + 1)
				ELSE
					SELF:AddType(aNested[n] , nNest + 1)
				END IF
            NEXT
            SELF:AddLine("#endregion Nested Classes", nNest)
		END IF

		SELF:AddLine("END " + cType , nNest)

	RETURN

	PROTECTED METHOD AddDelegate(oType AS Type , nNest AS INT) AS VOID
		LOCAL aParams AS ParameterInfo[]
		LOCAL oParam AS ParameterInfo
		LOCAL oInfo AS MethodInfo
		LOCAL cLine AS STRING
		LOCAL n AS INT

		AddAttributes(oType , SELF:aLines , nNest)
		cLine := ""
		DO CASE
		CASE oType:IsPublic
			cLine += _typeexport + " "
		CASE oType:IsNotPublic
			cLine += "INTERNAL "
		END CASE
		IF _showbeginnamespace .or. oType:IsNested
			cLine += "DELEGATE " + oType:Name
		ELSE
			cLine += "DELEGATE " + oType:FullName
		END IF

		TRY
			oInfo := oType:GetMethod("Invoke")
        CATCH
            oInfo := NULL
		END TRY
		IF oInfo != NULL
			aParams := oInfo:GetParameters()
			cLine += "("
			FOR n := 1 UPTO aParams:Length
				IF n > 1
					cLine += ", "
				END IF
				oParam := aParams[n]
				cLine += AddAttributes(oInfo , NULL , nNest)
				cLine += AdjustKeyword(oParam:Name)
				IF oParam:IsOut .or. oParam:ParameterType:Name:EndsWith("&")
					cLine += " REF "
				ELSE
					cLine += " AS "
				END IF
				cLine += GetTypeName(oParam:ParameterType , SELF:aNameSpaces)
			NEXT
			cLine += ")"

			cLine += " AS " + GetTypeName(oInfo:ReturnType , SELF:aNameSpaces)

			SELF:AddLine(cLine , nNest)
		END IF

	RETURN

	PROTECTED METHOD AddFields(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		VAR aFieldInfo := oType:GetFields(eFlags + BindingFlags.DeclaredOnly):ToList()
        aFieldInfo:Sort( { a,b => String.Compare(a:Name, b:Name) })
		FOREACH var oField in aFieldInfo
			IF oType:IsEnum
				SELF:AddLine("MEMBER " + AdjustKeyword(oField:Name) , nNest)
			ELSE
				AddFieldCode(oField , nNest)
			END IF
		NEXT
	RETURN

	PROTECTED METHOD AddFieldCode(oInfo AS FieldInfo , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		IF .not. _includehidden .and. (oInfo:IsAssembly .or. oInfo:IsPrivate .or. oInfo:IsFamilyOrAssembly)
			RETURN
		END IF
		AddAttributes(oInfo , SELF:aLines , nNest)
		cLine := GetFieldCode(oInfo , TRUE , SELF:aNameSpaces)
		SELF:AddLine(cLine , nNest)
	RETURN

	STATIC METHOD GetFieldCode(oInfo AS FieldInfo , lModifiers AS LOGIC , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL cLine AS STRING

		IF oInfo:IsStatic .and. .not. oInfo:IsLiteral
			cLine := "STATIC "
		ELSE
			cLine := ""
		END IF

		DO CASE
		CASE oInfo:IsInitOnly
			cLine += "INITONLY "
		CASE oInfo:IsLiteral
			cLine += "CONST "
		END CASE

		IF lModifiers
			DO CASE
			CASE oInfo:IsPublic
				cLine += _fieldexport
			CASE oInfo:IsPrivate
				cLine += _fieldprivate
			CASE oInfo:IsFamily
				cLine += _fieldprotect
			CASE oInfo:IsAssembly
				cLine += "INTERNAL"
			CASE oInfo:IsFamilyOrAssembly
				cLine += _fieldprotect + " INTERNAL"
			END CASE
			IF cLine != "" .and. .not. cLine:EndsWith(" ")
				cLine += " "
			END IF
		END IF

		cLine += AdjustKeyword(oInfo:Name)
		IF oInfo:IsLiteral .and. oInfo:GetRawConstantValue() != NULL
			cLine += " := "
			LOCAL oType AS Type
			oType := oInfo:GetRawConstantValue():GetType()
			DO CASE
			CASE oType == TypeOf(STRING)
				cLine += e"\""
			CASE oType == TypeOf(Char)
				cLine += "'"
			END CASE
			cLine += oInfo:GetRawConstantValue():ToString()
			DO CASE
			CASE oType == TypeOf(STRING)
				cLine += e"\""
			CASE oType == TypeOf(Char)
				cLine += "'"
			END CASE
		END IF
		cLine += " AS " + GetTypeName(oInfo:FieldType , aNameSpaces)
	RETURN cLine


	PROTECTED METHOD AddEvents(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		VAR aEventInfo := oType:GetEvents(eFlags + BindingFlags.DeclaredOnly):ToList()
        aEventInfo:Sort( { a,b => String.Compare(a:Name, b:Name)})
		FOREACH var evtInfo in aEventInfo
			AddEventCode(evtInfo , nNest)
		NEXT
	RETURN

	PROTECTED METHOD AddEventCode(oInfo AS EventInfo , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		LOCAL oMInfo AS MethodInfo

		oMInfo := oInfo:GetAddMethod(FALSE)
		IF oMInfo == NULL
			RETURN
		END IF

		IF .not. _includehidden .and. (oMInfo:IsAssembly .or. oMInfo:IsPrivate .or. oMInfo:IsFamilyOrAssembly)
			RETURN
		END IF

		AddAttributes(oInfo , SELF:aLines , nNest)
		IF oMInfo:IsStatic
			cLine := "STATIC "
		ELSE
			cLine := ""
		END IF
		DO CASE
		CASE oMInfo:IsPublic
			cLine += _eventexport
		CASE oMInfo:IsPrivate
			cLine += _eventprivate
		CASE oMInfo:IsFamily
			cLine += _eventprotect
		CASE oMInfo:IsAssembly
			cLine += "INTERNAL"
		CASE oMInfo:IsFamilyOrAssembly
			cLine += _eventprotect + " INTERNAL"
		END CASE
		IF cLine != "" .and. .not. cLine:EndsWith(" ")
			cLine += " "
		END IF
		cLine += "EVENT " + AdjustKeyword(oInfo:Name) + " AS " + GetTypeName(oInfo:EventHandlerType , SELF:aNameSpaces)
		SELF:AddLine(cLine , nNest)
	RETURN


	PROTECTED METHOD AddProperties(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		VAR aPropertyInfo := oType:GetProperties(eFlags + BindingFlags.DeclaredOnly):ToList()
        aPropertyInfo:Sort({ a,b => String.Compare(a:Name, b:Name) })
		FOREACH var oProp in aPropertyInfo
			AddPropertyCode(oProp , nNest)
		NEXT
	RETURN

	PROTECTED METHOD AddPropertyCode(oInfo AS PropertyInfo , nNest AS INT) AS VOID
		IF _useaccass
			IF oInfo:GetGetMethod(TRUE) != NULL
				AddMethodCode(oInfo:GetGetMethod(TRUE) , MethodType.Access , nNest)
			END IF
			IF oInfo:GetSetMethod(TRUE) != NULL
				AddMethodCode(oInfo:GetSetMethod(TRUE) , MethodType.Assign , nNest)
			END IF
		ELSE
			LOCAL cLine AS STRING
            var getMethod := oInfo:GetGetMethod(TRUE)
            var setMethod := oInfo:GetSetMethod(TRUE)
            local isHidden := TRUE as LOGIC
            IF getMethod != NULL .and. (getMethod:IsPublic .or. getMethod:IsFamily)
                isHidden := FALSE
            ENDIF
            IF setMethod != NULL .and. (setMethod:IsPublic .or. setMethod:IsFamily)
                isHidden := FALSE
            ENDIF

			IF .not. _includehidden .and. isHidden
				RETURN
			END IF
			AddAttributes(oInfo , SELF:aLines , nNest)
			cLine := GetPropertyCode(oInfo , NULL , TRUE , SELF:aNameSpaces)
			SELF:AddLine(cLine , nNest)
		END IF
	RETURN

	PUBLIC STATIC METHOD GetPropertyCode(oInfo AS PropertyInfo , cForceName AS STRING , lModifiers AS LOGIC , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL aParams AS ParameterInfo[]
		LOCAL oMethod AS MethodInfo
		LOCAL cName AS STRING
		LOCAL cLine AS STRING
		LOCAL n AS INT

		cName := oInfo:Name
		IF cForceName != NULL
			cName := cForceName
		END IF


		IF oInfo:GetGetMethod() != NULL
			oMethod := oInfo:GetGetMethod()
		ELSEIF oInfo:GetSetMethod() != NULL
			oMethod := oInfo:GetSetMethod()
		ELSE
			LOCAL aMethods AS MethodInfo[]
			aMethods := oInfo:GetAccessors(TRUE)
			oMethod := aMethods[1]
		END IF

		IF oMethod:IsStatic
			cLine := "STATIC "
		ELSE
			cLine := ""
		END IF

		IF lModifiers
			IF oMethod:IsVirtual .and. _showvirtual
				cLine += "VIRTUAL "
			END IF
			DO CASE
			CASE oMethod:IsPublic
				cLine += _propertyexport
			CASE oMethod:IsPrivate
				cLine += _propertyprivate
			CASE oMethod:IsFamily
				cLine += _propertyprotect
			CASE oMethod:IsAssembly
				cLine += "INTERNAL"
			CASE oMethod:IsFamilyOrAssembly
				cLine += _propertyprotect + " INTERNAL"
			END CASE
			IF cLine != "" .and. .not. cLine:EndsWith(" ")
				cLine += " "
			END IF
			DO CASE
			CASE oMethod:IsFinal
				cLine += "ABSTRACT "
			CASE oMethod:IsAbstract
				cLine += "SEALED "
			END CASE
		END IF

		cLine += "PROPERTY " + AdjustKeyword(cName)

//		aParams := oMethod:GetParameters()
		aParams := oInfo:GetIndexParameters()

		IF aParams:Length != 0
			cLine += "["
			FOR n := 1 UPTO aParams:Length
				IF n > 1
					cLine += ", "
				END IF
				cLine += GetParameterCode(aParams[n] , aNameSpaces)
			NEXT
			cLine += "]"
		END IF

		cLine += " AS " + GetTypeName(oInfo:PropertyType , aNameSpaces)

		IF .not. _ImplementatorMode
			IF oInfo:CanRead
				cLine += " GET"
			END IF
			IF oInfo:CanWrite
				cLine += " SET"
			END IF
		END IF

	RETURN cLine



	PROTECTED METHOD AddMethods(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		VAR aMethodInfo := oType:GetMethods(eFlags + BindingFlags.DeclaredOnly):ToList()
        aMethodInfo:Sort( { a,b => String.Compare(a:Name, b:Name) })

		FOREACH Var mi in aMethodInfo
			AddMethodCode(mi , MethodType.Method , nNest)
		NEXT
	RETURN

	PROTECTED METHOD AddMethodCode(oInfo AS MethodInfo , eType AS MethodType , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		IF .not. _includehidden .and. (oInfo:IsAssembly .or. oInfo:IsPrivate .or. oInfo:IsFamilyOrAssembly)
			RETURN
		END IF
		IF oInfo:IsSpecialName
			IF .not. (eType != MethodType.Method .and. (oInfo:Name:StartsWith("get_") .or. oInfo:Name:StartsWith("set_")))
				RETURN
			END IF
		END IF
		AddAttributes(oInfo , SELF:aLines , nNest)
		cLine := GetMethodCode(oInfo , eType , NULL , TRUE , SELF:aNameSpaces)
		SELF:AddLine(cLine , nNest)
	RETURN

	PUBLIC STATIC METHOD GetMethodCode(oInfo AS MethodInfo , eType AS MethodType , cForceName AS STRING , lModifiers AS LOGIC , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL aParams AS ParameterInfo[]
		LOCAL cName AS STRING
		LOCAL cLine AS STRING
		LOCAL n AS INT

		cName := oInfo:Name
//		IF oInfo:IsSpecialName
			IF eType != MethodType.Method
				IF cName:StartsWith("get_") .or. cName:StartsWith("set_")
					cName := cName:Substring(4)
				ENDIF
			END IF
//		END IF

		IF cForceName != NULL
			cName := cForceName
		END IF

		IF oInfo:IsStatic
			cLine := "STATIC "
		ELSE
			cLine := ""
		END IF

		IF lModifiers
			IF oInfo:IsVirtual .and. _showvirtual
				cLine += "VIRTUAL "
			END IF
			IF eType == MethodType.Method
				DO CASE
				CASE oInfo:IsPublic
					cLine += _methodexport
				CASE oInfo:IsPrivate
					cLine += _methodprivate
				CASE oInfo:IsFamily
					cLine += _methodprotect
				CASE oInfo:IsAssembly
					cLine += "INTERNAL"
				CASE oInfo:IsFamilyOrAssembly
					cLine += _methodprotect + " INTERNAL"
				END CASE
			ELSE
				DO CASE
				CASE oInfo:IsPublic
					cLine += _propertyexport
				CASE oInfo:IsPrivate
					cLine += _propertyprivate
				CASE oInfo:IsFamily
					cLine += _propertyprotect
				CASE oInfo:IsAssembly
					cLine += "INTERNAL"
				CASE oInfo:IsFamilyOrAssembly
					cLine += _propertyprotect + " INTERNAL"
				END CASE
			END IF
			IF cLine != "" .and. .not. cLine:EndsWith(" ")
				cLine += " "
			END IF
			IF eType == MethodType.Method
				DO CASE
				CASE oInfo:IsFinal
					cLine += "ABSTRACT "
				CASE oInfo:IsAbstract
					cLine += "SEALED "
				END CASE
			END IF
		END IF

		cLine += eType:ToString():ToUpper() + " " + AdjustKeyword(cName)

		aParams := oInfo:GetParameters()

		IF aParams:Length != 0
			cLine += "("
			IF eType == MethodType.Assign
				cLine += GetParameterCode(aParams[aParams:Length] , aNameSpaces)
				FOR n := 1 UPTO aParams:Length - 1
					cLine += ", "
					cLine += GetParameterCode(aParams[n] , aNameSpaces)
				NEXT
			ELSE
				FOR n := 1 UPTO aParams:Length
					IF n > 1
						cLine += ", "
					END IF
					cLine += GetParameterCode(aParams[n] , aNameSpaces)
				NEXT
			END IF
			cLine += ")"
		ELSEIF eType == MethodType.Method
			cLine += "( )"
		END IF

		IF eType != MethodType.Assign
			cLine += " AS " + GetTypeName(oInfo:ReturnType , aNameSpaces)
		END IF

	RETURN cLine

	PROTECTED STATIC METHOD GetParameterCode(oParam AS ParameterInfo , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL cLine AS STRING
//		cLine := AddAttributes(oParam , NULL , 0)
		cLine += AdjustKeyword(oParam:Name)
		IF oParam:IsOut .or. oParam:ParameterType:Name:EndsWith("&")
			cLine += " REF "
		ELSE
			cLine += " AS "
		END IF
		cLine += GetTypeName(oParam:ParameterType , aNameSpaces)
	RETURN cLine

	PROTECTED STATIC METHOD AddAttributes(oInfo AS MemberInfo , aLines AS List<STRING> , nNest AS INT) AS STRING
		LOCAL aAttributes AS OBJECT[]
		LOCAL oAttribute AS OBJECT
		LOCAL cAttribute AS STRING
		LOCAL oType AS Type
		LOCAL cRet AS STRING
		LOCAL n AS INT
		cRet := ""
		aAttributes := oInfo:GetCustomAttributes(FALSE)
		FOR n := 1 UPTO aAttributes:Length
			oAttribute := aAttributes[n]
			oType := oAttribute:GetType()
			IF TypeOf(Attribute):IsAssignableFrom(oType)
				IF oType:GetConstructors(BindingFlags.Public + BindingFlags.Instance):Length == 1 .and. ;
					oType:GetConstructors(BindingFlags.Public + BindingFlags.Instance)[1]:GetParameters():Length == 0
					cAttribute := oType:Name
					IF cAttribute:EndsWith("Attribute")
						cAttribute := cAttribute:Substring(0 , cAttribute:Length - 9)
					END IF
					cAttribute := "[" + cAttribute + "]"
					IF aLines == NULL
						cRet += cAttribute
					ELSE
						AddLine(cAttribute + ";" , nNest , aLines)
					END IF
				END IF
			END IF
		NEXT
	RETURN cRet

	// Type:GetInterfaces() returns declared and inherited interfaces, we need declared only
	PROTECTED METHOD GetInterfaces(oType AS Type) AS STRING
		LOCAL aInterfaces AS List<Type>
		LOCAL aTest AS Type[]
		LOCAL lFound AS LOGIC
		LOCAL cRet AS STRING
		LOCAL n,m AS INT

		aInterfaces := List<Type>{}
		aTest := oType:GetInterfaces()
		FOR n := 1 UPTO aTest:Length
			IF .not. aInterfaces:Contains(aTest[n])
				IF .not. aTest[n]:FullName:StartsWith("System.Windows.Forms.UnsafeNativeMethods")
					aInterfaces:Add(aTest[n])
				END IF
			END IF
		NEXT

		IF oType:BaseType != NULL
			aTest := oType:BaseType:GetInterfaces()
			FOR n := 1 UPTO aTest:Length
				IF aInterfaces:Contains(aTest[n])
					aInterfaces:Remove(aTest[n])
				END IF
			NEXT
		END IF

		lFound := TRUE
		DO WHILE lFound
			lFound := FALSE
			FOR m := 0 UPTO aInterfaces:Count - 1
				aTest := aInterfaces[m]:GetInterfaces()
				FOR n := 1 UPTO aTest:Length
					IF aInterfaces:Contains(aTest[n])
						aInterfaces:Remove(aTest[n])
						lFound := TRUE
						EXIT
					END IF
				NEXT
				IF lFound
					LOOP
				END IF
			NEXT
		END DO

		IF aInterfaces:Count == 0
			cRet := ""
		ELSE
			FOR n := 0 UPTO aInterfaces:Count - 1
				IF n > 0
					cRet += ", "
				END IF
				cRet += GetTypeName(aInterfaces[n] , SELF:aNameSpaces)
			NEXT
		END IF

	RETURN cRet


	PROTECTED STATIC METHOD GetTypeName(oType AS Type , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL cType AS STRING
		LOCAL n AS INT
		IF oType:IsArray
			cType := GetTypeName(oType:GetElementType() , aNameSpaces)
			cType += "["
			FOR n := 1 UPTO oType:GetArrayRank() - 1
				cType += ","
			NEXT
			cType += "]"
		ELSEIF oType:IsGenericType
			cType := AdjustTypeName(oType:GetGenericTypeDefinition() , aNameSpaces)
			IF cType:IndexOf('`') != -1
				LOCAL aArgs AS Type[]
				cType := cType:Substring(0 , cType:IndexOf('`'))
				cType += "< "
				aArgs := oType:GetGenericArguments()
				FOR n := 1 UPTO aArgs:Length
					IF n > 1
						cType += ", "
					END IF
					cType += GetTypeName(aArgs[n] , aNameSpaces)
				NEXT
				cType += " >"
			END IF
		ELSE
			LOCAL cTypeName AS STRING
			cTypeName := oType:FullName
            cType := cTypeName:GetXSharpTypeName()
		END IF
		cType := cType:Replace('+' , '.') // nested classes
		cType := cType:Replace("&" , "") // by reference
	RETURN cType

	PROTECTED STATIC METHOD AdjustTypeName(oType AS Type , aNameSpaces AS SortedList<STRING,STRING>) AS STRING
		LOCAL cType AS STRING
		IF _useusings .and. aNameSpaces != NULL
			LOCAL cNamespace AS STRING
			cNamespace := oType:@@Namespace
			IF .not. String.IsNullOrEmpty(cNamespace)
				IF .not. aNameSpaces:ContainsKey(cNamespace:ToUpper())
					aNameSpaces:Add(cNamespace:ToUpper() , cNamespace)
				END IF
			END IF
		END IF
		IF aNameSpaces == NULL
			cType := oType:FullName
		ELSE
			cType := oType:Name
		END IF
//		cType := cType:Replace('+' , '.') // nested classes
		cType := AdjustKeyword(cType)
	RETURN cType

	STATIC PROTECT keywords AS Dictionary<STRING , STRING>
	STATIC METHOD AdjustKeyword(cWord AS STRING) AS STRING
		IF keywords:ContainsKey(cWord:ToUpper())
			cWord := "@@" + cWord
		END IF
	RETURN cWord
	STATIC CONSTRUCTOR()
		SetDefinitionMode()
		LOCAL aKeywords := <STRING>{;
		"BYTE","SHORT","SHORTINT","WORD",;
		"INT","LONG","LONGINT","DWORD",  "INT64",;
		"REAL4","REAL8","FLOAT",  "LOGIC",  "STRING","PSZ",;
		"USUAL","ARRAY","DATE",;
		"OBJECT","SYMBOL","CODEBLOCK","PTR",;
		"VOID","NIL","NULL","TRUE","FALSE",;
		"STRUCTURE","STRUCT","VOSTRUCT","ALIGN","UNION","MEMBER",;
		"ENUM","EVENT",;
		"DELEGATE","GLOBAL","USING","NAMESPACE",;
		"CLASS","INHERIT","HIDDEN","SELF","SUPER","PARTIAL","INTERFACE",;
		"FUNCTION","FUNC","PROCEDURE","PROC","RETURN",;
		"METHOD","ACCESS","ASSIGN",;
		"CONSTRUCTOR","DESTRUCTOR",;
		"LOCAL","STATIC","PROTECT","INSTANCE","EXPORT",;
		"AS","IS","_CAST","REF","DIM",;
		"CLIPPER","PASCAL","CDECL","STRICT","WINAPI",;
		"FOR","FOREACH","NEXT","TO","UPTO","DOWNTO","STEP","LOOP","EXIT",;
		"IF","ELSE","ELSEIF","ENDIF","DO","CASE","OTHERWISE","WHILE","ENDDO","ENDCASE","REPEAT","UNTIL",;
		"BEGIN","SEQUENCE","END","RECOVER","FINALLY","BREAK","LOCK","TRY","CATCH","THROW",;
		"PRIVATE","PROTECTED","IMPLEMENTS","SEALED","ABSTRACT","PUBLIC",;
		"FIELD","_DLL","INTERNAL","CONST","INITONLY","VIRTUAL",;
		"OPERATOR","DECLARE","PROPERTY","SCOPE","IMPLIED","IN",;
		"_INIT1","_INIT2","_INIT3";
		} AS STRING[]

		keywords := Dictionary<STRING , STRING>{}

		LOCAL n AS INT
		FOR n := 1 UPTO aKeywords:Length
			keywords:Add(aKeywords[n] , aKeywords[n])
		NEXT

	RETURN

	STATIC METHOD SetDefinitionMode() AS VOID
		_ImplementatorMode := FALSE
		_includehidden := FALSE
//		_includehidden := TRUE
		_useusings := TRUE
		_showvirtual := TRUE
		_showbeginnamespace := FALSE
		_indent := e"\t"
		_typeexport := "PUBLIC"
		_fieldexport := "EXPORT"
		_fieldprotect := "PROTECT"
		_fieldprivate := "PRIVATE"
		_propertyexport := ""
		_propertyprotect := "PROTECTED"
		_propertyprivate := "PRIVATE"
		_methodexport := ""
		_methodprotect := "PROTECTED"
		_methodprivate := "PRIVATE"
		_eventexport := ""
		_eventprotect := "PROTECTED"
		_eventprivate := "PRIVATE"
	RETURN
	STATIC METHOD SetImplementorMode(lExplicitly AS LOGIC) AS VOID
		_ImplementatorMode := TRUE
		_includehidden := TRUE
		_useusings := FALSE
		_explicitinterface := lExplicitly
	RETURN

END CLASS


END NAMESPACE // XSharpModel
