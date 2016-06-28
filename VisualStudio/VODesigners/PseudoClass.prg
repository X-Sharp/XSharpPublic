#using System.Collections.Generic
#using System.Reflection

INTERNAL ENUM MethodType
	MEMBER @@Method
	MEMBER @@Access
	MEMBER @@Assign
END ENUM

CLASS Vulcan.PseudoClassCreator
	STATIC EXPORT _includehiddenmembers := FALSE AS LOGIC
	STATIC EXPORT _showusingdiectives := TRUE AS LOGIC
	STATIC EXPORT _showvirtualkeyword := TRUE AS LOGIC
	STATIC EXPORT _showbeginnamespace := TRUE AS LOGIC
	STATIC EXPORT _showattributes := TRUE AS LOGIC

	STATIC EXPORT _indent := e"\t" AS STRING

	STATIC EXPORT _typeexport := "PUBLIC" AS STRING
	STATIC EXPORT _fieldexport := "EXPORT" AS STRING
	STATIC EXPORT _fieldprotect := "PROTECT" AS STRING
	STATIC EXPORT _fieldprivate := "PRIVATE" AS STRING
	STATIC EXPORT _propertyexport := "" AS STRING
	STATIC EXPORT _propertyprotect := "PROTECTED" AS STRING
	STATIC EXPORT _propertyprivate := "PRIVATE" AS STRING
	STATIC EXPORT _methodexport := "" AS STRING
	STATIC EXPORT _methodprotect := "PROTECTED" AS STRING
	STATIC EXPORT _methodprivate := "PRIVATE" AS STRING
	STATIC EXPORT _eventexport := "" AS STRING
	STATIC EXPORT _eventprotect := "PROTECTED" AS STRING
	STATIC EXPORT _eventprivate := "PRIVATE" AS STRING
	
	PROTECT aLines AS List<STRING>
	PROTECT aNamespaces AS SortedList<STRING,STRING>
	PROTECT nSectionLine AS INT
	
	CONSTRUCTOR(oType AS Type)
		SELF:aLines := List<STRING>{}
		SELF:aNamespaces := SortedList<STRING,STRING>{}
		SELF:CreateCode(oType)
	RETURN

	STATIC METHOD Create(oType AS Type) AS List<STRING>
		LOCAL aLines AS List<STRING>
		LOCAL oCreator AS PseudoClassCreator
		TRY
			oCreator := PseudoClassCreator{oType}
			aLines := oCreator:GetLines()
        CATCH
            aLines := List<STRING>{}
		END TRY
	RETURN aLines
    
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
	PROTECTED METHOD BeginSection() AS VOID
		SELF:nSectionLine := SELF:aLines:Count
	RETURN
	PROTECTED METHOD EndSection(nNest AS INT) AS VOID
		IF SELF:nSectionLine != SELF:aLines:Count
			SELF:AddLine("" , nNest)
		END IF
	RETURN
	
	PROTECTED METHOD CreateCode(oType AS Type) AS VOID
		LOCAL nNest := 0  AS INT

		SELF:aLines:Clear()
		SELF:aNamespaces:Clear()
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
		ELSE
			SELF:AddType(oType , nNest)
		END IF

		IF .not. String.IsNullOrEmpty(oType:@@Namespace) .and. _showbeginnamespace
			nNest --
			SELF:AddLine("" , nNest)
			SELF:AddLine("END NAMESPACE" , nNest)
		END IF
		
		IF SELF:aNamespaces:Count != 0
			LOCAL n AS INT
			SELF:aLines:Insert(0 , "")
			FOR n := aNamespaces:Count - 1 DOWNTO 0
				SELF:aLines:Insert(0 , "#using " + aNamespaces:Values[n])
			NEXT
			
		END IF
	RETURN
		
	PROTECTED METHOD AddType(oType AS Type , nNest AS INT) AS VOID
		LOCAL cLine := "" AS STRING
		LOCAL cType := "" AS STRING
		
/*		IF oType:IsSerializable
			SELF:AddLine("[Serializable];" , nNest)
		END IF*/
		SELF:AddAttributes(oType , FALSE , nNest)
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
			cLine += " INHERIT " + SELF:GetTypeName(oType:BaseType)
		CASE oType:IsEnum .and. FALSE
			cLine += " AS " + SELF:GetTypeName(oType:GetFields(BindingFlags.Static + BindingFlags.Public)[1]:FieldType)
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

			SELF:BeginSection()
			AddFields(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			AddFields(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			AddFields(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			AddFields(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			SELF:EndSection(nNest)
	
			SELF:BeginSection()
			AddEvents(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			AddEvents(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			AddEvents(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			AddEvents(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			SELF:EndSection(nNest)
	
			SELF:BeginSection()
			AddProperties(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			AddProperties(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			AddProperties(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			AddProperties(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			SELF:EndSection(nNest)
	
			SELF:BeginSection()
			AddMethods(oType , BindingFlags.Static + BindingFlags.NonPublic , nNest)
			AddMethods(oType , BindingFlags.Static + BindingFlags.Public , nNest)
			AddMethods(oType , BindingFlags.Instance + BindingFlags.NonPublic , nNest)
			AddMethods(oType , BindingFlags.Instance + BindingFlags.Public , nNest)
			SELF:EndSection(nNest)
		END IF
		nNest --

		LOCAL aNested AS Type[]
		LOCAL n AS INT
		aNested := oType:GetNestedTypes(BindingFlags.Public)
		IF aNested:Length != 0
			FOR n := 1 UPTO aNested:Length
				SELF:AddLine("" , nNest)
				IF aNested[n]:IsSubclassOf(TypeOf(System.Delegate))
					SELF:AddDelegate(aNested[n] , nNest + 1)
				ELSE
					SELF:AddType(aNested[n] , nNest + 1)
				END IF
			NEXT
		END IF
	
		SELF:AddLine("END " + cType , nNest)
		
	RETURN
	
	PROTECTED METHOD AddDelegate(oType AS Type , nNest AS INT) AS VOID
		LOCAL aParams AS ParameterInfo[]
		LOCAL oParam AS ParameterInfo
		LOCAL oInfo := NULL AS MethodInfo
		LOCAL cLine AS STRING
		LOCAL n AS INT

		SELF:AddAttributes(oType , FALSE , nNest)
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
		END TRY
		IF oInfo != NULL
			aParams := oInfo:GetParameters()
			cLine += "("
			FOR n := 1 UPTO aParams:Length
				IF n > 1
					cLine += ", "
				END IF
				oParam := aParams[n]
				cLine += SELF:AddAttributes(oInfo , TRUE , nNest)
				cLine += AdjustKeyword(oParam:Name)
				IF oParam:IsOut
					cLine += " REF "
				ELSE
					cLine += " AS "
				END IF
				cLine += SELF:GetTypeName(oParam:ParameterType)
			NEXT
			cLine += ")"
		
			cLine += " AS " + SELF:GetTypeName(oInfo:ReturnType)

			SELF:AddLine(cLine , nNest)
		END IF

	RETURN

	PROTECTED METHOD AddFields(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		LOCAL aFieldInfo AS FieldInfo[]
		LOCAL n AS INT
		aFieldInfo := oType:GetFields(eFlags + BindingFlags.DeclaredOnly)
		FOR n := 1 UPTO aFieldInfo:Length
			IF oType:IsEnum
				SELF:AddLine("MEMBER " + AdjustKeyword(aFieldInfo[n]:Name) , nNest)
			ELSE
				AddFieldCode(aFieldInfo[n] , nNest)
			END IF
		NEXT
	RETURN 
	
	PROTECTED METHOD AddFieldCode(oInfo AS FieldInfo , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		
		IF .not. _includehiddenmembers .and. (oInfo:IsAssembly .or. oInfo:IsPrivate .or. oInfo:IsFamilyOrAssembly)
			RETURN
		END IF

		SELF:AddAttributes(oInfo , FALSE , nNest)
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
		cline += " AS " + SELF:GetTypeName(oInfo:FieldType)
		
		SELF:AddLine(cLine , nNest)
	RETURN
	
	
	PROTECTED METHOD AddEvents(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		LOCAL aEventInfo AS EventInfo[]
		LOCAL n AS INT
		aEventInfo := oType:GetEvents(eFlags + BindingFlags.DeclaredOnly)
		FOR n := 1 UPTO aEventInfo:Length
			AddEventCode(aEventInfo[n] , nNest)
		NEXT
	RETURN 
	
	PROTECTED METHOD AddEventCode(oInfo AS EventInfo , nNest AS INT) AS VOID
		LOCAL cLine AS STRING
		LOCAL oMInfo AS MethodInfo

		oMInfo := oInfo:GetAddMethod(FALSE)
		IF oMInfo == NULL
			RETURN
		END IF

		IF .not. _includehiddenmembers .and. (oMInfo:IsAssembly .or. oMInfo:IsPrivate .or. oMInfo:IsFamilyOrAssembly)
			RETURN
		END IF

		SELF:AddAttributes(oInfo , FALSE , nNest)
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
		cLine += "EVENT " + AdjustKeyword(oInfo:Name) + " AS " + SELF:GetTypeName(oInfo:EventHandlerType)
		SELF:AddLine(cLine , nNest)
	RETURN
	
	
	PROTECTED METHOD AddProperties(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		LOCAL aPropertyInfo AS PropertyInfo[]
		LOCAL n AS INT
		aPropertyInfo := oType:GetProperties(eFlags + BindingFlags.DeclaredOnly)
		FOR n := 1 UPTO aPropertyInfo:Length
			AddPropertyCode(aPropertyInfo[n] , nNest)
		NEXT
	RETURN 
	
	PROTECTED METHOD AddPropertyCode(oInfo AS PropertyInfo , nNest AS INT) AS VOID
		IF oInfo:GetGetMethod(TRUE) != NULL
			AddMethodCode(oInfo:GetGetMethod(TRUE) , MethodType.Access , nNest)
		END IF
		IF oInfo:GetSetMethod(TRUE) != NULL
			AddMethodCode(oInfo:GetSetMethod(TRUE) , MethodType.Assign , nNest)
		END IF
	RETURN
	
	
	PROTECTED METHOD AddMethods(oType AS Type , eFlags AS BindingFlags , nNest AS INT) AS VOID
		LOCAL aMethodInfo AS MethodInfo[]
		LOCAL n AS INT
		aMethodInfo := oType:GetMethods(eFlags + BindingFlags.DeclaredOnly)
		FOR n := 1 UPTO aMethodInfo:Length
			AddMethodCode(aMethodInfo[n] , MethodType.Method , nNest)
		NEXT
	RETURN 
	
	INTERNAL METHOD AddMethodCode(oInfo AS MethodInfo , eType AS MethodType , nNest AS INT) AS VOID
		LOCAL aParams AS ParameterInfo[]
		LOCAL oParam AS ParameterInfo
		LOCAL cName AS STRING
		LOCAL cLine AS STRING
		LOCAL n AS INT
		
		IF .not. _includehiddenmembers .and. (oInfo:IsAssembly .or. oInfo:IsPrivate .or. oInfo:IsFamilyOrAssembly)
			RETURN
		END IF

		cName := oInfo:Name
		IF oInfo:IsSpecialName
			IF eType != MethodType.Method .and. (cName:StartsWith("get_") .or. cName:StartsWith("set_"))
				cName := cName:Substring(4)
			ELSE
				RETURN
			END IF
		END IF
	
		SELF:AddAttributes(oInfo , FALSE , nNest)
		IF oInfo:IsStatic
			cLine := "STATIC "
		ELSE
			cLine := ""
		END IF
		IF oInfo:IsVirtual .and. _showvirtualkeyword
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
		cLine += eType:ToString():ToUpper() + " " + AdjustKeyword(cName)
	
		aParams := oInfo:GetParameters()
	
		IF eType != MethodType.Access .or. aParams:Length != 0
			cLine += "("
			FOR n := 1 UPTO aParams:Length
				IF n > 1
					cLine += ", "
				END IF
				oParam := aParams[n]
				cLine += SELF:AddAttributes(oInfo , TRUE , nNest)
				cLine += AdjustKeyword(oParam:Name)
				IF oParam:IsOut
					cLine += " REF "
				ELSE
					cLine += " AS "
				END IF
				cLine += SELF:GetTypeName(oParam:ParameterType)
			NEXT
			cLine += ")"
		END IF
	
		IF eType != MethodType.Assign
			cLine += " AS " + SELF:GetTypeName(oInfo:ReturnType)
		END IF
		SELF:AddLine(cLine , nNest)
	RETURN

	PROTECTED METHOD AddAttributes(oInfo AS MemberInfo , lInline AS LOGIC , nNest AS INT) AS STRING
		LOCAL aAttributes AS OBJECT[]
		LOCAL oAttribute AS OBJECT
		LOCAL cAttribute AS STRING
		LOCAL oType AS Type
		LOCAL cRet AS STRING
		LOCAL n AS INT

		cRet := ""
		IF .not. _showattributes
			RETURN cRet
		END IF
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
					IF lInline
						cRet += cAttribute
					ELSE
						SELF:AddLine(cAttribute + ";" , nNest)
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
		LOCAL cRet := "" AS STRING
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
				cRet += SELF:GetTypeName(aInterfaces[n])
			NEXT
		END IF
		
	RETURN cRet

	PROTECTED METHOD GetTypeName(oType AS Type) AS STRING
		LOCAL cType AS STRING
		LOCAL n AS INT
		IF oType:IsArray
			cType := SELF:GetTypeName(oType:GetElementType())
			cType += "["
			FOR n := 1 UPTO oType:GetArrayRank() - 1
				cType += ","
			NEXT
			cType += "]"
		ELSEIF oType:IsGenericType
			cType := SELF:AdjustTypeName(oType:GetGenericTypeDefinition())
			IF cType:IndexOf('`') != -1
				LOCAL aArgs AS Type[]
				cType := cType:Substring(0 , cType:IndexOf('`'))
				cType += "< "
				aArgs := oType:GetGenericArguments()
				FOR n := 1 UPTO aArgs:Length
					IF n > 1
						cType += ", "
					END IF
					cType += SELF:GetTypeName(aArgs[n])
				NEXT
				cType += " >"
			END IF
		ELSE
			DO CASE
			CASE oType == TypeOf(STRING)
				cType := "STRING"
			CASE oType == TypeOf(OBJECT)
				cType := "OBJECT"
			CASE oType == TypeOf(INT)
				cType := "INT"
			CASE oType == TypeOf(VOID)
				cType := "VOID"
			//CASE oType == TypeOf(USUAL)
			//	cType := "USUAL"
			CASE oType == TypeOf(INT64)
				cType := "INT64"
			CASE oType == TypeOf(BYTE)
				cType := "BYTE"
			CASE oType == TypeOf(SHORTINT)
				cType := "SHORTINT"
			CASE oType == TypeOf(WORD)
				cType := "WORD"
			CASE oType == TypeOf(DWORD)
				cType := "DWORD"
			//CASE oType == TypeOf(FLOAT)
			//	cType := "FLOAT"
			CASE oType == TypeOf(REAL4)
				cType := "REAL4"
			CASE oType == TypeOf(REAL8)
				cType := "REAL8"
			CASE oType == TypeOf(LOGIC)
				cType := "LOGIC"
			//CASE oType == TypeOf(DATE)
			//	cType := "DATE"
			//CASE oType == TypeOf(PSZ)
			//	cType := "PSZ"
			//CASE oType == TypeOf(CODEBLOCK)
			//	cType := "CODEBLOCK"
			//CASE oType == TypeOf(_CODEBLOCK)
			//	cType := "_CODEBLOCK"
			//CASE oType == TypeOf(ARRAY)
			//	cType := "ARRAY"
			//CASE oType == TypeOf(SYMBOL)
			//	cType := "SYMBOL"
			CASE oType == TypeOf(PTR)
				cType := "PTR"
			OTHERWISE
				cType := SELF:AdjustTypeName(oType)
			END CASE
		END IF
		cType := cType:Replace('+' , '.') // nested classes
		cType := cType:Replace("&" , "") // by reference
	RETURN cType

	PROTECTED METHOD AdjustTypeName(oType AS Type) AS STRING
		LOCAL cType AS STRING
		IF _showusingdiectives
			LOCAL cNameSpace AS STRING
			cNameSpace := oType:@@Namespace
			IF .not. String.IsNullOrEmpty(cNameSpace)
				IF .not. SELF:aNamespaces:ContainsKey(cNamespace:ToUpper())
					SELF:aNamespaces:Add(cNamespace:ToUpper() , cNamespace)
				END IF
			END IF
		END IF
		cType := oType:Name
//		cType := cType:Replace('+' , '.') // nested classes
		cType := AdjustKeyword(cType)
	RETURN cType

	STATIC PROTECT keywords AS Dictionary<STRING , STRING>
	PROTECTED STATIC METHOD AdjustKeyword(cWord AS STRING) AS STRING
		IF keywords:ContainsKey(cWord:ToUpper())
			cWord := "@@" + cWord
		END IF
	RETURN cWord
	STATIC CONSTRUCTOR()
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
		"TEXTBLOCK","ENDTEXT","OPERATOR","DECLARE","PROPERTY",;
		"_INIT1","_INIT2","_INIT3";
		} AS STRING[]

		keywords := Dictionary<STRING , STRING>{}
		
		LOCAL n AS INT
		FOR n := 1 UPTO aKeywords:Length
			keywords:Add(aKeywords[n] , aKeywords[n])
		NEXT
		
	RETURN 
	
END CLASS
