//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



USING XSharp.Internal
USING System.Reflection
USING System.Collections.Generic
USING System.Linq
USING System.Runtime.CompilerServices

INTERNAL STATIC CLASS OOPHelpers

	STATIC METHOD FindOurAssemblies AS IEnumerable<Assembly>
		RETURN	FROM asm IN AppDomain.CurrentDomain:GetAssemblies() ;
				WHERE asm:IsDefined(TYPEOF( ClassLibraryAttribute ), FALSE) ;
				SELECT asm

	STATIC METHOD FindClipperFunctions(cFunction AS STRING) AS MethodInfo[]
		VAR cla := TYPEOF( ClassLibraryAttribute )
		LOCAL aMethods AS List<MethodInfo>
		aMethods := List<MethodInfo>{}
		FOREACH asm AS Assembly IN FindOurAssemblies()
			LOCAL atr := (ClassLibraryAttribute) (asm:GetCustomAttributes(cla,FALSE) [1]) AS ClassLibraryAttribute
			LOCAL oType AS System.Type
			oType := asm:GetType(atr:GlobalClassName,FALSE, TRUE)
			IF oType != NULL_OBJECT
				LOCAL oMI AS MethodInfo
				LOCAL bf AS BindingFlags
				bf := BindingFlags.Static | BindingFlags.IgnoreCase | BindingFlags.Public | BindingFlags.DeclaredOnly 
				TRY 
					oMI := oType:GetMethod(cFunction,bf)
					IF oMI != NULL_OBJECT
						aMethods:Add( (MethodInfo) oMI)
					ENDIF
				CATCH AS AmbiguousMatchException
					LOCAL aMI AS MethodInfo[]
					aMI := oType:GetMethods(bf)
					FOREACH oM AS MethodInfo IN aMI
						IF String.Compare(oM:Name, cFunction, TRUE) == 0
							aMethods:Add( oM )
						ENDIF
					NEXT
				END TRY 
			ENDIF
		NEXT
		RETURN aMethods:ToArray()
		
	STATIC METHOD FindClass(cName AS STRING) AS System.Type 
	RETURN FindClass(cName, TRUE)
	STATIC METHOD FindClass(cName AS STRING, lOurAssembliesOnly AS LOGIC) AS System.Type 
		// TOdo Optimize
		LOCAL ret := NULL AS System.Type
		LOCAL aAssemblies AS IEnumerable<Assembly>

		IF lOurAssembliesOnly
			aAssemblies := FindOurAssemblies()
		ELSE
			aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
		END IF
		
		FOREACH asm AS Assembly IN aAssemblies
			ret := asm:GetType( cName, FALSE, TRUE )
			IF ret != NULL
				EXIT 
			ENDIF
			// The class could be prefixed with a Namespace. 
			// If there is an Implicoit Namespace Attribute
			// We don't know if the current assembly is compiler with /INS, but we assume it is when they
			// use the 'old fashioned' CreateInstance().
			VAR ins := TYPEOF( ImplicitNamespaceAttribute )
			IF asm:IsDefined(  ins, FALSE )
				VAR atr := (ImplicitNamespaceAttribute) (asm:GetCustomAttributes(ins,FALSE) [1])
				VAR cFullName := atr:Namespace +"."+cName
				ret := asm:GetType( cFullName, FALSE, TRUE )
				IF ret != NULL
					EXIT
				ENDIF  
			ENDIF
		NEXT   
		RETURN ret
		
	STATIC METHOD FindMethod(t AS System.Type, cName AS STRING ) AS MethodInfo
		LOCAL oMI := NULL AS MethodInfo
		
		TRY
			oMI := t:GetMethod(cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public ) 
		CATCH AS System.Reflection.AmbiguousMatchException
			oMI := NULL
		END TRY
		
		RETURN oMI
		
	STATIC METHOD IsMethod( t AS System.Type, cName AS STRING ) AS LOGIC
		RETURN FindMethod(t, cName) != NULL
		
	STATIC METHOD ClassTree( t AS Type ) AS ARRAY   
		LOCAL aList := {} AS ARRAY
		DO WHILE t != NULL
			AAdd( aList, (SYMBOL) t:Name)
			t := t:BaseType
		ENDDO
		
		RETURN aList
		
	STATIC METHOD IVarHelper(o AS OBJECT, cName AS STRING, lGet AS LOGIC) AS DWORD
	
		IF o == NULL
			RETURN 0
		ENDIF
		
		VAR t := o:GetType()
		
		VAR fi := t:GetField( cName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic  | BindingFlags.IgnoreCase)
		IF fi != NULL
			IF fi:IsPublic
				RETURN 2U
			ELSEIF fi:IsFamily 
				RETURN 1U
			ENDIF
		ENDIF
		
		DO WHILE t != NULL
			VAR pi := t:GetProperty( cName , BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
			IF pi != NULL .AND. ( (lGet .AND. pi:CanRead) .OR. (.NOT. lGet .AND. pi:CanWrite) )
				RETURN 3U
			ELSE
				t := t:BaseType
			ENDIF
		ENDDO
		
		RETURN 0U
		
	STATIC METHOD IVarList( t AS Type ) AS ARRAY
		IF t == NULL
			RETURN NULL_ARRAY
		ENDIF
		
		VAR aFields := t:GetFields( BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic )
		VAR nLen    := aFields:Length
		VAR list := List<STRING>{}
		FOREACH fi AS FieldInfo IN aFields
			IF fi:IsPublic || fi:IsFamily 
				VAR name := fi:Name:ToUpper()
				IF ! list:Contains(name)
					list:Add(name)
				ENDIF
			ENDIF
		NEXT
		
		VAR aProps := t:GetProperties( BindingFlags.Instance | BindingFlags.Public )
		
		FOREACH pi AS PropertyInfo IN aProps
			VAR name := pi:Name:ToUpper()
			IF ! list:Contains(name)
				list:Add(name)
			ENDIF
		NEXT
		RETURN list:ToVoSymArray()
		
		
	STATIC METHOD MethodList(t AS Type) AS ARRAY
		VAR list := List<STRING>{}
		VAR aInfo := t:GetMethods( BindingFlags.Instance | BindingFlags.Public )
		FOREACH oMI AS MethodInfo IN aInfo
			IF !oMI:IsSpecialName
				IF ! list:Contains(oMI:Name)
					list:Add(oMI:Name )
				ENDIF   
			ENDIF
		NEXT
		RETURN list:ToVoSymArray()
		
	STATIC METHOD ToVoSymArray(SELF list AS List<STRING>) AS ARRAY
		// convert List<STRING> to Array of Symbols
		LOCAL aResult AS ARRAY
		aResult := {}
		FOREACH VAR name IN list
			AAdd(aResult, String2Symbol(name))
		NEXT
		RETURN aResult
		
	STATIC METHOD TreeHelper( t AS Type ) AS ARRAY 
		LOCAL aList := NULL_ARRAY AS ARRAY
		IF t == NULL
			RETURN aList
		END IF
		
		VAR aInheritance := List<Type>{}
		DO WHILE t != NULL
			aInheritance:Add(t)
			t := t:BaseType
		END DO
		aList := {}
		FOREACH type AS Type IN aInheritance
			VAR listMethod := List<STRING>{}
			VAR listVar    := List<STRING>{}
			VAR aInfo := type:GetMembers(BindingFlags.Instance + BindingFlags.Public + BindingFlags.NonPublic)
			FOREACH oInfo AS MemberInfo IN aInfo
				VAR name := oInfo:Name:Toupper()
				DO CASE
					CASE oInfo:MemberType == MemberTypes.Field
						IF listVar:IndexOf(name)  == -1 .AND. ((FieldInfo)oInfo):IsPublic
							listVar:Add(name)
						END IF
					CASE oInfo:MemberType == MemberTypes.Property
						IF listVar:IndexOf(name)  == -1
							listVar:Add(name)
						END IF
					CASE oInfo:MemberType == MemberTypes.Method
						IF listMethod:IndexOf(name)  == -1 .AND. .NOT. ((MethodInfo)oInfo):IsSpecialName
							listMethod:Add(name)
						END IF
				END CASE
			NEXT
			VAR aInstance := listVar:ToVoSymArray()
			VAR aMethod   := listMethod:ToVoSymArray()
			AAdd(aList , {(SYMBOL) type:FullName, aInstance, aMethod})
			
		NEXT
		RETURN aList

	STATIC METHOD FindProperty( t AS Type , cName AS STRING, lAccess AS LOGIC) AS PropertyInfo
		DO WHILE t != NULL
			VAR oInfo := t:GetProperty( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public  | BindingFlags.DeclaredOnly ) 
			IF oInfo != NULL .AND. ( (lAccess .AND. oInfo:CanRead) .OR. (.NOT. lAccess .AND. oInfo:CanWrite) )
				RETURN oInfo
			ELSE
				t := t:BaseType
			ENDIF
		ENDDO
		RETURN NULL
		
	STATIC METHOD FindField( t AS Type, cName AS STRING, lAccess AS LOGIC ) AS FieldInfo
		DO WHILE t != NULL
			VAR oInfo := t:GetField( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public  | BindingFlags.DeclaredOnly ) 
			IF oInfo != NULL 
				// check for readonly (initonly) fields
				IF lAccess .OR. ! oInfo:Attributes:HasFlag(FieldAttributes.InitOnly)
					RETURN oInfo
				ENDIF
			ELSE
				t := t:BaseType
			ENDIF
		ENDDO
		RETURN NULL
		
	STATIC METHOD IsFieldVisible(oFld AS FieldInfo, lSelf AS LOGIC) AS LOGIC
		IF oFld == NULL_OBJECT
			RETURN FALSE
		ELSEIF oFld:IsPublic
			RETURN TRUE
		ELSEIF lSelf .AND. (oFld:IsFamily .OR. oFld:IsFamilyOrAssembly)
			RETURN TRUE
		ENDIF
		RETURN FALSE
		
		
	STATIC METHOD IVarGet(oObject AS OBJECT, cIVar AS STRING, lSelf AS LOGIC) AS OBJECT
		LOCAL t AS Type
		t := oObject:GetType()
		//Todo: optimization
		VAR fldInfo := FindField(t, cIVar,TRUE)
		IF fldInfo != NULL_OBJECT .AND. IsFieldVisible(fldInfo, lSelf)
			RETURN fldInfo:GetValue(oObject)
		ENDIF
		VAR propInfo := FindProperty(t, cIVar,TRUE)
		IF propInfo != NULL_OBJECT .AND. propInfo:CanRead
			RETURN propInfo:GetValue(oObject, NULL)
		ENDIF
		THROW Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{cIVar} )
		
	STATIC METHOD IVarPut(oObject AS OBJECT, cIVar AS STRING, oValue AS OBJECT, lSelf AS LOGIC)  AS VOID
		LOCAL t AS Type
		t := oObject:GetType()
		VAR fldInfo := FindField(t, cIVar,FALSE)
		//Todo: optimization
		IF fldInfo != NULL_OBJECT .AND. IsFieldVisible(fldInfo, lSelf)
			oValue := MyConvert(oValue, fldInfo:FieldType)
			fldInfo:SetValue(oObject, oValue)
			RETURN
		ENDIF
		LOCAL propInfo AS PropertyInfo
		propInfo := FindProperty(t, cIVar, lSelf)
		IF propInfo != NULL_OBJECT .AND. propInfo:CanWrite
			oValue := MyConvert(oValue, propInfo:PropertyType)
			propInfo:SetValue(oObject,oValue , NULL)
			RETURN
		ENDIF
		THROW Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{cIVar})
		
	STATIC METHOD SendHelper(oObject AS OBJECT, cMethod AS STRING, uArgs AS USUAL[], result OUT USUAL) AS LOGIC
		LOCAL t := oObject?:GetType() AS Type
		result := NIL
		IF t == NULL
			THROW Error.NullArgumentError( __ENTITY__, NAMEOF(oObject), 1 )
		ENDIF
		LOCAL mi AS MethodInfo
		mi := t:GetMethod(cMethod,BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
		IF mi == NULL
			// No Error Here. THat is done in the calling code
			RETURN FALSE
		ENDIF	
		RETURN sendHelper(oObject, mi, uArgs, OUT result)
		
	STATIC METHOD SendHelper(oObject AS OBJECT, mi AS MethodInfo , uArgs AS USUAL[], result OUT USUAL) AS LOGIC
		LOCAL paramInfo AS ParameterInfo[]
		result := NIL
		paramInfo := mi:GetParameters()
		// Clipper calling convention ?
		LOCAL lClipper := FALSE AS LOGIC
		IF paramInfo:Length == 1 .AND. mi:IsDefined( TYPEOF( ClipperCallingConventionAttribute ), FALSE )
			lClipper := TRUE
		ENDIF
		LOCAL oArgs AS OBJECT[]
		IF lClipper
			oArgs := <OBJECT> {uArgs}
		ELSEIF paramInfo:Length > 0
			oArgs := OBJECT[]{ paramInfo:Length }
			LOCAL nPar AS INT
			FOR nPar := 1 TO paramInfo:Length
				LOCAL pi := paramInfo[nPar] AS ParameterInfo
				IF nPar <= uArgs:Length
					LOCAL arg := uArgs[nPar] AS USUAL
					IF pi:ParameterType == TYPEOF(USUAL)
						// We need to box a usual here 
						oArgs[nPar] := Myconvert(arg, TYPEOF(__USUAL))
					ELSEIF pi:ParameterType:IsAssignableFrom(arg:SystemType) .OR. arg == NULL
						oArgs[nPar] := uArgs[nPar]
					ELSEIF pi:GetCustomAttributes( TYPEOF( ParamArrayAttribute ), FALSE ):Length > 0
						// Parameter array of certain type
						// -> convert remaining elements from uArgs to an array and assign that to oArgs[i] 
						LOCAL elementType := pi:ParameterType:GetElementType() AS System.Type
						LOCAL aVarArgs := System.Array.CreateInstance(elementType, uArgs:Length - nPar +1) AS System.Array
						LOCAL nArg AS INT
						FOR nArg := nPar TO uArgs:Length
							TRY
								IF elementType:IsAssignableFrom(uArgs[nArg]:SystemType)
									aVarArgs:SetValue(uArgs[nArg], nArg-nPar)
								ELSE
									aVarArgs:SetValue(MyConvert(uArgs[nArg], elementType), nArg-nPar)
								ENDIF
							CATCH
								aVarArgs:SetValue(NULL, nArg-nPar)
							END TRY
						NEXT
						oArgs[nPar] := aVarArgs
						EXIT	// parameter loop
					ELSE	// try to convert to the expected type
						oArgs[nPar]  := MyConvert(uArgs[nPar], pi:ParameterType)
					ENDIF
				ENDIF 
			NEXT
		ELSE
			oArgs := NULL
		ENDIF
		IF mi != NULL
			result := mi:Invoke(oObject, oArgs)
		ENDIF
		RETURN TRUE
		
	STATIC METHOD MyConvert(uValue AS USUAL,toType AS System.type) AS OBJECT
		IF toType == TYPEOF(FLOAT)
			RETURN (FLOAT) uValue
		ELSE
			IF toType == TYPEOF(USUAL)
				// todo: better mechanism for boxing
				// box the usual
				VAR oTemp := <OBJECT> { uValue }
				RETURN Activator.CreateInstance(TYPEOF(XSharp.__Usual), oTemp)
			ENDIF
			VAR o := (OBJECT) uValue 
			IF toType:IsAssignableFrom(o:GetType())
				RETURN o
			ENDIF
			RETURN Convert.ChangeType(o, toType)
		ENDIF
		
	STATIC METHOD DoSend(oObject AS OBJECT, cMethod AS STRING, args AS USUAL[] ) AS USUAL
		LOCAL result AS USUAL
		IF ! SendHelper(oObject, cMethod, args, OUT result)
			LOCAL nomethodArgs := USUAL[]{ args:Length + 1 } AS USUAL[]
			cMethod := cMethod:ToUpperInvariant()
			RuntimeState.NoMethod := cMethod   // For NoMethod() function
			noMethodArgs[__ARRAYBASE__] := cMethod
			Array.Copy( args, 0, noMethodArgs, 1, args:Length )
			IF ! SendHelper(oObject, "NoMethod" , noMethodArgs, OUT result)
				THROW Error.VOError( EG_NOMETHOD, "Send", NAMEOF(cMethod), 2, <OBJECT>{cMethod} )
			ENDIF
		ENDIF
		RETURN result
		
END CLASS
		
		
/// <summary>
/// Invoke a method for every element in an Array of objects.
/// </summary>
/// <param name="aTarget">The array to process.</param>
/// <param name="symMethod">The method name, specified without parentheses.</param>
/// <param name="args">A comma-separated list of arguments to pass to symMethod.</param>
/// <returns>A reference to aTarget.</returns>
FUNCTION ASend(aTarget AS ARRAY, cName AS STRING, args PARAMS USUAL[] ) AS ARRAY 
	IF aTarget != NULL .AND. ! String.IsNullOrEmpty( cName )
		FOREACH VAR x IN aTarget
			__InternalSend( x, cName, args )
		NEXT
	ENDIF   
	RETURN aTarget
	
	
/// <summary>
/// Determine if an object is an instance of a particular class.
/// </summary>
/// <param name="oObject">The object to check for.</param>
/// <param name="symClassName">The class that oObject may be an instance of.</param>
/// <returns>TRUE if oObject is an instance of symClassName; otherwise, an error is generated.  FALSE is returned if you choose to ignore the error.
/// If oObject is a NULL_OBJECT, a FALSE is returned but no error is generated.
/// </returns>
FUNCTION CheckInstanceOf(oObject AS OBJECT,symClassName AS STRING) AS LOGIC
	IF oObject == NULL_OBJECT
		RETURN FALSE
	ELSEIF IsInstanceOf(oObject, symClassName)
		RETURN TRUE
	ENDIF
	LOCAL oError := Error.VOError(EG_WRONGCLASS, "CHECKINSTANCEOF", NAMEOF(oObject),1, NULL) AS Error
	oError:Description := symClassName + " <-> " + oObject:GetType():Name
	THROW oError
	
	
/// <summary>
/// Return the number of classes available to your application.
/// </summary>
/// <returns>
/// The number of available classes.
/// </returns>
FUNCTION ClassCount() AS DWORD
	RETURN ClassList():Length
	
/// <summary>
/// Return an Array of Symbols corresponding to the classes available to your application.
/// </summary>
/// <returns>
/// Returns an Array with the name of all available classes
/// </returns>
FUNCTION ClassList() AS ARRAY
	LOCAL classes    := ARRAY{} AS ARRAY
	LOCAL assemblies := System.AppDomain.CurrentDomain:GetAssemblies() AS System.Reflection.Assembly[]
	FOREACH assembly AS System.Reflection.Assembly IN assemblies
		LOCAL types := assembly:GetTypes() AS System.Type[]
		FOREACH type AS System.Type IN types
			IF type:IsPublic
				classes:Add(String2Symbol(type:Name))
			ENDIF
		NEXT
	NEXT
	RETURN classes
	
/// <summary>
/// Get the class name of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION ClassName(o AS OBJECT) AS STRING
	RETURN o?:GetType():Name:ToUpper()
	
	
	
/// <summary>
/// Get the class hierarchy of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION ClassTree(o AS OBJECT) AS ARRAY
	RETURN OOPHelpers.ClassTree(o?:GetType())
	
	
FUNCTION CreateInstance(cClassName) AS OBJECT CLIPPER
	IF ! ( cClassName:IsSymbol || cClassName:IsString )
		THROW Error.DataTypeError( "CreateInstance", NAMEOF(cClassName), 1, cClassName)
	ENDIF    	
	VAR t := OOPHelpers.FindClass(cClassName)
	IF t == NULL
		 THROW Error.VOError( EG_NOCLASS, "CreateInstance", NAMEOF(cClassName), 1,  cClassName  )
	ENDIF
	VAR constructors := t:getConstructors() 
	IF constructors:Length > 1
		THROW Error.VOError( EG_AMBIGUOUSMETHOD, "CreateInstance", NAMEOF(cClassName), 0 , NULL)
	ENDIF
	LOCAL ctor := constructors[1] AS ConstructorInfo
	LOCAL nPCount AS INT
	LOCAL nArg AS INT
	nPCount := PCount()
	LOCAL oRet AS OBJECT  
	TRY
		LOCAL oArgs AS OBJECT[]
		IF ctor:IsDefined(TYPEOF(ClipperCallingconventionAttribute),FALSE)
			LOCAL args AS USUAL[]
			args := USUAL[]{nPCount-1}
			FOR nArg := 2 TO nPCount
				args[nArg-1] := _GetFParam(nArg)
			NEXT 
			oArgs  := <OBJECT>{args}
		ELSEIF ctor:IsDefined(TYPEOF( CompilerGeneratedAttribute ), FALSE )
			// generated default ctor without args
			oArgs := NULL
		ELSEIF ctor:GetParameters():Length == 0
			oArgs := NULL
		ELSE
			// convert args to array of objects
			LOCAL args AS OBJECT[]
			args := OBJECT[]{nPCount-1}
			FOR nArg := 2 TO nPCount
				args[nArg-1] := _GetFParam(nArg)
			NEXT 
			oArgs  := <OBJECT>{args}
		ENDIF
		oRet := ctor:Invoke( oArgs)	
	CATCH
		THROW Error.VOError( EG_NOMETHOD, "CreateInstance", "Constructor", 0 , NULL)
		oRet := NULL_OBJECT
	END TRY
	RETURN oRet
	
	
	
	
/// <summary>
/// Get the class hierarchy of a class.
/// </summary>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
FUNCTION ClassTreeClass(cName AS STRING) AS ARRAY
	VAR t := OOPHelpers.FindClass(cName)
	IF t != NULL
		RETURN OOPHelpers.ClassTree(t)
	ELSE
		THROW Error{EG_NOCLASS,0}
	ENDIF
	RETURN NULL_ARRAY   
	
	
	
/// <summary>
/// Check whether a particular access method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION IsAccess(o AS OBJECT,cName AS STRING) AS LOGIC
	VAR oprop := OOPHelpers.FindProperty(o?:GetType(), cName, FALSE)
	IF oProp != NULL_OBJECT
		RETURN oProp:CanRead
	ENDIF
	RETURN FALSE

/// <summary>
/// Check whether a particular assign method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAssignName"></param>
/// <returns>
/// </returns>
FUNCTION IsAssign(o AS OBJECT,cName AS STRING) AS LOGIC
	VAR oprop := OOPHelpers.FindProperty(o?:GetType(), cName, FALSE)
	IF oProp != NULL_OBJECT
		RETURN oProp:CanWrite
	ENDIF
	RETURN FALSE
	
/// <summary>
/// Determine if a class exists.
/// </summary>
/// <param name="cClassName"></param>
/// <returns>
/// </returns>
FUNCTION IsClass(cClassName AS STRING) AS LOGIC

	RETURN OOPHelpers.FindClass(cClassName) != NULL
	
/// <summary>
/// Determine if one class is a subclass of another class.
/// </summary>
/// <param name="symClassName"></param>
/// <param name="symSuperClassName"></param>
/// <returns>
/// </returns>
FUNCTION IsClassOf(cClassName AS STRING,cSuperClassName AS STRING) AS LOGIC
	LOCAL tSub   := OOPHelpers.FindClass(cClassName) AS Type
	LOCAL tSuper := OOPHelpers.FindClass(cSuperClassName) AS Type
	// IsClassOf() in VO returns TRUE when child and parent class is the same (and it exists)
	RETURN tSub != NULL .AND. tSuper != NULL .AND. (tSub == tSuper .OR. tSub:IsSubclassOf(tSuper))
	
	
	
/// <summary>
/// Determine if an object is an instance of a class.
/// </summary>
/// <param name="oObject"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION IsInstanceOf(oObject AS OBJECT,cName AS STRING) AS LOGIC
	IF oObject == NULL_OBJECT
		RETURN FALSE
	ENDIF
	LOCAL oType := OOPHelpers.FindClass(cName, FALSE) AS System.Type
	IF oType == NULL
		RETURN FALSE
	END IF
	RETURN oType:IsAssignableFrom(oObject:GetType())
	
	
	
/// <summary>
/// Determine if an object inside a Usual is an instance of a class.
/// </summary>
/// <param name="oX"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION IsInstanceOfUsual(oX AS USUAL,cName AS STRING) AS LOGIC
	IF ! oX:IsObject
		RETURN FALSE
	ENDIF
	RETURN IsInstanceOf(oX, cName)
	
	
	
/// <summary>
/// Return the contents of an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIvar"></param>
/// <returns>
/// </returns>
FUNCTION IVarGet(o AS OBJECT,cIvar AS STRING) AS USUAL
	IF o == NULL_OBJECT
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(cIVar),2)
	ENDIF
	RETURN OOPHelpers.IVarGet(o, cIVar, FALSE)
	
/// <summary>
/// Get information about how a particular instance variable (or access method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
FUNCTION IVarGetInfo(o AS OBJECT,cIVar AS STRING) AS DWORD
	RETURN OOPHelpers.IVarHelper(o, cIVar, TRUE)
	
	
/// <summary>
/// Check whether a particular method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="cMethodName"></param>
/// <returns>
/// </returns>
FUNCTION IsMethod(o AS OBJECT,cMethodName AS STRING) AS LOGIC
	RETURN OOPHelpers.IsMethod(o?:GetType(), cMethodName)
	
	
/// <summary>
/// Check whether a particular method can be sent to an object inside a Usual.
/// </summary>
/// <param name="oX"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
FUNCTION IsMethodUsual(oX AS USUAL,cName AS STRING) AS LOGIC
	IF oX:IsObject
		RETURN IsMethod( oX, cName )
	ENDIF
	RETURN FALSE
	
/// <summary>
/// Check whether a particular method can be sent to a class.
/// </summary>
/// <param name="c"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION IsMethodClass( c AS STRING, cName AS STRING ) AS LOGIC
	VAR t := OOPHelpers.FindClass( c )
	
	IF t != NULL
		RETURN OOPHelpers.IsMethod( t, cName )
	ENDIF
	RETURN FALSE
	

/// <summary>
/// Return the contents of an instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
FUNCTION IVarGetSelf(o AS OBJECT,cIVar AS STRING) AS USUAL
	IF o == NULL_OBJECT
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(cIVar),2)
	ENDIF
	RETURN OOPHelpers.IVarGet(o, cIVar, TRUE)
	
/// <summary>
/// Store all instance variables of an object into an Array.
/// </summary>
/// <param name="oObject"></param>
/// <returns>
/// </returns>
FUNCTION IvarList(oObject AS OBJECT) AS ARRAY
	RETURN OOPHelpers.IVarList(oObject?:GetType())
	
	
/// <summary>
/// Store all instance variables of a class into an Array.
/// </summary>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
FUNCTION IvarListClass(cName AS STRING) AS ARRAY
	VAR t := OOPHelpers.FindClass(cName)
	RETURN OOPHelpers.IVarList(t)
	
	
/// <summary>
/// Get information about how a particular instance variable (or assign method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
FUNCTION IVarPutInfo(o AS OBJECT,cIVar AS SYMBOL) AS DWORD
	RETURN OOPHelpers.IVarHelper(o, cIVar, FALSE)
	
/// <summary>
/// Assign a value to an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
FUNCTION IVarPut(o AS OBJECT,cIVar AS STRING,uValue AS USUAL) AS USUAL
	IF o == NULL_OBJECT
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(cIVar),2)
	ENDIF
	OOPHelpers.IVarPut(o, cIVar, uValue, FALSE)
	RETURN uValue
	
/// <summary>
/// Assign a value to an instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIvar"></param>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
FUNCTION IVarPutSelf(o AS OBJECT,cIVar AS STRING,uValue AS USUAL) AS USUAL
	IF o == NULL_OBJECT
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(cIVar),2)
	ENDIF
	OOPHelpers.IVarPut(o, cIVar, uValue,TRUE) 
	RETURN uValue
	
	
/// <summary>
/// Create a class list in the form of an Array for the specified object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION MethodList(o AS OBJECT) AS ARRAY
	IF o != NULL
		RETURN OOPHelpers.MethodList( o:GetType() )
	ENDIF
	RETURN NULL_ARRAY
	
/// <summary>
/// Create a class list in the form of an Array for the specified class.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION MethodListClass( c AS STRING ) AS ARRAY
	LOCAL aReturn AS ARRAY
	VAR t := OOPHelpers.FindClass( c )
	IF t != NULL
		aReturn := OOpHelpers.MethodList( t )
	ELSE
		aReturn  := NULL_ARRAY
	ENDIF
	
	RETURN aReturn
	
	
	
/// <summary>
/// Identify an the last method call 
/// </summary>
/// <returns>
/// </returns>
FUNCTION NoMethod() AS STRING
	RETURN RuntimeState.NoMethod
	
	
/// <summary>
/// Convert the values of an object's instance variables to an Array.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION Object2Array(o AS OBJECT) AS ARRAY
	LOCAL t AS System.Type
	IF o == NULL_OBJECT
		RETURN NULL_ARRAY
	ENDIF
	LOCAL aProps AS PropertyInfo[]
	LOCAL aFields AS FieldInfo[]
	LOCAL aResult AS ARRAY
	aResult := {}
	t := o:GetType()
	aProps := t:GetProperties(BindingFlags.Instance | BindingFlags.Public)
	FOREACH p AS PropertyInfo IN aProps
		LOCAL uVal AS USUAL
		IF p:CanRead 
			uVal := p:GetValue(o,NULL)
			AAdd(aResult, uVal)
		ENDIF
	NEXT
	aFields := t:GetFields(BindingFlags.Instance | BindingFlags.Public)
	FOREACH f AS FieldInfo IN aFields
		LOCAL uVal AS USUAL
		IF ! f:IsSpecialName
			uVal := f:GetValue(o)
			AAdd(aResult, uVal)
		ENDIF
	NEXT

	RETURN aResult
	
	
	
/// <summary>
/// Return a multidimensional Array of all object-oriented programming Symbols that constitute the class.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION OOPTree(o AS OBJECT) AS ARRAY
	RETURN OOPHelpers.TreeHelper(o?:GetType())
	
/// <summary>
/// Return a multidimensional Array of all object-oriented programming Symbols that constitute the class of an object.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
FUNCTION OOPTreeClass(cClass AS STRING) AS ARRAY
	VAR type := OOPHelpers.FindClass(cClass)
	RETURN OOPHelpers.TreeHelper(type)
	
	
/// <summary>Invoke a method.</summary>
/// <param name="o"></param>
/// <param name="uMethod "></param>
/// <returns>Return value of the method call. </returns>
FUNCTION Send(o AS USUAL,uMethod AS USUAL, args PARAMS USUAL[]) AS USUAL 
	IF !o:IsObject
	     THROW Error.VOError( EG_DATATYPE, "Send", NAMEOF(o), 1, <OBJECT>{ o}  )
	ENDIF
	IF ! uMethod:IsString  && ! uMethod:IsSymbol
		THROW Error.VOError( EG_DATATYPE, "Send", NAMEOF(uMethod) , 2, <OBJECT>{ uMethod } )
	ENDIF
	LOCAL oObject := o AS OBJECT
	LOCAL cMethod := uMethod AS STRING
	LOCAL uResult AS USUAL
	TRY
		uResult := OopHelpers.DoSend(oObject, cMethod, args)
	CATCH
		uResult := NIL
	END TRY
	RETURN uResult
	
/// <summary>Invoke a method.</summary>
/// <param name="o"></param>
/// <param name="symMethod"></param>
/// <param name="args">A comma-separated list of arguments to pass to symMethod.</param>
/// <returns>Return value of the method call. </returns>
FUNCTION CSend(o AS OBJECT,symMethod AS STRING, args PARAMS USUAL[]) AS USUAL
	RETURN __InternalSend(o, symMethod, args)
	
	// This is called by the compiler when a late bound call is made on a USUAL.
	// It is strongly typed and more efficient than Send(), which must use the
	// CLIPPER calling convention for compatiblity with VO.
	// Note: Make The first parameter in __InternalSend() in the runtime must be a USUAL!
	//       The compiler expects that
FUNCTION __InternalSend( oObject AS USUAL, cMethod AS STRING, args PARAMS USUAL[] ) AS USUAL
	RETURN OopHelpers.DoSend(oObject, cMethod, args)

/// <summary>Helper function to convert ARRAY to USUAL[]</summary>	
INTERNAL FUNCTION __ArrayToUsualArray (args AS ARRAY) AS USUAL[]
	LOCAL elements AS INT
	LOCAL uargs    AS USUAL[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	uargs    := USUAL[]{ elements }
	
	FOR x := 1 UPTO elements
		uargs[x] := args[x]
	NEXT
	RETURN uargs

/// <summary>Helper function to convert ARRAY to OBJECT[]</summary>		
INTERNAL FUNCTION __ArrayToObjectArray (args AS ARRAY) AS OBJECT[]
	LOCAL elements AS INT
	LOCAL oArgs    AS OBJECT[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	oArgs    := OBJECT[]{ elements }
	
	FOR x := 1 UPTO elements
		oArgs[x] := args[x]
	NEXT
	RETURN oArgs

/// <summary>Helper function to convert USUAL[] to OBJECT[]</summary>			
INTERNAL FUNCTION __UsualArrayToObjectArray (args AS USUAL[]) AS OBJECT[]
	LOCAL elements AS INT
	LOCAL oArgs    AS OBJECT[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	oArgs    := OBJECT[]{ elements }
	
	FOR x := 1 UPTO elements
		oArgs[x] := args[x]
	NEXT
	RETURN oArgs

/// <summary>Helper function to convert OBJECT[] to USUAL[]</summary>		
INTERNAL FUNCTION __ObjectArrayToUsualArray (args AS OBJECT[]) AS USUAL[]
	LOCAL elements AS INT
	LOCAL uArgs    AS USUAL[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	uArgs    := USUAL[]{ elements }
	
	FOR x := 1 UPTO elements
		uArgs[x] := args[x]
	NEXT
	RETURN uArgs
	
	
	// identical to CSend and __InternalSend but with a normal array of args
FUNCTION _SendClassParams( oObject AS OBJECT, cmethod AS STRING, args AS ARRAY ) AS USUAL
	LOCAL uArgs AS USUAL[]
	uArgs := __ArrayToUsualArray(args)
	RETURN OopHelpers.DoSend(oObject, cMethod, uArgs )
	
	
/// <summary>Return the number of arguments that a method is expecting.</summary>
/// <param name="cClass">The symbol of the class containing the method to examine.</param>
/// <param name="cMethod">The method name, specified without parentheses.</param>
/// <returns>The number of arguments that a method is expecting.</returns>
FUNCTION MParamCount(cClass AS STRING,cMethod AS STRING) AS DWORD
	LOCAL type AS Type
	type := OOPHelpers.FindClass(cClass)	
	IF type != NULL
		LOCAL met AS MethodInfo
		met := OOPHelpers.FindMethod(type, cMethod)
		IF met != NULL
			IF met:IsDefined(TYPEOF(ClipperCallingconventionAttribute),FALSE)
				// calculate the # of parameters
				LOCAL oAttr AS ClipperCallingConventionAttribute
				oAttr := (ClipperCallingconventionAttribute) met:GetCustomAttributes(TYPEOF(ClipperCallingconventionAttribute), FALSE)[1]
				RETURN (DWORD) oAttr:ParameterNames:Length
			ELSE
				RETURN (DWORD) met:GetParameters():Length
			ENDIF
		ELSE
			THROW Error.VOError( EG_NOMETHOD,  "MParamCount", NAMEOF(cMethod), 2, <OBJECT>{cMethod} )
		ENDIF
	ELSE
		THROW Error.VOError( EG_WRONGCLASS,  "MParamCount", NAMEOF(cClass), 1, <OBJECT>{cClass} )
	ENDIF
	RETURN 0   
	
	
	
/// <summary>Return the number of local arguments that a function is expecting.</summary>
/// <param name="symFunction">The name of the function to examine.</param>
/// <returns>The number of arguments that a method is expecting.</returns>
/// <remarks>Note that you can't call functions that are overloaded.<br/>
/// And unlike in VO this function can also be used to return the number of parameters for typed functions.</remarks>

FUNCTION FParamCount(symFunction AS STRING) AS DWORD
	LOCAL aFuncs AS MethodInfo[]
	aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
	// CLipper functions can't and shouldn't have overloads
	IF aFuncs != NULL 
		IF aFuncs:Length == 1 
			LOCAL oMI := aFuncs[1] AS MethodInfo
			IF oMI:IsDefined(TYPEOF(ClipperCallingconventionAttribute),FALSE)
				// calculate the # of parameters
				LOCAL oAttr AS ClipperCallingConventionAttribute
				oAttr := (ClipperCallingconventionAttribute) oMI:GetCustomAttributes(TYPEOF(ClipperCallingconventionAttribute), FALSE)[1]
				RETURN (DWORD) oAttr:ParameterNames:Length
			ELSE
				RETURN (DWORD) oMI:GetParameters():Length
			ENDIF
		ELSE
			THROW Error.VOError( EG_AMBIGUOUSMETHOD,  "FParamCount", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
		ENDIF
	ELSE
		THROW Error.VOError( EG_NOFUNC,  "FParamCount", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
	ENDIF
	RETURN 0   
	
/// <summary>Call a clipper function by name</summary>
/// <param name="symFunction">The name of the function to call.</param>
/// <param name="aArgs">The list of arguments to pass to the function</param>
/// <returns>The return value of the function</returns>
/// <remarks>Note that you can't call functions that are overloaded.</remarks>
FUNCTION _CallClipFunc(symFunction AS STRING,aArgs AS ARRAY) AS USUAL
	RETURN	_CallClipFunc(symFunction, __ArrayToUsualArray(aArgs))

/// <summary>Call a function by name</summary>
/// <param name="symFunction">The name of the function to call.</param>
/// <param name="uArgs">The list of arguments to pass to the function</param>
/// <returns>The return value of the function</returns>
/// <remarks>Note that you can't call functions that are overloaded.</remarks>
FUNCTION _CallClipFunc(symFunction AS STRING,uArgs PARAMS USUAL[]) AS USUAL
	LOCAL aFuncs AS MethodInfo[]

	aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
	// CLipper functions can't and shouldn't have overloads
	IF aFuncs != NULL 
		IF aFuncs:Length == 1 
			LOCAL oMI AS MethodInfo
			LOCAL result AS USUAL
			oMI		:= aFuncs[1] 
			IF OOPHelpers.SendHelper(NULL, oMI, uArgs, OUT result)
				RETURN result
			ENDIF
		ELSE
			THROW Error.VOError( EG_AMBIGUOUSMETHOD,  "_CallClipFunc", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
		ENDIF
	ELSE
		THROW Error.VOError( EG_NOFUNC,  "FParamCount", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
	ENDIF

	RETURN  NIL   



