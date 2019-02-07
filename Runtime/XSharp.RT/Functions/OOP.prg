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
    STATIC INTERNAL EnableOptimizations AS LOGIC
    STATIC INTERNAL cacheClassesAll AS Dictionary<STRING,Type>
    STATIC INTERNAL cacheClassesOurAssemblies AS Dictionary<STRING,Type>

    STATIC CONSTRUCTOR()
	    cacheClassesAll := Dictionary<STRING,Type>{Stringcomparer.OrdinalIgnoreCase}
	    cacheClassesOurAssemblies := Dictionary<STRING,Type>{Stringcomparer.OrdinalIgnoreCase}
	RETURN
    
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
		
		IF String.IsNullOrWhiteSpace(cName)
			// otherwise asm:GetType() will throw an exception with empty name
			RETURN ret
		END IF

		IF lOurAssembliesOnly
			IF cacheClassesOurAssemblies:ContainsKey(cName)
				RETURN cacheClassesOurAssemblies[cName]
			END IF
			aAssemblies := FindOurAssemblies()
		ELSE
			IF cacheClassesAll:ContainsKey(cName)
				RETURN cacheClassesAll[cName]
			END IF
			aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
		END IF
		
		FOREACH asm AS Assembly IN aAssemblies
			ret := asm:GetType( cName, FALSE, TRUE )
			IF ret != NULL
				EXIT 
			ENDIF
			// The class could be prefixed with a Namespace. 
			// If there is a class library attribute and we prefixed all classes with a namespace then
            // this is visible in the ClassLibraryAttribute
			// We don't know if the current assembly is compiler with /INS, but we assume it is when they
			// use the 'old fashioned' CreateInstance().
			VAR ins := TYPEOF( ClassLibraryAttribute )
			IF asm:IsDefined(  ins, FALSE )
                // there should be only one but it does not hurt to be cautious
                FOREACH VAR attribute IN asm:GetCustomAttributes(ins,FALSE)
				    VAR atr := (ClassLibraryAttribute) attribute
                    IF !String.IsNullOrEmpty(atr:DefaultNamespace)
				        VAR cFullName := atr:DefaultNamespace +"."+cName
				        ret := asm:GetType( cFullName, FALSE, TRUE )
				        IF ret != NULL
					        EXIT
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
            IF ret != NULL
            	EXIT
            END IF
            // If there is an Implicit Namespace Attribute
            ins := TYPEOF( ImplicitNamespaceAttribute )
			IF asm:IsDefined(  ins, FALSE )
                FOREACH VAR attribute IN asm:GetCustomAttributes(ins,FALSE)
				    VAR atr := (ImplicitNamespaceAttribute) attribute
                    IF !String.IsNullOrEmpty(atr:Namespace)
				        VAR cFullName := atr:Namespace+"."+cName
				        ret := asm:GetType( cFullName, FALSE, TRUE )
				        IF ret != NULL
					        EXIT
                        ENDIF
                    ENDIF
                 NEXT
             ENDIF
             IF ret != NULL
             	EXIT
             ENDIF
		NEXT
		
		IF ret != NULL
			IF lOurAssembliesOnly
				IF .NOT. cacheClassesOurAssemblies:ContainsKey(cName)
					cacheClassesOurAssemblies:Add(cName , ret)
				END IF
			ELSE
				IF .NOT. cacheClassesAll:ContainsKey(cName)
					cacheClassesAll:Add(cName , ret)
				END IF
			END IF
		END IF
		
		RETURN ret
		
	STATIC METHOD FindMethod(t AS System.Type, cName AS STRING, lSelf AS LOGIC ) AS MethodInfo
		LOCAL oMI := NULL AS MethodInfo
		
		IF t == NULL .OR. String.IsNullOrEmpty(cName)
			RETURN NULL
		END IF
		
		TRY
			oMI := t:GetMethod(cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public | IIF(lSelf, BindingFlags.NonPublic, BindingFlags.Public) ) 
		CATCH AS System.Reflection.AmbiguousMatchException
			oMI := NULL
		END TRY
		
		RETURN oMI


    STATIC METHOD CompareMethods(m1 as MethodBase, m2 as MethodBase, uArgs as USUAL[]) as LONG
        var p1 := m1:GetParameters()
        var p2 := m2:GetParameters()

        if p1:Length != p2:Length
            if p1:Length == uArgs:Length
                return 1
            elseif p2:Length == uArgs:Length
                return 2
            endif
        endif
        // when we get here then the parameter counts are the same
        FOR VAR nPar := 1 to p1:Length
            if nPar > uArgs:Length
                exit
            endif
            var par1 := p1[nPar]
            var par2 := p2[nPar]
            var arg  := uArgs[nPar]
            if par1:ParameterType != par2:ParameterType
                if par1:ParameterType:IsAssignableFrom(arg:SystemType)
                    return 1
                endif
                if par2:ParameterType:IsAssignableFrom(arg:SystemType)
                    return 2
                endif
                if par1:ParameterType = typeof(USUAL)
                    return 1
                endif
                if par2:ParameterType = typeof(USUAL)
                    return 2
                endif
            endif
        NEXT
        return 0

    STATIC METHOD FindBestOverLoad<T>(overloads as T[], cFunction as STRING, uArgs AS USUAL[]) as T where T is MethodBase
        if overloads:Length == 0
            return null
        elseif overloads:Length = 1
            return overloads[1]
        endif
        // More than one
        var found := List<T>{}
        // first look for methods with the same ! of parametes
        foreach var m in overloads
            if m:GetParameters():Length == uArgs:Length
                found:Add(m)
            endif
        next
        if found:Count == 1
            return found[0] // collection, so 0 based !
        endif
        // then look for methods with
        found:Clear()
        for var nMethod := 1 to overloads:Length -1
            var m1     := overloads[nMethod]
            var m2     := overloads[nMethod+1]
            var result := compareMethods(m1, m2, uArgs)
            if result == 1
                if ! found:Contains(m1)
                    found:Add(m1)
                endif
            elseif result == 2
                if ! found:Contains(m2)
                    found:Add(m2)
                endif
            endif
        next
        if found:Count == 1
            return found[0] // collection, so 0 based !
        endif
        local cClass as STRING
        cClass := overloads[1]:DeclaringType:Name
        THROW Error.VOError( EG_AMBIGUOUSMETHOD, cFunction, "MethodName", 1, <OBJECT>{cClass+":"+overloads[1]:Name})
        

    STATIC METHOD MatchParameters<T>( methodinfo as T, args as USUAL[]) AS OBJECT[] where T is MethodBase
        // args contains the list of arguments. The methodname has already been deleted when appropriated
		LOCAL oArgs AS OBJECT[]
        LOCAL lClipper := FALSE AS LOGIC
        VAR aPars := methodInfo:GetParameters()
        if aPars:Length == 1 .and. methodinfo:IsDefined(TYPEOF(ClipperCallingconventionAttribute),FALSE)
            lClipper := TRUE
        ENDIF
        DO CASE
        CASE lClipper
			oArgs  := <OBJECT>{args}
        CASE aPars:Length == 0 
			// no args
			oArgs := NULL
		OTHERWISE
			// convert args to array of objects
			oArgs := OBJECT[]{aPars:Length}
            VAR nMax := args:Length
            IF aPars:Length < nMax
                nMax := aPars:Length 
            ENDIF
			FOR VAR nPar := 1 TO nMax
                VAR     pi := aPars[nPar] // ParameterInfo
                LOCAL   arg := args[nPar] AS USUAL
                IF pi:ParameterType == TYPEOF(USUAL)
					// We need to box a usual here 
    				oArgs[nPar] := __CASTCLASS(OBJECT, arg)
                ELSEIF pi:ParameterType:IsAssignableFrom(arg:SystemType) .OR. arg == NULL
					oArgs[nPar] := arg
                ELSEIF pi:GetCustomAttributes( TYPEOF( ParamArrayAttribute ), FALSE ):Length > 0
                    // Parameter array of certain type
					// -> convert remaining elements from uArgs to an array and assign that to oArgs[i] 
					LOCAL elementType := pi:ParameterType:GetElementType() AS System.Type
					LOCAL aVarArgs    := System.Array.CreateInstance(elementType, args:Length - nPar +1) AS System.Array
					FOR VAR nArg := nPar TO args:Length
						TRY
							IF elementType:IsAssignableFrom(args[nArg]:SystemType)
								aVarArgs:SetValue(args[nArg], nArg-nPar)
							ELSE
								aVarArgs:SetValue(VOConvert(args[nArg], elementType), nArg-nPar)
							ENDIF
						CATCH
							aVarArgs:SetValue(NULL, nArg-nPar)
						END TRY
					NEXT
					oArgs[nPar] := aVarArgs
                    EXIT    // done with parameters
                ELSE	// try to convert to the expected type
					oArgs[nPar]  := VOConvert(args[nPar], pi:ParameterType)
                ENDIF
			NEXT 
            // set default parameters for missing parameters
            for VAR nArg := nMax+1 to aPars:Length
                local oPar as ParameterInfo
                oPar        := aPars[nArg]
                IF oPar:HasDefaultValue
                    oArgs[nArg] := oPar:DefaultValue
                ENDIF
            next
		ENDCASE
        return oArgs

	STATIC METHOD IsMethod( t AS System.Type, cName AS STRING ) AS LOGIC
		RETURN FindMethod(t, cName, TRUE) != NULL
		
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

	STATIC METHOD FindProperty( t AS Type , cName AS STRING, lAccess AS LOGIC, lSelf AS LOGIC) AS PropertyInfo
		DO WHILE t != NULL
			VAR oInfo := t:GetProperty( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public | IIF(lSelf , BindingFlags.NonPublic , BindingFlags.Public) | BindingFlags.DeclaredOnly ) 
			IF oInfo != NULL .AND. ( (lAccess .AND. oInfo:CanRead) .OR. (.NOT. lAccess .AND. oInfo:CanWrite) )
				RETURN oInfo
			ELSE
				t := t:BaseType
			ENDIF
		ENDDO
		RETURN NULL

	STATIC METHOD IsPropertyMethodVisible(oMethod AS MethodInfo, lSelf AS LOGIC) AS LOGIC
		IF oMethod == NULL_OBJECT
			RETURN FALSE
		ELSEIF oMethod:IsPublic
			RETURN TRUE
		ELSEIF lSelf .AND. (oMethod:IsFamily .OR. oMethod:IsFamilyOrAssembly)
			RETURN TRUE
		ENDIF
		RETURN FALSE
		
	STATIC METHOD FindField( t AS Type, cName AS STRING, lAccess AS LOGIC, lSelf AS LOGIC ) AS FieldInfo
		DO WHILE t != NULL
			VAR oInfo := t:GetField( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public | IIF(lSelf, BindingFlags.NonPublic , BindingFlags.Public | BindingFlags.DeclaredOnly ) ) 
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
		VAR fldInfo := FindField(t, cIVar, TRUE, lSelf)
		IF fldInfo != NULL_OBJECT .AND. IsFieldVisible(fldInfo, lSelf)
			RETURN fldInfo:GetValue(oObject)
		ENDIF
		VAR propInfo := FindProperty(t, cIVar, TRUE, lSelf) 
		IF propInfo != NULL_OBJECT .AND. propInfo:CanRead .AND. IsPropertyMethodVisible(propInfo:GetMethod, lSelf)
            IF propInfo:GetIndexParameters():Length == 0
			    RETURN propInfo:GetValue(oObject, NULL)
            ELSE
                RETURN NULL
            ENDIF
		ENDIF
		LOCAL result AS USUAL
		IF sendHelper(oObject, "NoIVarGet", <USUAL>{String2Symbol(cIVar)}, OUT result)
			RETURN result
		END IF
		THROW Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{cIVar} )
		
	STATIC METHOD IVarPut(oObject AS OBJECT, cIVar AS STRING, oValue AS OBJECT, lSelf AS LOGIC)  AS VOID
		LOCAL t AS Type
		t := oObject:GetType()
		VAR fldInfo := FindField(t, cIVar, FALSE, lSelf)
		//Todo: optimization
		IF fldInfo != NULL_OBJECT .AND. IsFieldVisible(fldInfo, lSelf)
			oValue := VOConvert(oValue, fldInfo:FieldType)
			fldInfo:SetValue(oObject, oValue)
			RETURN
		ENDIF
		LOCAL propInfo AS PropertyInfo
		propInfo := FindProperty(t, cIVar, FALSE, lSelf)
		IF propInfo != NULL_OBJECT .AND. propInfo:CanWrite .AND. IsPropertyMethodVisible(propInfo:SetMethod, lSelf)
			oValue := VOConvert(oValue, propInfo:PropertyType)
			propInfo:SetValue(oObject,oValue , NULL)
			RETURN
		ENDIF
		LOCAL dummy AS USUAL
		IF sendHelper(oObject, "NoIVarPut", <USUAL>{String2Symbol(cIVar), oValue}, OUT dummy)
			RETURN
		END IF
		THROW Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{cIVar})
		
	STATIC METHOD SendHelper(oObject AS OBJECT, cMethod AS STRING, uArgs AS USUAL[], result OUT USUAL) AS LOGIC
		LOCAL t := oObject?:GetType() AS Type
		result := NIL
		IF t == NULL
			THROW Error.NullArgumentError( __ENTITY__, NAMEOF(oObject), 1 )
		ENDIF
		LOCAL mi AS MethodInfo
        TRY
		    mi := t:GetMethod(cMethod,BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
        CATCH  as AmbiguousMatchException
            try
                var list := List<MethodInfo>{}
                foreach var minfo in t:GetMethods(BindingFlags.Instance | BindingFlags.Public)
                    if String.Compare(minfo:Name, cMethod, StringComparison.OrdinalIgnoreCase) == 0
                        list:Add(minfo)
                    endif
                next
                var mis := list:ToArray()
                mi := FindBestOverload(mis, "SendHelper",uArgs)
            catch as Exception
                mi := null
            end try
        END TRY
		IF mi == NULL
			// No Error Here. THat is done in the calling code
			RETURN FALSE
		ENDIF	
		RETURN sendHelper(oObject, mi, uArgs, OUT result)
		
	STATIC METHOD SendHelper(oObject AS OBJECT, mi AS MethodInfo , uArgs AS USUAL[], result OUT USUAL) AS LOGIC
        result := NULL
		IF mi != NULL   
            VAR oArgs := MatchParameters(mi, uArgs) 
			IF mi:ReturnType == typeof(USUAL)
                result := mi:Invoke(oObject, oArgs)
            ELSE
                LOCAL oResult AS OBJECT
                oResult := mi:Invoke(oObject, oArgs)
                result := oResult
            ENDIF
			
		ENDIF
		RETURN TRUE
		
	STATIC METHOD VOConvert(uValue AS USUAL,toType AS System.type) AS OBJECT
		IF toType == TYPEOF(FLOAT)
			RETURN (FLOAT) uValue
		ELSE
			IF toType == TYPEOF(USUAL)
				// box the usual
                RETURN __CASTCLASS(OBJECT, uValue)
            ELSEIF IsArray(uValue) .AND. totype == typeof(ARRAY)
                RETURN (ARRAY) uValue
            ELSEIF IsObject(uValue) .OR. isCodeBlock(uValue)
                RETURN (OBJECT) uValue
            ENDIF
      		
      		LOCAL oRet AS OBJECT
      		TRY
	      		oRet := Convert.ChangeType(uValue, toType)
      		CATCH
      			oRet := uValue
      		END TRY
			RETURN oRet
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
				THROW Error.VOError( EG_NOMETHOD, __ENTITY__, NAMEOF(cMethod), 2, <OBJECT>{cMethod} )
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
	LOCAL oError := Error.VOError(EG_WRONGCLASS, __FUNCTION__, NAMEOF(oObject),1, NULL) AS Error
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
		TRY
			LOCAL types := assembly:GetTypes() AS System.Type[]
			FOREACH type AS System.Type IN types
				TRY
					IF type:IsPublic
						classes:Add(String2Symbol(type:Name))
					ENDIF
				END TRY
			NEXT
//		CATCH oEx AS ReflectionTypeLoadException
		END TRY
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
	
/// <summary>Create a new instance of a named class</summary>	
FUNCTION CreateInstance(cClassName) AS OBJECT CLIPPER
	IF ! ( cClassName:IsSymbol || cClassName:IsString )
		THROW Error.DataTypeError( __FUNCTION__, NAMEOF(cClassName), 1, cClassName)
	ENDIF    	
	VAR t := OOPHelpers.FindClass((STRING) cClassName)
	IF t == NULL
		 THROW Error.VOError( EG_NOCLASS, __FUNCTION__, NAMEOF(cClassName), 1,  <OBJECT>{cClassName}  )
	ENDIF
	VAR constructors := t:getConstructors()
    VAR nPCount := PCount() 
	VAR args := USUAL[]{nPCount-1}
	FOR VAR nArg := 2 TO nPCount
		args[nArg-1] := _GetFParam(nArg)
	NEXT 
    LOCAL ctor := OOPHelpers.FindBestOverLoad(constructors, __FUNCTION__ ,args) as ConstructorInfo
	
	LOCAL oRet AS OBJECT  
	TRY
		LOCAL oArgs := OOPHelpers.MatchParameters(ctor, args) as OBJECT[]
		oRet := ctor:Invoke( oArgs )
	CATCH
		THROW Error.VOError( EG_NOMETHOD, __FUNCTION__, "Constructor", 0 , NULL)
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
	
	
	
	
/// <summary>
/// Check whether a particular access method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION IsAccess(o AS OBJECT,cName AS STRING) AS LOGIC
	VAR oprop := OOPHelpers.FindProperty(o?:GetType(), cName, TRUE, TRUE)
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
	VAR oprop := OOPHelpers.FindProperty(o?:GetType(), cName, FALSE, TRUE)
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
	// this was a smarter implemenation, but has performance issues
	// especially when cName is not found, as we cannot cache that
/*	LOCAL oType := OOPHelpers.FindClass(cName, FALSE) AS System.Type
	IF oType == NULL
		RETURN FALSE
	END IF
	RETURN oType:IsAssignableFrom(oObject:GetType())*/
	LOCAL oType AS Type
	oType := oObject:GetType()
	DO WHILE oType != NULL
		IF String.Compare(oType:Name, cName, TRUE) == 0
			RETURN TRUE
		END IF
		oType := oType:BaseType
	END DO
	RETURN FALSE
	
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
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(cIVar),2)
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
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(cIVar),2)
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
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(cIVar),2)
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
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(o),1)
	ENDIF
	IF String.IsNullOrEmpty(cIVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(cIVar),2)
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
	     THROW Error.VOError( EG_DATATYPE, __FUNCTION__, NAMEOF(o), 1, <OBJECT>{ o}  )
	ENDIF
	IF ! uMethod:IsString  .AND. ! uMethod:IsSymbol
		THROW Error.VOError( EG_DATATYPE, __FUNCTION__, NAMEOF(uMethod) , 2, <OBJECT>{ uMethod } )
	ENDIF
    IF args == NULL
        // this happens for SEND (oObject, "method", NULL)
        args := <USUAL>{NULL}
    ENDIF
	LOCAL oObject := o AS OBJECT
	LOCAL cMethod := uMethod AS STRING
	LOCAL uResult AS USUAL
	uResult := OopHelpers.DoSend(oObject, cMethod, args)
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
/// <esclude />
FUNCTION __InternalSend( oObject AS USUAL, cMethod AS STRING, args PARAMS USUAL[] ) AS USUAL
	RETURN OopHelpers.DoSend(oObject, cMethod, args)


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
	
/// <exclude/>	
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
		met := OOPHelpers.FindMethod(type, cMethod, TRUE)
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
		ELSEIF aFuncs:Length == 0
			RETURN NIL
		ELSE
			THROW Error.VOError( EG_AMBIGUOUSMETHOD,  "_CallClipFunc", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
		ENDIF
	ELSE
		THROW Error.VOError( EG_NOFUNC,  "FParamCount", NAMEOF(symFunction), 1, <OBJECT>{symFunction} )
	ENDIF

	RETURN  NIL   


/// <summary>Dynamically loads a library (dll) compiled with X#, running any _INIT procedures it may contain.</summary>
/// <param name="cLibFileName">The full path of the library to load.</param>
/// <returns>The Assembly object of the loaded library.</returns>
FUNCTION XSharpLoadLibrary(cLibFileName AS STRING) AS Assembly
	LOCAL oAssembly AS Assembly
	oAssembly := Assembly.LoadFrom(cLibFileName)
	LOCAL oModule AS Module
	oModule := oAssembly:GetModules()[1]
	LOCAL oMethod AS MethodInfo
	oMethod := oModule:GetMethod("RunInitProcs")
	IF oMethod != NULL
		oMethod:Invoke(NULL, NULL)
	END IF
RETURN oAssembly

FUNCTION EnableLBOptimizations(lSet AS LOGIC) AS LOGIC
    LOCAL lOld := OOPHelpers.EnableOptimizations AS LOGIC
    OOPHelpers.EnableOptimizations := lSet
    RETURN lOld
