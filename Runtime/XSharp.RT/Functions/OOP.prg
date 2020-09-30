//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

#pragma options("az", on)


USING XSharp.Internal
USING System.Reflection
USING System.Collections.Generic
USING System.Linq
USING System.Runtime.CompilerServices


INTERNAL STATIC CLASS OOPHelpers
    STATIC INTERNAL EnableOptimizations AS LOGIC
    STATIC INTERNAL cacheClassesAll AS Dictionary<STRING,Type>
    STATIC INTERNAL cacheClassesOurAssemblies AS Dictionary<STRING,Type>
    STATIC INTERNAL fieldPropCache    AS Dictionary<System.Type, Dictionary<STRING, MemberInfo> >
    STATIC INTERNAL overloadCache     AS Dictionary<System.Type, Dictionary<STRING, IList<MethodInfo>> >
    STATIC CONSTRUCTOR()
	    cacheClassesAll             := Dictionary<STRING,Type>{StringComparer.OrdinalIgnoreCase}
	    cacheClassesOurAssemblies   := Dictionary<STRING,Type>{StringComparer.OrdinalIgnoreCase}
        fieldPropCache              := Dictionary<System.Type, Dictionary<STRING, MemberInfo> >{}
        overloadCache               := Dictionary<System.Type, Dictionary<STRING, IList<MethodInfo>> >{}
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
			LOCAL atr := (ClassLibraryAttribute) (asm:GetCustomAttributes(cla,FALSE):First()) AS ClassLibraryAttribute
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
                    VAR list := GetOverLoads(oType, cFunction)
                    IF list != NULL
                        aMethods:AddRange(list)
                    ELSE
                        list := List<MethodInfo>{}
					    aMI := oType:GetMethods(bf)
					    FOREACH oM AS MethodInfo IN aMI
						    IF String.Compare(oM:Name, cFunction, TRUE) == 0
							    list:Add( oM )
						    ENDIF
                        NEXT
                        IF list:Count > 0
                            AddOverLoads(oType, cFunction, list)
                            aMethods:AddRange(list)
                        ENDIF
                    ENDIF
				END TRY 
			ENDIF
		NEXT
		RETURN aMethods:ToArray()
		
	STATIC METHOD FindClass(cName AS STRING) AS System.Type 
	    RETURN FindClass(cName, TRUE)

	STATIC METHOD FindClass(cName AS STRING, lOurAssembliesOnly AS LOGIC) AS System.Type 
		// TOdo Optimize FindClass
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
			VAR att := TYPEOF( ClassLibraryAttribute )
			IF asm:IsDefined(  att, FALSE )
                // there should be only one but it does not hurt to be cautious
                FOREACH VAR attribute IN asm:GetCustomAttributes(att,FALSE)
				    VAR cla := (ClassLibraryAttribute) attribute
                    IF !String.IsNullOrEmpty(cla:DefaultNameSpace)
				        VAR cFullName := cla:DefaultNameSpace +"."+cName
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
            att := TYPEOF( ImplicitNamespaceAttribute )
			IF asm:IsDefined(  att, FALSE )
                FOREACH VAR attribute IN asm:GetCustomAttributes(att,FALSE)
				    VAR ins := (ImplicitNamespaceAttribute) attribute
                    IF !String.IsNullOrEmpty(ins:Namespace)
				        VAR cFullName := ins:Namespace+"."+cName
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
        IF ret == NULL
            // try to find classes in a namespace
            FOREACH asm AS Assembly IN aAssemblies
                VAR cla := TYPEOF( ClassLibraryAttribute )
			    IF asm:IsDefined(  cla, FALSE )
                    VAR types := asm:GetTypes()
                    FOREACH type AS System.Type IN types
                        IF String.Compare(type:Name, cName, StringComparison.OrdinalIgnoreCase) == 0
                            ret := type
                            EXIT
                        ENDIF
                    NEXT
                    IF ret != NULL
                        EXIT
                    ENDIF
                ENDIF
            NEXT
        ENDIF

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

    STATIC METHOD CompareMethods(m1 AS MethodBase, m2 AS MethodBase, uArgs AS USUAL[]) AS LONG
        VAR p1 := m1:GetParameters()
        VAR p2 := m2:GetParameters()

        IF p1:Length != p2:Length
            IF p1:Length == uArgs:Length
                RETURN 1
            ELSEIF p2:Length == uArgs:Length
                RETURN 2
            ENDIF
        ENDIF
        // when we get here then the parameter counts are the same
        FOR VAR nPar := 0 TO p1:Length-1
            IF nPar > uArgs:Length-1
                EXIT
            ENDIF
            VAR par1 := p1[nPar]
            VAR par2 := p2[nPar]
            VAR parType1 := par1:ParameterType
            VAR parType2 := par2:ParameterType
            VAR arg  := uArgs[nPar]
            IF  parType1 != parType2
                IF parType1:IsAssignableFrom(arg:SystemType)
                    RETURN 1
                ENDIF
                IF parType2:IsAssignableFrom(arg:SystemType)
                    RETURN 2
                ENDIF
                IF parType1 = typeof(USUAL)
                    RETURN 1
                ENDIF
                IF parType2 = typeof(USUAL)
                    RETURN 2
                ENDIF
            ENDIF
        NEXT
        RETURN 0

    STATIC METHOD FindBestOverLoad<T>(overloads AS T[], cFunction AS STRING, uArgs AS USUAL[]) AS T WHERE T IS MethodBase
        IF overloads:Length <= 1
            RETURN overloads:FirstOrDefault()
        ENDIF
        // More than one
        VAR found := List<T>{}
        // first look for methods with the same ! of parametes
        FOREACH VAR m IN overloads
            IF m:GetParameters():Length == uArgs:Length
                found:Add(m)
            ENDIF
        NEXT
        IF found:Count == 1
            RETURN found:First() // collection, so 0 based !
        ENDIF
        // then look for methods with
        found:Clear()
        FOR VAR nMethod := 0 TO overloads:Length -2
            VAR m1     := overloads[nMethod]
            VAR m2     := overloads[nMethod+1]
            VAR result := CompareMethods(m1, m2, uArgs)
            IF result == 1
                IF ! found:Contains(m1)
                    found:Add(m1)
                ENDIF
            ELSEIF result == 2
                IF ! found:Contains(m2)
                    found:Add(m2)
                ENDIF
            ENDIF
        NEXT
        IF found:Count == 1
            RETURN found:First() 
        ENDIF
        LOCAL cClass AS STRING
        cClass := overloads:First():DeclaringType:Name
        VAR oError := Error.VOError( EG_AMBIGUOUSMETHOD, cFunction, "MethodName", 1, <OBJECT>{cClass+":"+overloads:First():Name})
        oError:Description := oError:Message+" ' "+cFunction+"'"
        THROW oError
        

    STATIC METHOD MatchParameters<T>( methodinfo AS T, args AS USUAL[], hasByRef OUT LOGIC) AS OBJECT[] WHERE T IS MethodBase
        // args contains the list of arguments. The methodname has already been deleted when appropriated
		LOCAL oArgs AS OBJECT[]
        LOCAL lClipper := FALSE AS LOGIC
        hasByRef := FALSE
        VAR aPars := methodinfo:GetParameters()
        VAR numDefinedParameters := aPars:Length
        VAR numActualParameters  := args:Length
        IF numDefinedParameters == 1 .AND. methodinfo:IsDefined(TYPEOF(ClipperCallingConventionAttribute),FALSE)
            lClipper := TRUE
        ENDIF
        DO CASE
        CASE lClipper
            // pass the whole array of clipper parameters (usual[]) as single parameter        
			oArgs  := <OBJECT>{args}
        CASE aPars:Length == 0 
			// no args
			oArgs := NULL
		OTHERWISE
			// convert args to array of objects
			oArgs := OBJECT[]{numDefinedParameters}
            IF numDefinedParameters < numActualParameters
                // ignore extra parameters
                numActualParameters := numDefinedParameters
            ENDIF
			FOR VAR nPar := 0 TO numActualParameters -1
                LOCAL pi        := aPars[nPar] AS ParameterInfo
                LOCAL parType   := pi:ParameterType AS System.Type
                LOCAL arg       := args[nPar] AS USUAL
                IF parType:IsByRef
                    // Get the referenced type. We assume it is in the assembly where the ByRef type is also defined
                    // I am not sure if that is always true ?
                    hasByRef := TRUE
                    VAR typeName := parType:FullName
                    typeName := typeName:Substring(0, typeName:Length-1)
                    TRY
                        VAR referencedType := parType:Assembly:GetType(typeName)
                        IF referencedType != NULL
                            parType := referencedType
                        ENDIF
                    CATCH
                        NOP
                    END TRY
                ENDIF
                IF parType == TYPEOF(USUAL)
					// We need to box a usual here 
    				oArgs[nPar] := __CASTCLASS(OBJECT, arg)
                ELSEIF arg == NIL
                    // This is new in X#: a NIL in the middle of the parameter list gets set to the default value now 
                    oArgs[nPar] := GetDefaultValue(pi)
                ELSEIF arg == NULL .or. parType:IsAssignableFrom(arg:SystemType) // Null check must appear first !
					oArgs[nPar] := arg
                ELSEIF pi:GetCustomAttributes( TYPEOF( ParamArrayAttribute ), FALSE ):Length > 0
                    // Parameter array of certain type
					// -> convert remaining elements from uArgs to an array and assign that to oArgs[i] 
					LOCAL elementType := parType:GetElementType() AS System.Type
					LOCAL aVarArgs    := System.Array.CreateInstance(elementType, args:Length - nPar +1) AS System.Array
					FOR VAR nArg := nPar TO numActualParameters -1
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
                ELSE
                    
                    // try to convert to the expected type, but don't do this for out parameters.
                    // We can leave the slot empty for out parameters
                    IF ! pi:IsOut
					    oArgs[nPar]  := VOConvert(args[nPar], parType)
                    ENDIF
                ENDIF
			NEXT 
            // set default values for missing parameters, so we start after the last parameter
            FOR VAR nArg := numActualParameters TO numDefinedParameters -1
                LOCAL oPar AS ParameterInfo
                oPar        := aPars[nArg]
                VAR oArg    := GetDefaultValue(oPar)
                IF oArg != NULL
                    oArgs[nArg] := oArg
                ELSE
                    oArgs[nArg] := NIL
                ENDIF
            NEXT
		ENDCASE
        RETURN oArgs

    STATIC METHOD GetDefaultValue(oPar AS ParameterInfo) AS OBJECT
        LOCAL result := NULL AS OBJECT
        IF oPar:HasDefaultValue
            result := oPar:DefaultValue
        ELSE
            LOCAL oDefAttrib AS DefaultParameterValueAttribute
            oDefAttrib := (DefaultParameterValueAttribute) oPar:GetCustomAttribute(TypeOf(DefaultParameterValueAttribute))
            IF oDefAttrib != NULL
                SWITCH oDefAttrib:Flag 
                CASE 1 // NIL
                	NOP // it is already NIL
                CASE 2 // DATE, stored in Ticks
	                result := DATE{ (INT64)oDefAttrib:Value }
                CASE 3 // SYMBOL
	                result := String2Symbol( (STRING)oDefAttrib:Value )
                CASE 4 // NULL_PSZ
                    IF oDefAttrib:Value IS STRING
                        // Note: Do not use String2Psz() because that PSZ will be freed when this method finishes !
                        result := PSZ{ (STRING) oDefAttrib:Value}
                    ELSE
	                	result := PSZ{IntPtr.Zero}
                    ENDIF
                CASE  5 // NULL_PTR
                    IF oDefAttrib:Value IS Int32
                        result := IntPtr{ (Int32) oDefAttrib:Value}
                    ELSE
                		result := IntPtr.Zero
                    ENDIF
                OTHERWISE
	                result := oDefAttrib:Value
                END SWITCH
            END IF
        ENDIF
        RETURN result
    
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
		// Note that VO only returns PUBLIC properties and fields
		VAR aFields := t:GetFields( BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
		VAR list := List<STRING>{}
		FOREACH fi AS FieldInfo IN aFields
			IF fi:IsPublic || (fi:IsFamily  .and. fi:IsDefined(typeof(IsInstanceAttribute), false))
				VAR name := fi:Name:ToUpperInvariant()
				IF ! list:Contains(name)
					list:Add(name)
				ENDIF
			ENDIF
		NEXT
		
		VAR aProps := t:GetProperties( BindingFlags.Instance | BindingFlags.Public )
		
		FOREACH pi AS PropertyInfo IN aProps
			VAR name := pi:Name:ToUpperInvariant()
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
				VAR name := oInfo:Name:ToUpperInvariant()
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
        IF t == NULL .OR. String.IsNullOrEmpty(cName)
            RETURN NULL
        ENDIF
        VAR mi := GetMember(t, cName)
        IF mi != NULL
            IF mi IS PropertyInfo VAR pi
                // we must check. Sometimes in a subclass the Access was overwritten but not the assign
                // then we want to read the assign from the parent class
                IF lAccess .and. pi:CanRead .and. IsPropertyMethodVisible(pi:GetMethod, lSelf)
                    RETURN pi
                ELSEIF ! lAccess .and. pi:CanWrite .and. IsPropertyMethodVisible(pi:SetMethod, lSelf)
                    RETURN pi
                ENDIF
            ELSE
                RETURN NULL
            ENDIF
        ELSE
            VAR pi := FindProperty(t:BaseType, cName, lAccess, lSelf)
            IF pi != NULL
                RETURN pi
            ENDIF
        ENDIF

        var bf := BindingFlags.Instance | BindingFlags.IgnoreCase |  BindingFlags.DeclaredOnly | BindingFlags.Public
        IF lSelf
            bf |= BindingFlags.NonPublic 
        ENDIF
		DO WHILE t != NULL
			VAR oInfo := t:GetProperty( cName, bf) 
			IF oInfo != NULL .AND. ( (lAccess .AND. oInfo:CanRead) .OR. (.NOT. lAccess .AND. oInfo:CanWrite) )
                AddMember(t, cName, oInfo)
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


    STATIC METHOD GetMember(t AS Type, cName AS STRING) AS MemberInfo
        IF t != NULL .AND. ! String.IsNullOrEmpty(cName) .AND. fieldPropCache:ContainsKey(t)
            VAR fields := fieldPropCache[t]
            IF fields:ContainsKey(cName)
                VAR result := fields[cName]
                RETURN result
            ENDIF
        ENDIF
        RETURN NULL

    STATIC METHOD AddMember(t AS Type, cName AS STRING, mi AS MemberInfo) AS LOGIC
        IF t != NULL .AND. ! String.IsNullOrEmpty(cName) 
            IF ! fieldPropCache:ContainsKey(t)
                fieldPropCache:Add( t, Dictionary<STRING, MemberInfo> {StringComparison.OrdinalIgnoreCase})
            ENDIF
            VAR fields := fieldPropCache[t]
            IF !fields:ContainsKey(cName)
                fields:Add(cName, mi)
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE


	STATIC METHOD FindField( t AS Type, cName AS STRING, lAccess AS LOGIC, lSelf AS LOGIC ) AS FieldInfo
        IF t == NULL .OR. String.IsNullOrEmpty(cName)
            RETURN NULL
        ENDIF
        VAR mi := GetMember(t, cName)
        IF mi != NULL
            IF mi IS FieldInfo VAR fi .AND. IsFieldVisible(fi, lSelf)
                RETURN fi
            ENDIF
            RETURN NULL     // it must be a property then
        ENDIF
        VAR bt := t
        var bf := BindingFlags.Instance | BindingFlags.IgnoreCase |  BindingFlags.DeclaredOnly | BindingFlags.Public
        IF lSelf
            bf |= BindingFlags.NonPublic
        ENDIF        
		DO WHILE t != NULL
			VAR oInfo := t:GetField( cName, bf ) 
			IF oInfo != NULL 
				// check for readonly (initonly) fields
				IF lAccess .OR. ! oInfo:Attributes:HasFlag(FieldAttributes.InitOnly)
                    AddMember(bt, cName, oInfo)
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
		
    
	STATIC METHOD IVarGet(oObject AS OBJECT, cIVar AS STRING, lSelf AS LOGIC) AS USUAL
		LOCAL t AS Type
        LOCAL result AS OBJECT
        IF oObject == NULL_OBJECT
            THROW Error.NullArgumentError(__FUNCTION__, nameof(oObject),1)
        ENDIF
        IF String.IsNullOrEmpty(cIVar)
            THROW Error.NullArgumentError(__FUNCTION__, nameof(cIVar),2)
        ENDIF
        // VFP Empty and XPP DataObject
        IF oObject IS IDynamicProperties VAR oDynamic
            RETURN oDynamic:NoIvarGet(cIVar)
        ENDIF
		t := oObject:GetType()
        TRY
		    VAR propInfo := FindProperty(t, cIVar, TRUE, lSelf)
            if propInfo != NULL_OBJECT 
                IF propInfo:GetIndexParameters():Length == 0
			        result := propInfo:GetValue(oObject, NULL)
                    IF result == NULL .AND. propInfo:PropertyType == TYPEOF(System.String)
                        result := String.Empty
                    ENDIF
                    RETURN result
                ELSE
                    RETURN NIL
                ENDIF
            ENDIF
    		VAR fldInfo := FindField(t, cIVar, TRUE, lSelf)
		    IF fldInfo != NULL_OBJECT 
                result := fldInfo:GetValue(oObject)
                IF result == NULL .AND. fldInfo:FieldType == TYPEOF(System.String)
                    result := String.Empty
                ENDIF
                RETURN result
		    ENDIF
        CATCH e as Exception
            IF e:InnerException != NULL
                THROW Error{e:GetInnerException()}
            ENDIF
            THROW // rethrow exception
        END TRY
        cIVar := cIVar:ToUpperInvariant()
		IF SendHelper(oObject, "NoIVarGet", <USUAL>{cIVar}, OUT VAR oResult)
			RETURN oResult
		END IF
		VAR oError := Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{oObject, cIVar} )
        oError:Description := oError:Message+" '"+cIVar+"'"
        THROW oError
		
	STATIC METHOD IVarPut(oObject AS OBJECT, cIVar AS STRING, oValue AS OBJECT, lSelf AS LOGIC)  AS VOID
		LOCAL t AS Type
        IF oObject == NULL_OBJECT
            THROW Error.NullArgumentError(__FUNCTION__, nameof(oObject),1)
        ENDIF
        IF String.IsNullOrEmpty(cIVar)
            THROW Error.NullArgumentError(__FUNCTION__, nameof(cIVar),2)
        ENDIF
        // VFP Empty and XPP DataObject
        IF oObject IS IDynamicProperties VAR oDynamic
            oDynamic:NoIvarPut(cIVar, oValue)
            RETURN
        ENDIF
		t := oObject:GetType()
        TRY
		    VAR propInfo := FindProperty(t, cIVar, FALSE, lSelf)
		    IF propInfo != NULL_OBJECT 
			    oValue := VOConvert(oValue, propInfo:PropertyType)
			    propInfo:SetValue(oObject,oValue , NULL)
			    RETURN
		    ENDIF
		    VAR fldInfo := FindField(t, cIVar, FALSE, lSelf)
		    IF fldInfo != NULL_OBJECT 
			    oValue := VOConvert(oValue, fldInfo:FieldType)
			    fldInfo:SetValue(oObject, oValue)
			    RETURN
            ENDIF
            cIVar := cIVar:ToUpperInvariant()
		    IF SendHelper(oObject, "NoIVarPut", <USUAL>{cIVar, oValue})
			    RETURN
		    END IF
		    VAR oError :=  Error.VOError( EG_NOVARMETHOD, IIF( lSelf, __ENTITY__, __ENTITY__ ), NAMEOF(cIVar), 2, <OBJECT>{oObject, cIVar, oValue, lSelf})
		    oError:Description := oError:Message+" '"+cIVar+"'"
            THROW oError
        CATCH e as Exception
            IF e:InnerException != NULL
                THROW Error{e:GetInnerException()}
            ENDIF
            THROW // rethrow exception
        END TRY

    STATIC METHOD SendHelper(oObject AS OBJECT, cMethod AS STRING, uArgs AS USUAL[]) AS LOGIC
        LOCAL lOk := SendHelper(oObject, cMethod, uArgs, OUT VAR result) AS LOGIC
        oObject := result   // get rid of warning
        RETURN lOk

    STATIC METHOD GetOverLoads(t AS System.Type, cMethod AS STRING) AS IList<MethodInfo>
        IF t == NULL .OR. String.IsNullOrEmpty(cMethod)
            RETURN List<MethodInfo>{}
        ENDIF
        IF overloadCache:ContainsKey(t)
            VAR type := overloadCache[t]
            IF type:ContainsKey(cMethod)
                VAR result := type[cMethod]
                RETURN result
            ENDIF
        ENDIF
        RETURN NULL

    STATIC METHOD AddOverLoads(t AS System.Type, cMethod AS STRING, ml AS IList<MethodInfo>) AS LOGIC
        IF !overloadCache:ContainsKey(t)
            overloadCache:Add(t, Dictionary<STRING, IList<MethodInfo> >{StringComparer.OrdinalIgnoreCase})
        ENDIF
        VAR type := overloadCache[t]
        IF type:ContainsKey(cMethod)
            RETURN FALSE
        ENDIF
        type:Add(cMethod, ml)
        RETURN TRUE

	STATIC METHOD SendHelper(oObject AS OBJECT, cMethod AS STRING, uArgs AS USUAL[], result OUT USUAL) AS LOGIC
		LOCAL t := oObject?:GetType() AS Type
		result := NIL
		IF t == NULL
			THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(oObject), 1 )
		ENDIF
		IF cMethod == NULL
			THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(cMethod), 2 )
		ENDIF
		LOCAL mi := NULL AS MethodInfo
        cMethod := cMethod:ToUpperInvariant()
        VAR list := GetOverLoads(t, cMethod)
        IF list == NULL
            TRY
		        mi := t:GetMethod(cMethod,BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
            CATCH  AS AmbiguousMatchException
                mi := NULL
            END TRY
        ENDIF
        IF mi == NULL
            IF list == NULL
                list := List<MethodInfo>{}
                FOREACH VAR minfo IN t:GetMethods(BindingFlags.Instance | BindingFlags.Public)
                    IF String.Compare(minfo:Name, cMethod, StringComparison.OrdinalIgnoreCase) == 0
                        list:Add(minfo)
                    ENDIF
                NEXT
                IF list:Count > 0
                    AddOverLoads(t, cMethod, list)
                ENDIF
            ENDIF
            TRY
                IF list:Count > 0
                    VAR mis := list:ToArray()
                    mi := FindBestOverLoad(mis, "SendHelper",uArgs)
                ENDIF
            CATCH AS Exception
                mi := NULL
            END TRY
        ENDIF
		IF mi == NULL
			// No Error Here. THat is done in the calling code
			RETURN FALSE
		ENDIF	
		RETURN SendHelper(oObject, mi, uArgs, OUT result)
		
	STATIC METHOD SendHelper(oObject AS OBJECT, mi AS MethodInfo , uArgs AS USUAL[], result OUT USUAL) AS LOGIC
        result := NIL
		IF oObject == NULL .AND. ! mi:IsStatic
			THROW Error.NullArgumentError( __ENTITY__, NAMEOF(oObject), 1 )
		ENDIF
		IF mi != NULL   
            VAR oArgs := MatchParameters(mi, uArgs, OUT VAR hasByRef) 
            TRY
			    IF mi:ReturnType == typeof(USUAL)
                    result := mi:Invoke(oObject, oArgs)
                ELSE
                    LOCAL oResult AS OBJECT
                    oResult := mi:Invoke(oObject, oArgs)
                    IF oResult == NULL .AND. mi:ReturnType == TYPEOF(STRING)
                        oResult := String.Empty
                    ENDIF
                    result := oResult
                ENDIF
                IF hasByRef
                    CopyByRefParameters( uArgs, oArgs, mi:GetParameters())
                ENDIF
            CATCH e as Exception
                IF e:InnerException != NULL
                    THROW Error{e:GetInnerException()}
                ENDIF
                THROW // rethrow exception
            END TRY
			
		ENDIF
		RETURN TRUE

    STATIC METHOD CopyByRefParameters(uArgs as USUAL[], oArgs as OBJECT[], pars as ParameterInfo[]) AS VOID
        // Assign parameters back.
        VAR max    := Math.Min(uArgs:Length, oArgs:Length)  -1
        FOR VAR nParam := 0 to max
            LOCAL param := pars[nParam] as ParameterInfo
            IF param:IsOut .or. param:ParameterType:IsByRef
                IF uArgs[nParam]:IsByRef
                    uArgs[nParam] := oArgs[nParam]
                ENDIF
            ENDIF
        NEXT
        
	STATIC METHOD VOConvert(uValue AS USUAL,toType AS System.Type) AS OBJECT
		IF toType == TYPEOF(FLOAT)
			RETURN (FLOAT) uValue
		ELSE
			IF toType == TYPEOF(USUAL)
				// box the usual
                RETURN __CASTCLASS(OBJECT, uValue)
            ELSEIF toType == typeof(DATE) .AND. uValue:IsDateTime
                RETURN (DATE)(DateTime) uValue
            ELSEIF uValue:IsArray .AND. toType == typeof(ARRAY)
                RETURN (ARRAY) uValue
            ELSEIF uValue:IsObject .OR. uValue:IsCodeblock
                RETURN (OBJECT) uValue
            ELSEIF uValue:IsPtr .AND. toType == typeof(PTR)
                return IntPtr{(PTR) uValue}
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
		IF oObject == NULL
			THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(oObject), 1 )
		ENDIF
		IF cMethod == NULL
			THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(cMethod), 2 )
		ENDIF
		IF ! SendHelper(oObject, cMethod, args, OUT VAR result)
			LOCAL nomethodArgs AS USUAL[]
    	    cMethod := cMethod:ToUpperInvariant()
    	    RuntimeState.NoMethod := cMethod   // For NoMethod() function
            IF XSharp.RuntimeState.Dialect == XSharpDialect.Vulcan
                // vulcan includes the method name
			    nomethodArgs := USUAL[]{ args:Length+1 } 
                nomethodArgs[__ARRAYBASE__] := cMethod
			    Array.Copy( args, 0, nomethodArgs, 1, args:Length )
            ELSE
                // other dialects do not include the method name
			    nomethodArgs := USUAL[]{ args:Length } 
			    Array.Copy( args, 0, nomethodArgs, 0, args:Length )
            ENDIF
			IF ! SendHelper(oObject, "NoMethod" , nomethodArgs, OUT result)
                VAR oError := Error.VOError( EG_NOMETHOD, __FUNCTION__, nameof(cMethod), 2, <OBJECT>{oObject, cMethod, args} )
                oError:Description  := oError:Message + " '"+cMethod+"'"
                THROW oError

			ENDIF
		ENDIF
		RETURN result
		
END CLASS
		
		
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asend/*" />
FUNCTION ASend(aTarget AS ARRAY, symMethod AS STRING, MethodArgList PARAMS USUAL[] ) AS ARRAY 
	IF aTarget != NULL .AND. ! String.IsNullOrEmpty( symMethod )
		FOREACH VAR x IN aTarget
			__InternalSend( x, symMethod, MethodArgList )
		NEXT
	ENDIF   
	RETURN aTarget
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/checkinstanceof/*" />
FUNCTION CheckInstanceOf(oObject AS OBJECT,symClassName AS STRING) AS LOGIC
	IF oObject == NULL_OBJECT
		RETURN FALSE
	ELSEIF IsInstanceOf(oObject, symClassName)
		RETURN TRUE
	ENDIF
	LOCAL oError := Error.VOError(EG_WRONGCLASS, __FUNCTION__, NAMEOF(oObject),1, NULL) AS Error
	oError:Description := symClassName + " <-> " + oObject:GetType():Name
	THROW oError
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classcount/*" />
FUNCTION ClassCount() AS DWORD
	RETURN ClassList():Length
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classlist/*" />
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
                CATCH as Exception
                    NOP
                
				END TRY
			NEXT
//		CATCH oEx AS ReflectionTypeLoadException
        CATCH as Exception
            NOP
		END TRY
	NEXT
	RETURN classes
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classname/*" />
FUNCTION ClassName(oObject AS OBJECT) AS STRING
    IF oObject != NULL
	    RETURN oObject?:GetType():Name:ToUpper()
    ENDIF
    RETURN ""
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classtree/*" />
FUNCTION ClassTree(oObject AS OBJECT) AS ARRAY
    IF oObject != NULL
	    RETURN OOPHelpers.ClassTree(oObject?:GetType())
    ENDIF
    RETURN {}
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/createinstance/*" />
FUNCTION CreateInstance(symClassName,InitArgList) AS OBJECT CLIPPER
	IF ! ( symClassName:IsSymbol || symClassName:IsString )
		THROW Error.DataTypeError( __FUNCTION__, NAMEOF(symClassName), 1, symClassName)
	ENDIF    	
	VAR t := OOPHelpers.FindClass((STRING) symClassName)
	IF t == NULL
		 VAR oError := Error.VOError( EG_NOCLASS, __FUNCTION__, NAMEOF(symClassName), 1,  <OBJECT>{symClassName}  )
         oError:Description := oError:Message+" '"+symClassName+"'"
         THROW oError
	ENDIF
	VAR constructors := t:GetConstructors()
    VAR nPCount := PCount() 
	VAR uArgs := USUAL[]{nPCount-1}
	FOR VAR nArg := 1 TO nPCount-1
		uArgs[nArg-1] := _GetFParam(nArg+1) // _GetFParam() is 1 based !
	NEXT 
    LOCAL ctor := OOPHelpers.FindBestOverLoad(constructors, __FUNCTION__ ,uArgs) AS ConstructorInfo
	IF ctor == NULL
    	VAR oError := Error.VOError( EG_NOMETHOD, __FUNCTION__, "Constructor", 0 , NULL)
        oError:Description := "No CONSTRUCTOR defined for type "+ (String) symClassName
        throw oError
    ENDIF
	LOCAL oRet AS OBJECT  
	TRY
		LOCAL oArgs := OOPHelpers.MatchParameters(ctor, uArgs, OUT VAR hasByRef) AS OBJECT[]
		oRet := ctor:Invoke( oArgs )
        IF hasByRef
            OOPHelpers.CopyByRefParameters(uArgs, oArgs, ctor:GetParameters())
            
        ENDIF
    CATCH e as Error
        THROW e
    CATCH e as Exception
        THROW Error{e:GetInnerException()}
	END TRY
	RETURN oRet
	
	
	
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classtreeclass/*" />
FUNCTION ClassTreeClass(symClass AS STRING) AS ARRAY
	VAR t := OOPHelpers.FindClass(symClass)
	IF t != NULL
		RETURN OOPHelpers.ClassTree(t)
	ELSE
		THROW Error{EG_NOCLASS,0}
	ENDIF
	
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isaccess/*" />
FUNCTION IsAccess(oObject AS OBJECT,symAccess AS STRING) AS LOGIC
    IF oObject != NULL
	    VAR oProp := OOPHelpers.FindProperty(oObject?:GetType(), symAccess, TRUE, TRUE)
	    IF oProp != NULL_OBJECT
		    RETURN oProp:CanRead
        ENDIF
    ENDIF
	RETURN FALSE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isassign/*" />
FUNCTION IsAssign(oObject AS OBJECT,symAssign AS STRING) AS LOGIC
    IF oObject != NULL
	    VAR oProp := OOPHelpers.FindProperty(oObject?:GetType(), symAssign, FALSE, TRUE)
	    IF oProp != NULL_OBJECT
		    RETURN oProp:CanWrite
        ENDIF
    ENDIF
	RETURN FALSE
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isclass/*" />
FUNCTION IsClass(symClassName AS STRING) AS LOGIC
	RETURN OOPHelpers.FindClass(symClassName) != NULL
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isclassof/*" />
FUNCTION IsClassOf(symClassName AS STRING,symSuperClassName AS STRING) AS LOGIC
	LOCAL tSub   := OOPHelpers.FindClass(symClassName) AS Type
	LOCAL tSuper := OOPHelpers.FindClass(symSuperClassName) AS Type
	// IsClassOf() in VO returns TRUE when child and parent class is the same (and it exists)
	RETURN tSub != NULL .AND. tSuper != NULL .AND. (tSub == tSuper .OR. tSub:IsSubclassOf(tSuper))


/// <summary>
/// Find a class in the referenced assemblies
/// </summary>
/// <param name="cClassName">Classname to find</param>
/// <returns>System.Type object or NULL </returns>

FUNCTION FindClass(cClassname AS STRING) AS System.Type
	RETURN OOPHelpers.FindClass(cClassname) 
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isinstanceof/*" />
FUNCTION IsInstanceOf(oObject AS OBJECT,symClassName AS STRING) AS LOGIC
	IF oObject == NULL_OBJECT
		RETURN FALSE
	ENDIF
	// this was a smarter implemenation, but has performance issues
	// especially when symClassName is not found, as we cannot cache that
/*	LOCAL oType := OOPHelpers.FindClass(cName, FALSE) AS System.Type
	IF oType == NULL
		RETURN FALSE
	END IF
	RETURN oType:IsAssignableFrom(oObject:GetType())*/
	LOCAL oType AS Type
	oType := oObject:GetType()
	DO WHILE oType != NULL
		IF String.Compare(oType:Name, symClassName, TRUE) == 0
			RETURN TRUE
		END IF
		oType := oType:BaseType
	END DO
	RETURN FALSE
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isinstanceofusual/*" />
FUNCTION IsInstanceOfUsual(uObject AS USUAL,symClassName AS STRING) AS LOGIC
    SWITCH uObject:Type
    CASE __UsualType.Object
    CASE __UsualType.Codeblock
    CASE __UsualType.Array
    CASE __UsualType.Decimal
    CASE __UsualType.Currency
    	RETURN IsInstanceOf(uObject, symClassName)
    END SWITCH
    RETURN FALSE
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarget/*" />
FUNCTION IVarGet(oObject AS OBJECT,symInstanceVar AS STRING) AS USUAL
	IF oObject == NULL_OBJECT
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(oObject),1)
	ENDIF
	IF String.IsNullOrEmpty(symInstanceVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(symInstanceVar),2)
	ENDIF
	RETURN OOPHelpers.IVarGet(oObject, symInstanceVar, FALSE)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivargetinfo/*" />
FUNCTION IVarGetInfo(oObject AS OBJECT,symInstanceVar AS STRING) AS DWORD
    RETURN OOPHelpers.IVarHelper(oObject, symInstanceVar, TRUE)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethod/*" />
FUNCTION IsMethod(oObject AS OBJECT,symMethod AS STRING) AS LOGIC
	IF oObject != NULL_OBJECT
	    RETURN OOPHelpers.IsMethod(oObject?:GetType(), symMethod)
    ENDIF
    RETURN FALSE
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethodusual/*" />
FUNCTION IsMethodUsual(uObject AS USUAL,symMethod AS STRING) AS LOGIC
	IF uObject:IsObject
		RETURN IsMethod( uObject, symMethod )
	ENDIF
	RETURN FALSE
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethodclass/*" />
FUNCTION IsMethodClass( symClass AS STRING, symMethod AS STRING ) AS LOGIC
	VAR t := OOPHelpers.FindClass( symClass )
	
	IF t != NULL
		RETURN OOPHelpers.IsMethod( t, symMethod )
	ENDIF
	RETURN FALSE
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivargetself/*" />
FUNCTION IVarGetSelf(oObject AS OBJECT,symInstanceVar AS STRING) AS USUAL
	IF oObject == NULL_OBJECT
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(oObject),1)
	ENDIF
	IF String.IsNullOrEmpty(symInstanceVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(symInstanceVar),2)
	ENDIF
	RETURN OOPHelpers.IVarGet(oObject, symInstanceVar, TRUE)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarlist/*" />
FUNCTION IvarList(oObject AS OBJECT) AS ARRAY
    // IVarList already checks for NULL_OBJECT
    IF oObject IS IDynamicProperties var oDynamic
        var props := oDynamic:GetPropertyNames()
        var result := {}
        FOREACH var prop in props
            result:Add(prop:ToUpper())
        NEXT
        RETURN result
    ENDIF
    RETURN OOPHelpers.IVarList(oObject?:GetType())
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarlistclass/*" />
FUNCTION IvarListClass(symClass AS STRING) AS ARRAY
	VAR t := OOPHelpers.FindClass(symClass)
	RETURN OOPHelpers.IVarList(t)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarputinfo/*" />
FUNCTION IVarPutInfo(oObject AS OBJECT,symInstanceVar AS SYMBOL) AS DWORD
    // IVarHelper already checks for NULL_OBJECT
    RETURN OOPHelpers.IVarHelper(oObject, symInstanceVar, FALSE)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarput/*" />
FUNCTION IVarPut(oObject AS OBJECT,symInstanceVar AS STRING,uValue AS USUAL) AS USUAL
	IF oObject == NULL_OBJECT
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(oObject),1)
	ENDIF
	IF String.IsNullOrEmpty(symInstanceVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(symInstanceVar),2)
	ENDIF
	OOPHelpers.IVarPut(oObject, symInstanceVar, uValue, FALSE)
	RETURN uValue
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarputself/*" />
FUNCTION IVarPutSelf(oObject AS OBJECT,symInstanceVar AS STRING,uValue AS USUAL) AS USUAL
	IF oObject == NULL_OBJECT
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(oObject),1)
	ENDIF
	IF String.IsNullOrEmpty(symInstanceVar)
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(symInstanceVar),2)
	ENDIF
	OOPHelpers.IVarPut(oObject, symInstanceVar, uValue,TRUE) 
	RETURN uValue
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/methodlist/*" />
FUNCTION MethodList(oClass AS OBJECT) AS ARRAY
	IF oClass != NULL
		RETURN OOPHelpers.MethodList( oClass:GetType() )
	ENDIF
	RETURN NULL_ARRAY
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/methodlistclass/*" />
FUNCTION MethodListClass( symClass AS STRING ) AS ARRAY
	LOCAL aReturn AS ARRAY
	VAR t := OOPHelpers.FindClass( symClass )
	IF t != NULL
		aReturn := OOPHelpers.MethodList( t )
	ELSE
		aReturn  := NULL_ARRAY
	ENDIF
	
	RETURN aReturn
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/nomethod/*" />
FUNCTION NoMethod() AS STRING
	RETURN RuntimeState.NoMethod
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/object2array/*" />
FUNCTION Object2Array(oObject AS OBJECT) AS ARRAY
	LOCAL t AS System.Type
	IF oObject == NULL_OBJECT
		RETURN NULL_ARRAY
	ENDIF
	LOCAL aProps AS PropertyInfo[]
	LOCAL aFields AS FieldInfo[]
	LOCAL aResult AS ARRAY
	aResult := {}
	t := oObject:GetType()
	aProps := t:GetProperties(BindingFlags.Instance | BindingFlags.Public)
    TRY
	    FOREACH p AS PropertyInfo IN aProps
		    LOCAL uVal AS USUAL
		    IF p:CanRead 
			    uVal := p:GetValue(oObject,NULL)
			    AAdd(aResult, uVal)
		    ENDIF
	    NEXT
	    aFields := t:GetFields(BindingFlags.Instance | BindingFlags.Public)
	    FOREACH f AS FieldInfo IN aFields
		    LOCAL uVal AS USUAL
		    IF ! f:IsSpecialName
			    uVal := f:GetValue(oObject)
			    AAdd(aResult, uVal)
		    ENDIF
    	NEXT
    CATCH e as Exception
        IF e:InnerException != NULL
            THROW Error{e:GetInnerException()}
        ENDIF
        THROW // rethrow exception
    END TRY
   	RETURN aResult
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ooptree/*" />
FUNCTION OOPTree(oObject AS OBJECT) AS ARRAY
    // TreeHelper already checks for NULL_OBJECT    
	RETURN OOPHelpers.TreeHelper(oObject?:GetType())
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ooptreeclass/*" />
FUNCTION OOPTreeClass(symClass AS STRING) AS ARRAY
	VAR type := OOPHelpers.FindClass(symClass)
    // TreeHelper already checks for NULL_OBJECT    
	RETURN OOPHelpers.TreeHelper(type)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/send/*" />
FUNCTION Send(oObject AS USUAL,symMethod AS USUAL, MethodArgList PARAMS USUAL[]) AS USUAL 
	IF !oObject:IsObject
	     THROW Error.VOError( EG_DATATYPE, __FUNCTION__, NAMEOF(oObject), 1, <OBJECT>{ oObject}  )
	ENDIF
	IF ! symMethod:IsString  .AND. ! symMethod:IsSymbol
		THROW Error.VOError( EG_DATATYPE, __FUNCTION__, NAMEOF(symMethod) , 2, <OBJECT>{ symMethod } )
	ENDIF
    IF MethodArgList == NULL
        // this happens for SEND (oObject, "method", NULL)
        MethodArgList := <USUAL>{NULL}
    ENDIF
	LOCAL oToSend := oObject AS OBJECT
	LOCAL cMethod := symMethod AS STRING
	LOCAL uResult AS USUAL
	uResult := OOPHelpers.DoSend(oToSend, cMethod, MethodArgList)
	RETURN uResult
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/send/*" />
FUNCTION CSend(oObject AS OBJECT,symMethod AS STRING, MethodArgList PARAMS USUAL[]) AS USUAL
	RETURN __InternalSend(oObject, symMethod, MethodArgList)
	
	// This is called by the compiler when a late bound call is made on a USUAL.
	// It is strongly typed and more efficient than Send(), which must use the
	// CLIPPER calling convention for compatiblity with VO.
	// Note: Make The first parameter in __InternalSend() in the runtime must be a USUAL!
	//       The compiler expects that
/// <exclude />
FUNCTION __InternalSend( oObject AS USUAL, cMethod AS STRING, args PARAMS USUAL[] ) AS USUAL
	RETURN OOPHelpers.DoSend(oObject, cMethod, args)

/// <summary>Helper function to convert ARRAY to USUAL[]</summary>		
FUNCTION _ArrayToUsualArray (args AS ARRAY) AS USUAL[]
	LOCAL elements AS INT
	LOCAL uargs    AS USUAL[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	uargs    := USUAL[]{ elements }
	
	FOR x := 0 UPTO elements -1
		uargs[x] := args[x]
	NEXT
	RETURN uargs

/// <summary>Helper function to convert ARRAY to OBJECT[]</summary>		
FUNCTION _ArrayToObjectArray (args AS ARRAY) AS OBJECT[]
	LOCAL elements AS INT
	LOCAL oArgs    AS OBJECT[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	oArgs    := OBJECT[]{ elements }
	
	FOR x := 0 UPTO elements -1
		oArgs[x] := args[x]
	NEXT
	RETURN oArgs

/// <summary>Helper function to convert USUAL[] to OBJECT[]</summary>			
FUNCTION _UsualArrayToObjectArray (args AS USUAL[]) AS OBJECT[]
	LOCAL elements AS INT
	LOCAL oArgs    AS OBJECT[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	oArgs    := OBJECT[]{ elements }
	
	FOR x := 0 UPTO elements -1
		oArgs[x] := args[x]
	NEXT
	RETURN oArgs

/// <summary>Helper function to convert OBJECT[] to USUAL[]</summary>		
FUNCTION _ObjectArrayToUsualArray (args AS OBJECT[]) AS USUAL[]
	LOCAL elements AS INT
	LOCAL uArgs    AS USUAL[]
	LOCAL x        AS DWORD
	
	elements := (INT) args:Length
	uArgs    := USUAL[]{ elements }
	
	FOR x := 0 UPTO elements -1
		uArgs[x] := args[x]
	NEXT
	RETURN uArgs
	
/// <exclude/>	
	// identical to CSend and __InternalSend but with a normal array of args
FUNCTION _SendClassParams( oObject AS OBJECT, cmethod AS STRING, args AS ARRAY ) AS USUAL
	LOCAL uArgs AS USUAL[]
	uArgs := _ArrayToUsualArray(args)
	RETURN OOPHelpers.DoSend(oObject, cmethod, uArgs )
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mparamcount/*" />
FUNCTION MParamCount(symClass AS STRING,symMethod AS STRING) AS DWORD
	LOCAL type AS Type
	type := OOPHelpers.FindClass(symClass)	
	IF type != NULL
		LOCAL met AS MethodInfo
		met := OOPHelpers.FindMethod(type, symMethod, TRUE)
		IF met != NULL
			IF met:IsDefined(TYPEOF(ClipperCallingConventionAttribute),FALSE)
				// calculate the # of parameters
				VAR oAttr := (ClipperCallingConventionAttribute) met:GetCustomAttributes(TYPEOF(ClipperCallingConventionAttribute), FALSE):First()
				RETURN (DWORD) oAttr:ParameterNames:Length
			ELSE
				RETURN (DWORD) met:GetParameters():Length
			ENDIF
		ELSE
			THROW Error.VOError( EG_NOMETHOD,  "MParamCount", NAMEOF(symMethod), 2, <OBJECT>{symMethod} )
		ENDIF
	ELSE
		THROW Error.VOError( EG_WRONGCLASS,  "MParamCount", NAMEOF(symClass), 1, <OBJECT>{symClass} )
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
			LOCAL oMI := aFuncs:First() AS MethodInfo
			IF oMI:IsDefined(TYPEOF(ClipperCallingConventionAttribute),FALSE)
				// calculate the # of parameters
				LOCAL oAttr AS ClipperCallingConventionAttribute
				oAttr := (ClipperCallingConventionAttribute) oMI:GetCustomAttributes(TYPEOF(ClipperCallingConventionAttribute), FALSE):First()
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
	RETURN	_CallClipFunc(symFunction, _ArrayToUsualArray(aArgs))

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
			oMI		:= aFuncs:First()
			IF OOPHelpers.SendHelper(NULL, oMI, uArgs, OUT VAR result)
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
	oModule := oAssembly:GetModules():First()
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

