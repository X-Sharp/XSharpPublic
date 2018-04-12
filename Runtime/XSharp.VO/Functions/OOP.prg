//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <summary>
/// Invoke a method for every element in an Array of objects.
/// </summary>
/// <param name="a"></param>
/// <param name="symMethod"></param>
/// <returns>
/// </returns>
using XSharp
using XSharp.Internal
using System.Reflection
using System.Collections.Generic

static class OOPHelpers
	static method FindClass(cName as string) as System.Type
	// TOdo Optimize
	LOCAL loadedAssemblies := AppDomain.CurrentDomain:GetAssemblies() AS Assembly[]
    local ret := NULL as System.Type
	FOREACH asm as Assembly in loadedAssemblies
		ret := asm:GetType( cName, false, true )
		if ret != null
			exit
		endif
		IF asm:IsDefined( typeof( ClassLibraryAttribute ), FALSE )
			ret := asm:GetType( cName, false, true )
			IF ret != NULL
	            EXIT
			ENDIF
         // The class could be prefixed with a Namespace. 
         // If there is an Implicoit Namespace Attribute
         // We don't know if the current assembly is compiler with /INS, but we assume it is when they
         // use the 'old fashioned' CreateInstance().
		 var ins := typeof( ImplicitNamespaceAttribute )
         IF asm:IsDefined(  ins, FALSE )
            var atr := (ImplicitNamespaceAttribute) (asm:GetCustomAttributes(ins,FALSE) [1])
            var cFullName := atr:Namespace +"."+cName
            ret := asm:GetType( cFullName, FALSE, TRUE )
            IF ret != NULL
               EXIT
            ENDIF
         ENDIF
      ENDIF
    NEXT   
    RETURN ret
	static method IsMethod( t AS System.Type, cName AS STRING ) AS LOGIC
	   LOCAL lReturn AS LOGIC
   
	   TRY
		  lReturn := t:GetMethod(cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public ) != NULL
	   CATCH AS System.Reflection.AmbiguousMatchException
		  lReturn := TRUE
	   END TRY
   
	   RETURN lReturn
	static method ClassTree( t AS Type ) AS ARRAY   
	   LOCAL aList := {} AS ARRAY
	   DO WHILE t != NULL
		  AAdd( aList, (SYMBOL) t:Name)
		  t := t:BaseType
	   ENDDO
   
	   return aList

	static method IVarHelper(o as object, cName as string, lGet as logic) as DWORD
   
	   IF o == NULL
		  RETURN 0
	   ENDIF

	   var t := o:GetType()
   
	   var fi := t:GetField( cName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic  | BindingFlags.IgnoreCase)
	   IF fi != NULL
		  IF fi:IsPublic
			 RETURN 2U
		  ELSEIF fi:IsFamily 
			 RETURN 1U
		  ENDIF
	   ENDIF

	   DO WHILE t != NULL
		  var pi := t:GetProperty( cName , BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
		  IF pi != NULL .and. ( (lGet .and. pi:CanRead) .or. (.not. lGet .and. pi:CanWrite) )
			 RETURN 3U
		  ELSE
			 t := t:BaseType
		  ENDIF
	   ENDDO

	   return 0U

	STATIC METHOD IVarList( t AS Type ) AS ARRAY
	   LOCAL aList   AS ARRAY
	   aList := {}
	   IF t == NULL
		  RETURN aList
	   ENDIF
   
	   var aFields := t:GetFields( BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic )
	   var nLen    := aFields:Length
	   var list := List<String>{}
	   FOREACH fi as FieldInfo in aFields
		  if fi:IsPublic || fi:IsFamily 
			var name := fi:Name:ToUpper()
			 if ! list:Contains(name)
				list:Add(name)
			 ENDIF
		  ENDIF
	   NEXT

	   var aProps := t:GetProperties( BindingFlags.Instance | BindingFlags.Public )
   
	   FOREACH pi as PropertyInfo in aProps
			var name := pi:Name:ToUpper()
			 if ! list:Contains(name)
				list:Add(name)
			 ENDIF
	   NEXT
	   foreach var name in List
			aadd(aList, String2Symbol(name))
	   next
	   RETURN aList
   

	   static method MethodList(t as Type) as ARRAY
		   var list := List<String>{}
		   var aInfo := t:GetMethods( BindingFlags.Instance | BindingFlags.Public )
		   foreach oMI as MethodInfo in aInfo
			  if !oMI:IsSpecialName
				  IF ! list:Contains(oMI:Name)
					 list:Add(oMI:Name )
				  endif   
			endif
		   next
		   local aList as array
		   aList := {}
		   foreach cMethod as string in list
				aadd(aList, String2Symbol(cMethod))
		   next
		   return aList
	STATIC METHOD TreeHelper( t AS Type ) AS ARRAY 
	   LOCAL aInstance , aMethod AS ARRAY
	   LOCAL aList := NULL_ARRAY AS ARRAY
	   IF t == NULL
   		  RETURN aList
	   END IF
   
	   var aInheritance := List<Type>{}
	   DO WHILE t != NULL
   		  aInheritance:Add(t)
   		  t := t:BaseType
	   END DO
   
	   FOREACH type as Type in aInheritance
   		  aInstance := {}
   		  aMethod := {}
   		  AAdd(aList , {(SYMBOL) type:FullName, aInstance, aMethod})
		  var listMethod := List<string>{}
		  var listVar    := List<String>{}
	   
   		  var aInfo := type:GetMembers(BindingFlags.Instance + BindingFlags.Public + BindingFlags.NonPublic)
   		  FOREACH oInfo as MemberInfo in aInfo
			var name := oInfo:Name:Toupper()
			 DO CASE
			 case oInfo:MemberType == MemberTypes.Field
         		IF listVar:IndexOf(name)  == -1 .and. ((FieldInfo)oInfo):IsPublic
         			listVar:Add(name)
         		END IF
			 CASE oInfo:MemberType == MemberTypes.Property
         		IF listVar:IndexOf(name)  == -1
         			listVar:Add(name)
         		END IF
			 CASE oInfo:MemberType == MemberTypes.Method
         		IF listMethod:IndexOf(name)  == -1 .and. .not. ((MethodInfo)oInfo):IsSpecialName
         			listMethod:Add(name)
         		END IF
			 END CASE
   		  next
	  	   foreach var name in listVar
			AADD(aInstance, String2Symbol(name))
		   next   
	  	   foreach var name in listMethod
			AADD(aMethod, String2Symbol(name))
		   next   

	   NEXT
	   return aList
	   static method FindProperty( t as Type , cName as STRING, lAccess as LOGIC) as PropertyInfo
		   DO WHILE t != NULL
			  var oInfo := t:GetProperty( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public  | BindingFlags.DeclaredOnly ) 
			  IF oInfo != NULL .and. ( (lAccess .and. oInfo:CanRead) .or. (.not. lAccess .and. oInfo:CanWrite) )
				 return oInfo
			  ELSE
				 t := t:BaseType
			  ENDIF
		   ENDDO
		   return null

	   static method FindField( t as Type, cName as STRING, lAccess as LOGIC ) as FieldInfo
		   DO WHILE t != NULL
			  var oInfo := t:GetField( cName, BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.Public  | BindingFlags.DeclaredOnly ) 
			  if oInfo != null 
			    // check for readonly (initonly) fields
			    if lAccess .or. ! oInfo:Attributes:HasFlag(FieldAttributes.InitOnly)
					return oInfo
				endif
			  ELSE
				 t := t:BaseType
			  ENDIF
		   ENDDO
		   return NULL
	
	static method IsFieldVisible(oFld as FieldInfo, lSelf as logic)
		if oFld == NULL_OBJECT
			return false
		elseif oFld:IsPublic
			return true
		elseif lSelf .and. (oFld:IsFamily .or. oFld:IsFamilyOrAssembly)
			return true
		ENDIF
		return false
			
			

	static method IVarGet(oObject as object, cIVarName as string, lSelf as logic) as object
		local t as Type
		t := oObject:GetType()
		//Todo: optimization
		var fldInfo := FindField(t, cIVarName,TRUE)
		if fldInfo != NULL_OBJECT
			if !IsFieldVisible(fldInfo, lSelf)
				// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )
			endif
			return fldInfo:GetValue(oObject)
		endif
		var propInfo := FindProperty(t, cIVarName,TRUE)
		if propInfo != NULL_OBJECT
			if !propInfo:CanRead
				// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )
			endif
			return propInfo:GetValue(oObject, NULL)
		endif
		// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )
		return NULL_OBJECT

	static method IVarPut(oObject as object, cIVarName as string, oValue as OBJECT, lSelf as LOGIC)  as void
		local t as Type
		t := oObject:GetType()
		var fldInfo := FindField(t, cIVarName,FALSE)
		//Todo: optimization
		if fldInfo != NULL_OBJECT
			if !IsFieldVisible(fldInfo, lSelf)
			// todo
				// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )
			endif
			fldInfo:SetValue(oObject, oValue)
			return
		endif
		var propInfo := FindProperty(t, cIVarName, lSelf)
		if propInfo != NULL_OBJECT
			if !propInfo:CanWrite
			// todo
				// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )
			endif
			propInfo:SetValue(oObject, oValue, null)
			return
		endif
			// todo
		// Throw Error.VOError( EG_NOVARMETHOD, IIF( isSelf, "IVarGetSelf", "IVarGet" ), "cName", 2, { cName } )

	static method SendHelper(oObject as object, cMethod as string, uArgs as usual[], result out usual) as logic
		local t := oObject?:GetType() as Type
		result := NIL
		if t == null
			// todo
			// throw an error
			return false
		endif
		local mi as MethodInfo
		mi := t:GetMethod(cMethod,BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
		if mi == NULL
			// todo
			// throw an error
			return false
		endif	
		local paramInfo as ParameterInfo[]
		paramInfo := mi:GetParameters()
		// Clipper calling convention ?
		local lClipper := false as logic
		if paramInfo:Length == 1 .and. mi:IsDefined( typeof( ClipperCallingConventionAttribute ), false )
			lClipper := true
		endif
		local oArgs as Object[]
		if lClipper
			oArgs := <Object> {uArgs}
		elseif paramInfo:Length > 0
			oArgs := object[]{ paramInfo:Length }
			local nPar as INT
			for nPar := 1 to paramInfo:Length
				local pi := paramInfo[nPar] as ParameterInfo
				if nPar <= uArgs:Length
					local arg := uArgs[nPar] as USUAL
					if pi:ParameterType == typeof(USUAL)
						// We need to box a usual here 
						var oTemp := object[]{ 1 }
						oTemp[1] := arg
						oArgs[nPar] := Activator.CreateInstance(typeof(Xsharp.__Usual), oTemp)
					elseif pi:ParameterType:IsAssignableFrom(arg:SystemType) .or. arg == null
						oArgs[nPar] := uArgs[nPar]
					elseif pi:GetCustomAttributes( typeof( ParamArrayAttribute ), false ):Length > 0
						// Parameter array of certain type
						// -> convert remaining elements from uArgs to an array and assign that to oArgs[i] 
						local elementType := pi:ParameterType:GetElementType() as System.Type
						local aVarArgs := System.Array.CreateInstance(elementType, uArgs:Length - nPar +1) as System.Array
						local nArg as INT
						for nArg := nPar to uArgs:Length
							try
								if elementType:IsAssignableFrom(uArgs[nArg]:SystemType)
									aVarArgs:SetValue(uArgs[nArg], nArg-nPar)
								else
									aVarArgs:SetValue(MyConvert(uArgs[nArg], elementType), nArg-nPar)
								endif
							catch
								aVarArgs:SetValue(null, nArg-nPar)
							end try
						next
						oArgs[nPar] := aVarArgs
						exit	// parameter loop
					else	// try to convert to the expected type
						oArgs[nPar]  := MyConvert(uArgs[nPar], pi:ParameterType)
					endif
				endif 
			next
		else
			oArgs := Null
		endif
		if mi != null
			result := mi:Invoke(oObject, oArgs)
		endif
		return true

		static method MyConvert(uValue as usual,toType as System.type) as object
			if toType == typeof(float)
				return (Float) uValue
			else
				return Convert.ChangeType((OBJECT) uValue, toType)
			endif

end class



	

function ASend(a as Usual,symMethod as Usual) as Array
	/// THROW NotImplementedException{}
	return null_array   

/// <summary>
/// Invoke a method of a specific class for every element in an Array of objects.
/// </summary>
/// <param name="a"></param>
/// <param name="symMethod"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function ASendClass(a as Usual,symMethod as Usual,symClassName as Usual) as Array
	/// THROW NotImplementedException{}
	return null_array   



/// <summary>
/// Determine if an object is an instance of a particular class.
/// </summary>
/// <param name="o"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function CheckInstanceOf(o as object,symClassName as Symbol) as logic
	return o:GetType():Name == symClassName

/// <summary>
/// Return the number of classes available to your application.
/// </summary>
/// <returns>
/// The number of available classes.
/// </returns>
function ClassCount() as dword
	return ClassList():Length

/// <summary>
/// Return an Array of Symbols corresponding to the classes available to your application.
/// </summary>
/// <returns>
/// Returns an Array with the name of all available classes
/// </returns>
function ClassList() as Array
	local classes    := Array{} as Array
	local assemblies := System.AppDomain.CurrentDomain:GetAssemblies() as System.Reflection.Assembly[]
	foreach assembly as System.Reflection.Assembly in assemblies
		local types := assembly:GetTypes() as System.Type[]
		foreach type as System.Type in types
			if type:IsPublic
				classes:Add(String2Symbol(type:Name))
			endif
		next
	next
	return classes

/// <summary>
/// Get the class name of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function ClassName(o as object) as STRING
	return o?:GetType():Name:ToUpper()



/// <summary>
/// Get the class hierarchy of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function ClassTree(o as object) as Array
	return OOPHelpers.ClassTree(o?:GetType())


function CreateInstance(cClassName) as object
   IF ! ( cClassName:IsSymbol || cClassName:IsString )
      BREAK DataTypeError( "CreateInstance", nameof(cClassName), 1)
   endif    	
   var t := OOPHelpers.FindClass(cClassName)
   if t == null
	// todo Throw an error
   endif
   var constructors := t:getConstructors() 
   if constructors:Length > 1
	// todo Throw an error overloaded constructors
   endif
   local ctor := constructors[1] as ConstructorInfo
   local nPCount as int
   local nArg as int
   nPCount := PCount()
   local oRet as OBJECT  
   try
	 if ctor:IsDefined(typeof(XSharp.Internal.ClipperCallingconventionAttribute),False)
	   local args as usual[]
	   args := Usual[]{nPCount-1}
	   for nArg := 2 to nPCount
		  args[nArg-1] := _GetFParam(nArg)
	   next 
		oRet := ctor:Invoke( <object>{args})	
	 elseif ctor:IsDefined(typeof( System.Runtime.CompilerServices.CompilerGeneratedAttribute ), false )
		// generated default ctor without args
		oRet := ctor:Invoke( NULL)	
	 else
		// convert args to array of objects
	   local args as object[]
	   args := object[]{nPCount-1}
	   for nArg := 2 to nPCount
		  args[nArg-1] := _GetFParam(nArg)
	   next 
	   oRet := ctor:Invoke( <object>{args})	
	 endif
   catch
	// Throw an error
	oRet := NULL_OBJECT
   end try
   return oRet

/// <summary>
/// </summary>
/// <param name="o"></param>
/// <param name="symMethod"></param>
/// <returns>
/// </returns>
function CSend(o as Usual,symMethod as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Get the class hierarchy of a class.
/// </summary>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function ClassTreeClass(cName as STRING) as Array
	var t := OOPHelpers.FindClass(cName)
	if t != null
		return OOPHelpers.ClassTree(t)
	else
      Throw Error{EG_NOCLASS}
	endif
	return null_array   



/// <summary>
/// Check whether a particular access method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
function IsAccess(o as object,cName as STRING) as logic
	var oprop := OOPHelpers.FindProperty(o?:GetType(), cName, FALSE)
	if oProp != null_object
		return oProp:CanRead
	endif
	return false
/// <summary>
/// Check whether a particular assign method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAssignName"></param>
/// <returns>
/// </returns>
function IsAssign(o as object,cName as STRING) as logic
	var oprop := OOPHelpers.FindProperty(o?:GetType(), cName, FALSE)
	if oProp != null_object
		return oProp:CanWrite
	endif
	return false

/// <summary>
/// Determine if a class exists.
/// </summary>
/// <param name="cClassName"></param>
/// <returns>
/// </returns>
FUNCTION IsClass(cClassName as STRING) AS LOGIC
		
RETURN OOPHelpers.FindClass(cClassName) != null

/// <summary>
/// Determine if one class is a subclass of another class.
/// </summary>
/// <param name="symClassName"></param>
/// <param name="symSuperClassName"></param>
/// <returns>
/// </returns>
FUNCTION IsClassOf(cClassName AS STRING,cSuperClassName AS STRING) AS LOGIC
	local tSub   := OOPHelpers.FindClass(cClassName) as Type
	local tSuper := OOPHelpers.FindClass(cSuperClassName) as Type
	return tSub != NULL .and. tSuper != NULL .and. tSub:IsSubclassOf(tSuper)



/// <summary>
/// Determine if an object is an instance of a class.
/// </summary>
/// <param name="oObject"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
function IsInstanceOf(oObject as object,cName as STRING) as logic
   IF String.IsNullOrEmpty( cName )
      THROW Error.NullArgumentError( "IsInstanceOf", nameof(cName), 2 )
   ENDIF

   local ret := FALSE as LOGIC
   IF oObject != NULL
      var t := oObject:GetType()
      
      DO WHILE t != typeof( OBJECT )
         IF String.Compare( t:Name, cName, StringComparison.OrdinalIgnoreCase ) == 0
            ret := TRUE
            EXIT
         ENDIF
         t := t:BaseType
      ENDDO
   ENDIF
   return ret


/// <summary>
/// Determine if an object inside a Usual is an instance of a class.
/// </summary>
/// <param name="oX"></param>
/// <param name="cName"></param>
/// <returns>
/// </returns>
function IsInstanceOfUsual(oX as Usual,cName as STRING) as logic
	return IsInstanceOf(oX, cName)



/// <summary>
/// Return the contents of an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIvar"></param>
/// <returns>
/// </returns>
function IVarGet(o as object,cIvar as STRING) as Usual
	if o == NULL_OBJECT
		throw Error.NullArgumentError(__ENTITY__, nameof(o),1)
	endif
	if String.IsNullOrEmpty(cIVar)
		throw Error.NullArgumentError(__ENTITY__, nameof(cIVar),2)
	endif
	return OOPHelpers.IVarGet(o, cIVar, FALSE)

/// <summary>
/// Get information about how a particular instance variable (or access method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
function IVarGetInfo(o as object,cIVar as STRING) as dword
	return OOPHelpers.IVarHelper(o, cIVar, TRUE)


/// <summary>
/// Check whether a particular method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="cMethodName"></param>
/// <returns>
/// </returns>
function IsMethod(o as object,cMethodName as String) as logic
	return OOPHelpers.IsMethod(o?:GetType(), cMethodName)


/// <summary>
/// Check whether a particular method can be sent to an object inside a Usual.
/// </summary>
/// <param name="oX"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
function IsMethodUsual(oX as usual,cName as string) as logic
	if oX:IsObject
		return IsMethod( oX, cName )
	endif
	return false

FUNCTION IsMethodClass( c AS STRING, cName AS STRING ) AS LOGIC
   var t := OOPHelpers.FindClass( c )
   
   IF t != NULL
      return OOPHelpers.IsMethod( t, cName )
   ENDIF
   RETURN false

/// <summary>
/// Determine if the given object is a valid VO Object.
/// </summary>
/// <param name="oObject"></param>
/// <returns>
/// </returns>
function IsVOObject(oObject as object) as logic
	/// THROW NotImplementedException{}
	return false   


/// <summary>
/// Return the contents of an instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
function IVarGetSelf(o as object,cIVar as String) as Usual
	if o == NULL_OBJECT
		throw Error.NullArgumentError(__ENTITY__, nameof(o),1)
	endif
	if String.IsNullOrEmpty(cIVar)
		throw Error.NullArgumentError(__ENTITY__, nameof(cIVar),2)
	endif
	return OOPHelpers.IVarGet(o, cIVar, TRUE)

/// <summary>
/// Store all instance variables of an object into an Array.
/// </summary>
/// <param name="oObject"></param>
/// <returns>
/// </returns>
function IvarList(oObject as object) as Array
	return OOPHelpers.IVarList(oObject?:GetType())


/// <summary>
/// Store all instance variables of a type into an Array.
/// </summary>
/// <param name="oObject"></param>
/// <returns>
/// </returns>
function IvarListClass(cName as string) as array
	var t := OOPHelpers.FindClass(cName)
	return OOPHelpers.IVarList(t)


/// <summary>
/// Get information about how a particular instance variable (or assign method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <returns>
/// </returns>
function IVarPutInfo(o as object,cIVar as Symbol) as dword
	return OOPHelpers.IVarHelper(o, cIVar, FALSE)
 
 /// <summary>
/// Assign a value to an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIVar"></param>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function IVarPut(o as object,cIVar as STRING,uValue as Usual) as Usual
	if o == NULL_OBJECT
		throw Error.NullArgumentError(__ENTITY__, nameof(o),1)
	endif
	if String.IsNullOrEmpty(cIVar)
		throw Error.NullArgumentError(__ENTITY__, nameof(cIVar),2)
	endif
	OOPHelpers.IVarPut(o, cIVar, uValue, false)
	return uValue

/// <summary>
/// Assign a value to an instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="cIvar"></param>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function IVarPutSelf(o as object,cIVar as STRING,uValue as Usual) as Usual
	if o == NULL_OBJECT
		throw Error.NullArgumentError(__ENTITY__, nameof(o),1)
	endif
	if String.IsNullOrEmpty(cIVar)
		throw Error.NullArgumentError(__ENTITY__, nameof(cIVar),2)
	endif
	OOPHelpers.IVarPut(o, cIVar, uValue,true) 
	return uValue


/// <summary>
/// Create a class list in the form of an Array for the specified object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function MethodList(o as object) as array
	if o != null
		return OOPHelpers.MethodList( o:GetType() )
	endif
	return NULL_ARRAY

FUNCTION MethodListClass( c AS STRING ) AS ARRAY
   LOCAL aReturn AS ARRAY
   var t := OOPHelpers.FindClass( c )
   IF t != NULL
      aReturn := OOpHelpers.MethodList( t )
	ELSE
	 aReturn  := NULL_ARRAY
   ENDIF

   RETURN aReturn



/// <summary>
/// Identify an invalid method call.
/// </summary>
/// <returns>
/// </returns>
function NoMethod() as Symbol
	/// THROW NotImplementedException{}
	return null_symbol   



/// <summary>
/// Convert the values of an object's instance variables to an Array.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function Object2Array(o as object) as Array
	/// THROW NotImplementedException{}
	return null_array   


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function OClone(o as object) as object
	/// THROW NotImplementedException{}
	return null_object   


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function OMemSize(o as object) as dword
	/// THROW NotImplementedException{}
	return 0  

/// <summary>
/// Return a multidimensional Array of all object-oriented programming Symbols that constitute the class.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function OOPTree(o as object) as Array
	return OOPHelpers.TreeHelper(o?:GetType())

/// <summary>
/// Return a multidimensional Array of all object-oriented programming Symbols that constitute the class of an object.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
function OOPTreeClass(cClass as string) as array
	var type := OOPHelpers.FindClass(cClass)
	return OOPHelpers.TreeHelper(type)





/// <summary>
/// Invoke a method.
/// </summary>
/// <param name="o"></param>
/// <param name="uMethod "></param>
/// <returns>
/// </returns>
function Send(o ,uMethod ) as usual
	local args as USUAL[]
	if !o:IsObject
		// todo
		// throw an error
	endif
	if ! uMethod :IsString  && ! uMethod :IsSymbol
		// todo
		// throw an error
	endif
	local oObject := o as object
	local cMethod := uMethod as STRING
	local nArgs   := PCount() - 2 as int
	local nArg  as int
	args := usual[]{nArgs}
	for nArg := 1 to nArgs
		args[nArg] := _GetFParam(nArg+2)
	next
	return __DoSend(oObject, cMethod, args)

// This is called by the compiler when a late bound call is made on a USUAL.
// It is strongly typed and more efficient than Send(), which must use the
// CLIPPER calling convention for compatiblity with VO.
FUNCTION __InternalSend( oObject AS USUAL, cMethod AS STRING, args params USUAL[] ) AS USUAL
   return __DoSend(oObject, cMethod, args)

internal function __DoSend(oObject as object, cMethod as string, args as usual[] ) as usual
	local result as USUAL
	if ! OOPHelpers.SendHelper(oObject, cMethod, args, out result)
		  LOCAL nomethodArgs := USUAL[]{ args:Length + 1 } AS USUAL[]
		  cMethod := cMethod:ToUpperInvariant()
		  //Todo Add Nomethod to RuntimeState
		  //RuntimeState.NoMethod := cMethod   // For NoMethod() function
		  noMethodArgs[1]		:= cMethod
		  Array.Copy( args, 0, noMethodArgs, 1, args:Length )
		  if ! OOPHelpers.SendHelper(oObject, "NoMethod" , noMethodArgs, out result)
			// Throw Exception
		  endif
	endif
	return result

/// <summary>
/// Invoke a method with a specified class.
/// </summary>
/// <param name="o"></param>
/// <param name="symMethod"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function SendClass(o as Usual,symMethod as Usual,symClassName as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

