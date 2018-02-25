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
function ASend(a as __Usual,symMethod as __Usual) as __Array
	/// THROW NotImplementedException{}
	return null_array   

/// <summary>
/// Invoke a method of a specific class for every element in an __Array of objects.
/// </summary>
/// <param name="a"></param>
/// <param name="symMethod"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function ASendClass(a as __Usual,symMethod as __Usual,symClassName as __Usual) as __Array
	/// THROW NotImplementedException{}
	return null_array   



/// <summary>
/// Determine if an object is an instance of a particular class.
/// </summary>
/// <param name="o"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function CheckInstanceOf(o as object,symClassName as __Symbol) as logic
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
/// Return an __Array of __Symbols corresponding to the classes available to your application.
/// </summary>
/// <returns>
/// Returns an __Array with the name of all available classes
/// </returns>
function ClassList() as __Array
	local classes    := __Array{} as __Array
	local assemblies := System.AppDomain.CurrentDomain:GetAssemblies() as System.Reflection.Assembly[]
	foreach assembly as System.Reflection.Assembly in assemblies
		local types := assembly:GetTypes() as System.Type[]
		foreach type as System.Type in types
			classes:Add(type:Name)
		next
	next
	return classes

/// <summary>
/// Get the class name of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function ClassName(o as object) as __Symbol
	return AsSymbol(o:GetType():Name)



/// <summary>
/// Get the class hierarchy of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function ClassTree(o as object) as __Array
	/// THROW NotImplementedException{}
	return null_array   



function CreateInstance(symClassName as __Usual) as object
	return null_object   


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <param name="symMethod"></param>
/// <returns>
/// </returns>
function CSend(o as __Usual,symMethod as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// Get the class hierarchy of a class.
/// </summary>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function ClassTreeClass(symClassName as __Symbol) as __Array
	/// THROW NotImplementedException{}
	return null_array   



/// <summary>
/// Check whether a particular access method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAccessName"></param>
/// <returns>
/// </returns>
function IsAccess(o as object,symAccessName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Check whether a particular assign method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAssignName"></param>
/// <returns>
/// </returns>
function IsAssign(o as object,symAssignName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Determine if an object is an instance of a class.
/// </summary>
/// <param name="o"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IsInstanceOf(o as object,symClassName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if an object inside a __Usual is an instance of a class.
/// </summary>
/// <param name="oX"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IsInstanceOfUsual(oX as __Usual,symClassName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false   




	/// <summary>
	/// Return the contents of an exported instance variable.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarGet(o AS OBJECT,symIvar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Get information about how a particular instance variable (or access method) was declared.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarGetInfo(o AS OBJECT,symIVar AS __Symbol) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   


/// <summary>
/// Check whether a particular method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
function IsMethod(o as object,symMethodName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Check whether a particular method can be sent to an object inside a __Usual.
/// </summary>
/// <param name="oX"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
function IsMethodUsual(oX as __Usual,symMethodName as __Symbol) as logic
	/// THROW NotImplementedException{}
	return false  


	/// <summary>
	/// Return the contents of an instance variable.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarGetSelf(o AS OBJECT,symIvar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarGetSuper(o AS OBJECT,symIvar AS __Symbol,symClassName AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	return __Usual._NIL   

	/// <summary>
	/// Store all instance variables of an object into an __Array.
	/// </summary>
	/// <param name="pObject"></param>
	/// <returns>
	/// </returns>
	FUNCTION IvarList(pObject AS OBJECT) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Assign a value to an exported instance variable.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarPut(o AS OBJECT,symIvar AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	return __Usual._NIL   

	/// <summary>
	/// Get information about how a particular instance variable (or assign method) was declared.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarPutInfo(o AS OBJECT,symIVar AS __Symbol) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Assign a value to an instance variable.
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarPutSelf(o AS OBJECT,symIvar AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIvar"></param>
	/// <param name="u"></param>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IVarPutSuper(o AS OBJECT,symIvar AS __Symbol,u AS __Usual,symClassName AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   





	/// <summary>
	/// Create a class list in the form of an __Array for the specified object.
	/// </summary>
	/// <param name="o"></param>
	/// <returns>
	/// </returns>
	FUNCTION MethodList(o AS OBJECT) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// Identify an invalid method call.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION NoMethod() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   


	/// <summary>
	/// Return a multidimensional __Array of all object-oriented programming __Symbols that constitute the class of an object.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION OOPTreeClass(s AS __Symbol) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   
