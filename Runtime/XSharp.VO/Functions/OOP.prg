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
function ClassName(o as object) as Symbol
	return AsSymbol(o:GetType():Name)



/// <summary>
/// Get the class hierarchy of an object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function ClassTree(o as object) as Array
	/// THROW NotImplementedException{}
	return null_array   



function CreateInstance(symClassName as Usual) as object
	return null_object   


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
function ClassTreeClass(symClassName as Symbol) as Array
	/// THROW NotImplementedException{}
	return null_array   



/// <summary>
/// Check whether a particular access method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAccessName"></param>
/// <returns>
/// </returns>
function IsAccess(o as object,symAccessName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Check whether a particular assign method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symAssignName"></param>
/// <returns>
/// </returns>
function IsAssign(o as object,symAssignName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Determine if an object is an instance of a class.
/// </summary>
/// <param name="o"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IsInstanceOf(o as object,symClassName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if an object inside a Usual is an instance of a class.
/// </summary>
/// <param name="oX"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IsInstanceOfUsual(oX as Usual,symClassName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false   




/// <summary>
/// Return the contents of an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="symIvar"></param>
/// <returns>
/// </returns>
function IVarGet(o as object,symIvar as Symbol) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Get information about how a particular instance variable (or access method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="symIVar"></param>
/// <returns>
/// </returns>
function IVarGetInfo(o as object,symIVar as Symbol) as dword
	/// THROW NotImplementedException{}
	return 0   


/// <summary>
/// Check whether a particular method can be sent to an object.
/// </summary>
/// <param name="o"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
function IsMethod(o as object,symMethodName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Check whether a particular method can be sent to an object inside a Usual.
/// </summary>
/// <param name="oX"></param>
/// <param name="symMethodName"></param>
/// <returns>
/// </returns>
function IsMethodUsual(oX as Usual,symMethodName as Symbol) as logic
	/// THROW NotImplementedException{}
	return false  


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
/// <param name="symIvar"></param>
/// <returns>
/// </returns>
function IVarGetSelf(o as object,symIvar as Symbol) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// </summary>
/// <param name="o"></param>
/// <param name="symIvar"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IVarGetSuper(o as object,symIvar as Symbol,symClassName as Symbol) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Store all instance variables of an object into an Array.
/// </summary>
/// <param name="pObject"></param>
/// <returns>
/// </returns>
function IvarList(pObject as object) as Array
	/// THROW NotImplementedException{}
	return null_array   

/// <summary>
/// Assign a value to an exported instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="symIvar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function IVarPut(o as object,symIvar as Symbol,u as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Get information about how a particular instance variable (or assign method) was declared.
/// </summary>
/// <param name="o"></param>
/// <param name="symIVar"></param>
/// <returns>
/// </returns>
function IVarPutInfo(o as object,symIVar as Symbol) as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Assign a value to an instance variable.
/// </summary>
/// <param name="o"></param>
/// <param name="symIvar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function IVarPutSelf(o as object,symIvar as Symbol,u as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// </summary>
/// <param name="o"></param>
/// <param name="symIvar"></param>
/// <param name="u"></param>
/// <param name="symClassName"></param>
/// <returns>
/// </returns>
function IVarPutSuper(o as object,symIvar as Symbol,u as Usual,symClassName as Symbol) as Usual
	/// THROW NotImplementedException{}
	return NIL   





/// <summary>
/// Create a class list in the form of an Array for the specified object.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function MethodList(o as object) as Array
	/// THROW NotImplementedException{}
	return null_array   


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
	/// THROW NotImplementedException{}
	return null_array   

/// <summary>
/// Return a multidimensional Array of all object-oriented programming Symbols that constitute the class of an object.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
function OOPTreeClass(s as Symbol) as Array
	/// THROW NotImplementedException{}
	return null_array   




/// <summary>
/// Invoke a method.
/// </summary>
/// <param name="o"></param>
/// <param name="symMethod"></param>
/// <returns>
/// </returns>
function Send(o as Usual,symMethod as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

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

