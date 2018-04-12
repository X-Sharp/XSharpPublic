//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
using XSharp


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function ArgCount() as dword
	/// THROW NotImplementedException{}
	return 0   


	/// <summary>
	/// </summary>
	/// <param name="symClass"></param>
	/// <param name="symMeth"></param>
	/// <param name="nType"></param>
	/// <param name="pFunc"></param>
	/// <param name="nArgs"></param>
	/// <returns>
	/// </returns>
 	unsafe FUNCTION DeclareMethod(symClass AS Symbol,symMeth AS Symbol,nType AS DWORD,pFunc AS PTR,nArgs AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the number of local arguments that a function with the CLIPPER calling convention is expecting.
	/// </summary>
	/// <param name="symFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION FParamCount(symFunc AS Symbol) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="symFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FunctionSym2Ptr(symFunc AS Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero


	/// <summary>
	/// Check whether a particular method can be sent to a class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsMethodClass(symClassName AS Symbol,symMethodName AS Symbol) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Store all instance variables of a class into an Array.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IvarListClass(symClassName AS Symbol) AS Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Obtain a set-get code block for a given memory variable.
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarBlockSym(symVar AS Symbol) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarGetSym(symVar AS Symbol) AS Usual
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarPutSym(symVar AS Symbol,u AS Usual) AS Usual
		/// THROW NotImplementedException{}
	RETURN NIL      

	/// <summary>
	/// Create a class list in the form of an Array for the specified class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION MethodListClass(symClassName AS Symbol) AS Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Return the number of arguments that a method is expecting.
	/// </summary>
	/// <param name="symClass"></param>
	/// <param name="symMethod"></param>
	/// <returns>
	/// </returns>
	FUNCTION MParamCount(symClass AS Symbol,symMethod AS Symbol) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   



	/// <summary>
	/// </summary>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION UnDeclareClass(symClass AS Symbol) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   
 



	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FunctionCount() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FunctionList() AS Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   
