//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Perform an assignment to a variable whose name is stored in a specified string.
/// </summary>
/// <param name="cExp"></param>
/// <param name="xValue"></param>
/// <returns>
/// </returns>
function MAssign(cExp as string,xValue as Usual) as Usual
	throw NotImplementedException{}
return	 NIL   



/// <summary>
/// Return a set-get code block for a given memory variable.
/// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
function MemVarBlock(cVar as string) as object
	throw NotImplementedException{}
return	 null_object   

/// <summary>
/// Return the contents of a memory variable.
/// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
function MemVarGet(cVar as string) as Usual
	throw NotImplementedException{}
return	 NIL   

/// <summary>
/// Assign a value to a memory variable of a given name.
/// </summary>
/// <param name="cVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function MemVarPut(cVar as string,u as Usual) as Usual
	throw NotImplementedException{}
return	 NIL   





/// <summary>
/// Return the contents of a field or a memory variable.
/// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
function VarGet(cVar as string) as Usual
	throw NotImplementedException{}
return	 NIL   

/// <summary>
/// Assign a value to a field or a memory variable of a given name.
/// </summary>
/// <param name="cVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function VarPut(cVar as string,u as Usual) as Usual
	throw NotImplementedException{}
return	 NIL   





/// <summary>
/// </summary>
/// <param name="symVar"></param>
/// <returns>
/// </returns>
function VarGetSym(symVar as Symbol) as Usual
	THROW NotImplementedException{}
	return NIL   

/// <summary>
/// </summary>
/// <param name="symVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function VarPutSym(symVar as Symbol,u as Usual) as Usual
	THROW NotImplementedException{}
	return NIL  

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
		THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarPutSym(symVar AS Symbol,u AS Usual) AS Usual
		THROW NotImplementedException{}
	RETURN NIL      
