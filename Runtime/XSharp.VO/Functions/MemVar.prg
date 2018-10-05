//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>This function is not implemented yet</summary>
// <summary>
// Perform an assignment to a variable whose name is stored in a specified string.
// </summary>
/// <param name="cExp"></param>
/// <param name="xValue"></param>
/// <returns>
/// </returns>
FUNCTION MAssign(cExp AS STRING,xValue AS USUAL) AS USUAL
	THROW NotImplementedException{}
RETURN	 NIL   



/// <summary>This function is not implemented yet</summary>
// <summary>
// Return a set-get code block for a given memory variable.
// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
FUNCTION MemVarBlock(cVar AS STRING) AS OBJECT
	THROW NotImplementedException{}
RETURN	 NULL_OBJECT   

/// <summary>This function is not implemented yet</summary>
// <summary>
// Return the contents of a memory variable.
// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
FUNCTION MemVarGet(cVar AS STRING) AS USUAL
	THROW NotImplementedException{}
RETURN	 NIL   

/// <summary>This function is not implemented yet</summary>
// <summary>
// Assign a value to a memory variable of a given name.
// </summary>
/// <param name="cVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION MemVarPut(cVar AS STRING,u AS USUAL) AS USUAL
	THROW NotImplementedException{}
RETURN	 NIL   




/// <summary>This function is not implemented yet</summary>
// <summary>
// Return the contents of a field or a memory variable.
// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
FUNCTION VarGet(cVar AS STRING) AS USUAL
	THROW NotImplementedException{}
RETURN	 NIL   

/// <summary>This function is not implemented yet</summary>
// <summary>
// Assign a value to a field or a memory variable of a given name.
// </summary>
/// <param name="cVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION VarPut(cVar AS STRING,u AS USUAL) AS USUAL
	THROW NotImplementedException{}
RETURN	 NIL   




/// <summary>This function is not implemented yet</summary>
// <summary>
// </summary>
// <param name="symVar"></param>
/// <returns>
/// </returns>
FUNCTION VarGetSym(symVar AS SYMBOL) AS USUAL
	THROW NotImplementedException{}
	RETURN NIL   

/// <summary>This function is not implemented yet</summary>
// <summary>
// </summary>
// <param name="symVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION VarPutSym(symVar AS SYMBOL,u AS USUAL) AS USUAL
	THROW NotImplementedException{}
	RETURN NIL  

/// <summary>This function is not implemented yet</summary>

	// <summary>
	// Obtain a set-get code block for a given memory variable.
	// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarBlockSym(symVar AS SYMBOL) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

/// <summary>This function is not implemented yet</summary>

	// <param name="symVar"></param>
	/// <returns> 
	/// </returns>
	FUNCTION MemVarGetSym(symVar AS SYMBOL) AS USUAL
		THROW NotImplementedException{}
	RETURN NIL   

/// <summary>This function is not implemented yet</summary>

	// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarPutSym(symVar AS SYMBOL,u AS USUAL) AS USUAL
		THROW NotImplementedException{}
	RETURN NIL      
