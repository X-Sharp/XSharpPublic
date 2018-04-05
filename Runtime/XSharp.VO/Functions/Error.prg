//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Return and optionally change the code block that is executed when a runtime error occurs.
/// </summary>
/// <param name="cobError"></param>
/// <returns>
/// </returns>
function ErrorBlock(cobError as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Return the number of errors that have occurred during program execution.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function ErrorCount(dw as Usual) as dword
	/// THROW NotImplementedException{}
	return 0   



/// <summary>
/// Install an error function.
/// </summary>
/// <param name="ptrFunc"></param>
/// <returns>
/// </returns>
function ErrorFunc(ptrFunc as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   



/// <summary>
/// Retrieve and optionally set the <%APP%> return code.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function ErrorLevel(dw as Usual) as dword
	/// THROW NotImplementedException{}
	return 0   
