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
function ErrorBlock(cobError as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   


/// <summary>
/// Call the currently installed error function and create a new error object.
/// </summary>
/// <param name="ptrERRINFO"></param>
/// <returns>
/// </returns>
[Obsolete];
	function ErrorBuild(ptrERRINFO as IntPtr) as ERROR
	/// THROW NotImplementedException{}
return  Error{}

/// <summary>
/// Return the number of errors that have occurred during program execution.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function ErrorCount(dw as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   



/// <summary>
/// Call the currently installed error function and error block.
/// </summary>
/// <param name="ptrERRINFO"></param>
/// <returns>
/// </returns>
[Obsolete];
	function ErrorExec(ptrERRINFO as IntPtr) as __Usual
	/// THROW NotImplementedException{}
return __Usual._NIL   

/// <summary>
/// Install an error function.
/// </summary>
/// <param name="ptrFunc"></param>
/// <returns>
/// </returns>
function ErrorFunc(ptrFunc as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   



/// <summary>
/// Retrieve and optionally set the <%APP%> return code.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function ErrorLevel(dw as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   
