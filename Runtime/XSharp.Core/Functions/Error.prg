//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Return an error message associated with a system-generated error code.
/// </summary>
/// <param name="nGenCode">The error code exported by an error object. One of the GenCodes enum values</param>
/// <returns>The message string associated with the error code.  Error messages are nation-dependent.</returns>
function ErrString(nGenCode as dword) as string
	local cResource as string
	if nGenCode > XSharp.GenCode.EG_MAX
		cResource := "RT_MSG_ERR_UNKNOWN"
	else
		cResource := "RT_MSG_ERR_" + nGenCode:ToString()
	endif
	//Todo Lookup string based on cResourse
	return cResource
	
	
	/// <summary>
	/// Return an error message associated with a system-generated error code.
	/// </summary>
	/// <param name="nGenCode">The error code exported by an error object. One of the GenCodes enum values</param>
	/// <returns>The message string associated with the error code.  Error messages are nation-dependent.</returns>
function ErrString(nGenCode as XSharp.GenCode) as string
	return ErrString( (dword) nGenCode)
	
	
	/// <summary>
	/// Return a description string for a DOS error number.
	/// </summary>
	/// <param name="nDosErr">The DOS error number that you want a description for.</param>
	/// <returns>The message string associated with the error number.</returns>
function DosErrString(nDosErr as dword) as string
	local cResource as string
	cResource := "RT_MSG_DOSERR_" + nDosErr:ToString() 
	//Todo Lookup string based on nDosErr
	// when not found return string from RT_MSG_DOSERR_UNKNOWN
	return cResource



