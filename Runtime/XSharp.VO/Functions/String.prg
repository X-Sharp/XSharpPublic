//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Execute a code block for each of the individual characters in a string.
/// </summary>
/// <param name="c"></param>
/// <param name="cod"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function SEval(c as usual,cod as usual,nStart as usual,nCount as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Execute a code block for each of the individual characters in a string, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="c"></param>
/// <param name="cod"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function SEvalA(c as usual,cod as usual,nStart as usual,nCount as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   




/// <summary>
/// Search and replace characters within a string.
/// </summary>
/// <param name="c"></param>
/// <param name="cSearch"></param>
/// <param name="cReplace"></param>
/// <param name="iStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function StrTran(c as usual,cSearch as usual,cReplace as usual,iStart as usual,nCount as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="iStart"></param>
/// <param name="wLen"></param>
/// <returns>
/// </returns>
function SubS(c as usual,iStart as usual,wLen as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Extract a substring from a string.
/// </summary>
/// <param name="c"></param>
/// <param name="iStart"></param>
/// <param name="wLen"></param>
/// <returns>
/// </returns>
function SubStr(c as usual,iStart as usual,wLen as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   



function EmptyString (s as string) as logic
	if !String.IsNullOrEmpty(s)
		foreach c as char in s
			switch c
			case '\r'
			case '\n'
			case '\t'
			case ' '
				nop
			otherwise
				return false
			end switch
		next
	endif
	return true
