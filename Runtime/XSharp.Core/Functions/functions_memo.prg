//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Extract a line of text from a string.
/// </summary>
/// <param name="cMemo"></param>
/// <param name="wWidth"></param>
/// <param name="wLineNum"></param> 
/// <param name="wTabSize"></param>
/// <param name="lWrap"></param>
/// <returns>
/// </returns>
FUNCTION MemoLine(cMemo AS STRING,wWidth AS LONG,wLineNum AS LONG,wTabSize AS LONG,lWrap AS LOGIC) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// Return the contents of a text file as a string.
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
FUNCTION MemoRead(cFile AS STRING) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// Report the status of memory. 
/// </summary>
/// <param name="iFunc"></param>
/// <returns>
/// </returns>
FUNCTION Memory(iFunc AS INT) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Write a string to a disk file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION MemoWrit(cFile AS STRING,c AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   


/// <summary>
/// Count the number of lines in a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION MlCount1(c AS STRING) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Determine the position of a line in a string.
/// </summary>
/// <param name="c"></param>
/// <param name="nLine"></param>
/// <returns>
/// </returns>
FUNCTION MLPos2(c AS STRING,nLine AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

