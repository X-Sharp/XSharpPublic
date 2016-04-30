//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
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
	FUNCTION MemoLine(cMemo AS USUAL,wWidth AS USUAL,wLineNum AS USUAL,wTabSize AS USUAL,lWrap AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the contents of a text file as a string.
	/// </summary>
	/// <param name="cFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemoRead(cFile AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

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
	/// Replace carriage-return/linefeeds with a character that can be displayed.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nCharHard"></param>
	/// <param name="nCharSoft"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemoTran(cSource AS USUAL,nCharHard AS USUAL,nCharSoft AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

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
	/// <param name="nWidth"></param>
	/// <param name="nTabsize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLCount(c AS USUAL,nWidth AS USUAL,nTabsize AS USUAL,lWrap AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

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
	/// Return the position of a character in a formatted string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nWidth"></param>
	/// <param name="nLineNum"></param>
	/// <param name="nCol"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLcToPos(cMemo AS USUAL,nWidth AS USUAL,nLineNum AS USUAL,nCol AS USUAL,nTabSize AS USUAL,lWrap AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Extract a line of text from a string, specifying an optional offset argument.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="nLine"></param>
	/// <param name="nOffset"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLine(c AS USUAL,nLine AS USUAL,nOffset AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Extract a line of text from a string, specifying a required offset argument.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="nLine"></param>
	/// <param name="PtrN"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLine3(c AS STRING,nLine AS DWORD,PtrN AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Determine the position of a line in a string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nWidth"></param>
	/// <param name="nLineNum"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLPos(cMemo AS USUAL,nWidth AS USUAL,nLineNum AS USUAL,nTabSize AS USUAL,lWrap AS USUAL) AS DWORD
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

	#endregion
end namespace