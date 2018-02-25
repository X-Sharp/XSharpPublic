//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>

	/// <summary>
	/// Replace carriage-return/linefeeds with a character that can be displayed.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nCharHard"></param>
	/// <param name="nCharSoft"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemoTran(cSource AS __Usual,nCharHard AS __Usual,nCharSoft AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   


		/// <summary>
	/// Count the number of lines in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="nWidth"></param>
	/// <param name="nTabsize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLCount(c AS __Usual,nWidth AS __Usual,nTabsize AS __Usual,lWrap AS __Usual) AS DWORD
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
	FUNCTION MLine(c AS __Usual,nLine AS __Usual,nOffset AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   



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
	FUNCTION MLcToPos(cMemo AS __Usual,nWidth AS __Usual,nLineNum AS __Usual,nCol AS __Usual,nTabSize AS __Usual,lWrap AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Extract a line of text from a string, specifying a required offset argument.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="nLine"></param>
	/// <param name="PtrN"></param>
	/// <returns>
	/// </returns>
	FUNCTION MLine3(c AS STRING,nLine AS DWORD,PtrN AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   


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
	FUNCTION MLPos(cMemo AS __Usual,nWidth AS __Usual,nLineNum AS __Usual,nTabSize AS __Usual,lWrap AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   




	/// <summary>
	/// </summary>
	/// <param name="ptrLine"></param>
	/// <param name="nLen"></param>
	/// <param name="nLimit"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
	FUNCTION LineLen(ptrLine AS BYTE[],nLen AS DWORD,nLimit AS DWORD,nTabSize AS DWORD,lWrap AS LOGIC) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0  