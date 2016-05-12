//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.IO
	#region functions
	/// <summary>
	/// Change the size of a file opened with a low-level file function.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nOffset"></param>
	/// <returns>
	/// </returns>
	FUNCTION FChSize(pFile AS IntPtr,nOffset AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Close an open file and write the buffers to disk.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION FClose(pFile AS IntPtr) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush file buffers.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION FCommit(pHandle AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Determine if the file pointer is positioned at the end-of-file.
	/// </summary>
	/// <param name="pFILE"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION FEof(pFILE AS IntPtr) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Lock a portion of an open file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="dwOffset"></param>
	/// <param name="dwLength"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFLock(pHandle AS IntPtr,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush to disk a file opened with a low-level file function.
	/// </summary>
	/// <param name="phandle"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFlush(phandle AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Unlock a portion of an opened file.
	/// </summary>
	/// <param name="phandle"></param>
	/// <param name="dwOffset"></param>
	/// <param name="dwLength"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFUnLock(phandle AS IntPtr,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read a line from an open file, specifying two strongly typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION FGetS2(pFile AS IntPtr,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldVal(ptrBuff AS IntPtr,nLen AS INT,nDec AS INT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrUsual"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION Float2Psz(ptrUsual AS IntPtr,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
	/// </summary>
	/// <param name="pFILE"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FPutS3(pFILE AS IntPtr,c AS STRING,nCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read characters from a file into a buffer variable that is passed by reference.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="refC"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRead(pHandle AS IntPtr,refC AS USUAL,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read characters from a file into an allocated buffer.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRead3(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read characters from a file into an allocated buffer.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <param name="lAnsi"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRead4(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read a line from an open file, specifying two strongly-typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadLine2(pFile AS IntPtr,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Read characters from a file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadStr(pHandle AS IntPtr,dwCount AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Read characters from a file into a buffer variable that is passed by reference.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="refC"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadText(pHandle AS IntPtr,refC AS USUAL,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read characters from a file into an allocated buffer, with possible OEM to ANSI conversion, based on the current SetAnsi() setting.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadText3(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the file pointer at the top of an open file.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRewind(pFile AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// Set the file pointer to a new position, specifying three strongly-typed arguments.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="lOffset"></param>
	/// <param name="dwOrigin"></param>
	/// <returns>
	/// </returns>
	FUNCTION FSeek3(pHandle AS IntPtr,lOffset AS LONG,dwOrigin AS DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the current position of the file pointer.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <returns>
	/// </returns>
	FUNCTION FTell(pHandle AS IntPtr) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION FunctionPtr2Sym(ptrFunc AS IntPtr) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Write the contents of a buffer to an open file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWrite3(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write the contents of a buffer to an open file, with an ANSI to OEM conversion option.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <param name="lAnsi"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWrite4(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
	/// </summary>
	/// <param name="pFILE"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWriteLine3(pFILE AS IntPtr,c AS STRING,nCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write the contents of a buffer to an open file, with SetAnsi() dependency.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWriteText3(pHandle AS IntPtr,ptrBuffer AS IntPtr,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
end namespace