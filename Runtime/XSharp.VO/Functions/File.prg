using XSharp
/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="uxFileAttr"></param>
/// <returns>
/// </returns>
unsafe function FCreate(cFile ,uxFileAttr ) as ptr
	/// THROW NotImplementedException{}
return IntPtr.Zero


/// <summary>
/// Read a line from an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FGetS(pFile ,nBuffLen ) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Open a file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="wMode"></param>
/// <returns>
/// </returns>
unsafe function FOpen(cFile ,wMode ) as ptr
	/// THROW NotImplementedException{}
return IntPtr.Zero


/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FPutS(pFile ,c ,nCount ) as dword
	/// THROW NotImplementedException{}
	return 0   


/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="refC"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
unsafe function FRead(pHandle as IntPtr,refC as object,dwCount as dword) as dword
	/// THROW NotImplementedException{}
return 0   



/// <summary>
/// Read a line from an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FReadLine(pFile as __Usual,nBuffLen as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Set the file pointer to a new position.
/// </summary>
/// <param name="nFile"></param>
/// <param name="nOffset"></param>
/// <param name="nOrigin"></param>
/// <returns>
/// </returns>
function FSeek(nFile ,nOffset ,nOrigin ) as long
	/// THROW NotImplementedException{}
	return 0   


/// <summary>
/// Write a string to an open file.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWrite(pHandle ,c ,nCount ) as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine(pFile as __Usual,c as __Usual,nCount as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Write a string to an open file, with SetAnsi() dependency.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteText(pHandle as __Usual,c as __Usual,nCount as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   

