using XSharp
/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="uxFileAttr"></param>
/// <returns>
/// </returns>
unsafe function FCreate(cFile as __Usual,uxFileAttr as __Usual) as ptr
	/// THROW NotImplementedException{}
return IntPtr.Zero




/// <summary>
/// Open a file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="wMode"></param>
/// <returns>
/// </returns>
unsafe function FOpen(cFile as __Usual,wMode as __Usual) as ptr
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
function FPutS(pFile as __Usual,c as __Usual,nCount as __Usual) as dword
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
function FSeek(nFile as __Usual,nOffset as __Usual,nOrigin as __Usual) as long
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
function FWrite(pHandle as __Usual,c as __Usual,nCount as __Usual) as dword
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

