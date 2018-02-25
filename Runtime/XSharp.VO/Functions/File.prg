using XSharp
/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="uxFileAttr"></param>
/// <returns>
/// </returns>
unsafe FUNCTION FCreate(cFile AS __Usual,uxFileAttr AS __Usual) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero




/// <summary>
/// Open a file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="wMode"></param>
/// <returns>
/// </returns>
unsafe FUNCTION FOpen(cFile AS __Usual,wMode AS __Usual) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero


	/// <summary>
	/// Write a string to an open file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWrite(pHandle AS __Usual,c AS __Usual,nCount AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write a string, a carriage-return character, and a linefeed character to an open file.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWriteLine(pFile AS __Usual,c AS __Usual,nCount AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   
	
	/// <summary>
	/// Write a string to an open file, with SetAnsi() dependency.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWriteText(pHandle AS __Usual,c AS __Usual,nCount AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

