using XSharp


/// <summary>
/// </summary>
/// <param name="x"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function Any2Usual(x as Usual,nType as dword) as Usual
	/// THROW NotImplementedException{}
	return NIL   


function AsHexString(uValue as usual) as string
	local result as string
	if IsString(uValue)
		result := "0x"+c2Hex( (string) uValue)
	elseif IsNumeric(uValue)
		result := String.Format("{0:X8}", (int64) uValue)
	else
		result := ""
	ENDIF
	return result

/// <summary>
/// Convert a value to a right-padded string.
/// </summary>
/// <param name="u"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
function AsPadr(u as Usual,dwLen as dword) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Convert a value to a string.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function AsString(u as usual) as string
	local result as string
	do case
	case u:IsString
		result := (string) u
	case u:IsNumeric
		result := Ntrim(u)
	case u:IsSymbol
		result := Symbol2String( (symbol) u)
	case u:IsDate
		result := DTOC( (date) u)
	OTHERWISE
		result := u:ToString()
	ENDCASE
	return result
	

/// <summary>
/// Convert a string or a Psz to a Symbol.
/// </summary>
/// <param name="u">The Usual holding a string or Psz</param>
/// <returns>
/// The Symbol representing the given string or Psz.
/// </returns>
function AsSymbol(u as Usual) as Symbol
	return Symbol{(string)u}   



	
/// <summary>
/// Create a descending order key value.
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function Descend(uValue as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function DescendA(uValue as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   




/// <summary>
/// Convert a numeric expression to a left-trimmed string.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function NTrim(n as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="ptrBuff"></param>
/// <param name="nLen"></param>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
unsafe function Psz2Float(ptrBuff as ptr,nLen as int,nDec as int) as Float
	throw NotImplementedException{}
return 0   

/// <summary>
/// Convert a Psz to a Usual with a Psz tag.
/// </summary>
/// <param name="ptrSource"></param>
/// <returns>
/// </returns>
unsafe function Psz2Usual(ptrSource as ptr) as Usual
	throw NotImplementedException{}
return NIL   

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function Pad(cSource as Usual,nLen as Usual,cPad as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Pad character, numeric, and Date values with fill characters on both the right and left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadC(cSource as Usual,nLen as Usual,cPad as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Pad character, numeric, and Date values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadL(cSource as Usual,nLen as Usual,cPad as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadR(cSource as Usual,nLen as Usual,cPad as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="nLen"></param>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
function Str(n as Usual,nLen as Usual,nDec as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


	/// <summary>
	/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="iLen"></param>
	/// <param name="iDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrZero(n AS Usual,iLen AS Usual,iDec AS Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   



	/// <summary>
	/// Convert any value into a formatted string.
	/// </summary>
	/// <param name="exp"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION Transform(exp AS Usual,p AS Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   


	/// <summary>
	/// Convert a number to a word.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION ToWord(n AS Usual) AS DWORD
		return (dword) n


	/// <summary>
	/// Convert an integer expression to a Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrInt(l as long,dwLen as dword,dwDec as dword) as Psz
		/// THROW NotImplementedException{}
		return NULL_PSZ
	
	/// <summary>
	/// Convert a long integer expression to a Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrLong(l as long,dwLen as dword,dwDec as dword) as Psz
		/// THROW NotImplementedException{}
		return NULL_PSZ
	
	/// <summary>
	/// Convert a Float expression to a Psz.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrFloat(flSource as Float,dwLen as dword,dwDec as dword) as Psz
		/// THROW NotImplementedException{}
		return NULL_PSZ
	
	