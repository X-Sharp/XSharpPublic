using XSharp


/// <summary>
/// </summary>
/// <param name="x"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function Any2Usual(x as __Usual,nType as dword) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   




/// <summary>
/// Convert a value to a right-padded string.
/// </summary>
/// <param name="u"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
function AsPadr(u as __Usual,dwLen as dword) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Convert a value to a string.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function AsString(u as __Usual) as string
	return (string) u   

/// <summary>
/// Convert a string or a __Psz to a __Symbol.
/// </summary>
/// <param name="u">The __Usual holding a string or __Psz</param>
/// <returns>
/// The __Symbol representing the given string or __Psz.
/// </returns>
function AsSymbol(u as __Usual) as __Symbol
	return __Symbol{(string)u}   



	
/// <summary>
/// Create a descending order key value.
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function Descend(uValue as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function DescendA(uValue as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   


/// <summary>
/// Make sure a variable is a numeric.
/// </summary>
/// <param name="refu"></param>
/// <returns> 
/// </returns>
function EnforceNumeric(refu as __Usual) as void
	/// THROW NotImplementedException{}
	return  


/// <summary>
/// Make sure a variable is of a certain type.
/// </summary>
/// <param name="refu"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function EnforceType(refu as __Usual,nType as dword) as void
	/// THROW NotImplementedException{}
	return  



/// <summary>
/// Convert a numeric expression to a left-trimmed string.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function NTrim(n as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="ptrBuff"></param>
/// <param name="nLen"></param>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
unsafe function Psz2Float(ptrBuff as ptr,nLen as int,nDec as int) as __VOFloat
	throw NotImplementedException{}
return 0   

/// <summary>
/// Convert a __Psz to a __Usual with a __Psz tag.
/// </summary>
/// <param name="ptrSource"></param>
/// <returns>
/// </returns>
unsafe function Psz2Usual(ptrSource as ptr) as __Usual
	throw NotImplementedException{}
return __Usual._NIL   

/// <summary>
/// Pad character, numeric, and __VODate values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function Pad(cSource as __Usual,nLen as __Usual,cPad as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Pad character, numeric, and __VODate values with fill characters on both the right and left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadC(cSource as __Usual,nLen as __Usual,cPad as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Pad character, numeric, and __VODate values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadL(cSource as __Usual,nLen as __Usual,cPad as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Pad character, numeric, and __VODate values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLen"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadR(cSource as __Usual,nLen as __Usual,cPad as __Usual) as string
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
function Str(n as __Usual,nLen as __Usual,nDec as __Usual) as string
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
	FUNCTION StrZero(n AS __Usual,iLen AS __Usual,iDec AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   



	/// <summary>
	/// Convert any value into a formatted string.
	/// </summary>
	/// <param name="exp"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION Transform(exp AS __Usual,p AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   


	/// <summary>
	/// Convert a number to a word.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION ToWord(n AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   
