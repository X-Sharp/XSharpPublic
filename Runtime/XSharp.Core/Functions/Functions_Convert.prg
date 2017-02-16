/// <summary>
/// Convert a string containing a 32-bit unsigned integer to a double word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2DW(c AS STRING) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// Convert a string containing a 16-bit signed integer to a short integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2I(c AS STRING) AS SHORT
	/// THROW NotImplementedException{}
RETURN 0

/// <summary>
/// Convert a string containing a 32-bit signed integer to a long integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2L(c AS STRING) AS LONG
	/// THROW NotImplementedException{}
RETURN 0   

	/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
unsafe FUNCTION Bin2Ptr(c AS STRING) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// Convert a string containing a 32-bit __VOFloating point number to a Real4 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Real4(c AS STRING) AS REAL4
	/// THROW NotImplementedException{}
RETURN 0   

	
/// <summary>
/// Convert a string containing a 32-bit __VOFloating point number to a Real8 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2REAL8(c AS STRING) AS REAL8
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// Convert a string containing a 16-bit unsigned integer to a word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2W(c AS STRING) AS WORD
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION C2Hex(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Return the high-order (leftmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION HiByte(dw AS WORD) AS BYTE
	local upper := Convert.ToByte(dw >> 8) AS byte
RETURN (byte) upper   

/// <summary>
/// Return the high-order (leftmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION HiWord(dw AS DWORD) AS WORD
	local upper := Convert.ToByte(dw >> 16) AS word
RETURN (WORD) upper


/// <summary>
/// Convert a short integer to a string containing a 16-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION I2Bin(n AS SHORT) AS STRING
	local byte__Array := BitConverter.GetBytes( n ) as byte[]
RETURN System.Text.Encoding.ASCII:GetString(byte__Array)  



/// <summary>
/// Convert a long integer to a string containing a 32-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION L2Bin(n AS LONG) AS STRING
	local byte__Array := BitConverter.GetBytes( n ) as byte[]
RETURN System.Text.Encoding.ASCII:GetString(byte__Array)     



/// <summary>
/// Return the low-order (rightmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION LoByte(dw AS WORD) AS BYTE
RETURN (byte) (dw & 0x00FF)

/// <summary>
/// Return the low-order (rightmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION LoWord(dw AS DWORD) AS WORD
RETURN (WORD) (dw & 0xFFFF) 




/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Multi2Wide(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



/// <summary>
/// Convert a Real4 value to a string containing a 32-bit __VOFloating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real42Bin(n AS REAL4) AS STRING
	local byte__Array := BitConverter.GetBytes( n ) as byte[]
RETURN System.Text.Encoding.ASCII:GetString(byte__Array)        

/// <summary>
/// Convert a Real8 value to a string containing an 8-byte __VOFloating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real82Bin(n AS REAL8) AS STRING
	local byte__Array := BitConverter.GetBytes( n ) as byte[]
RETURN System.Text.Encoding.ASCII:GetString(byte__Array)   




/// <summary>
/// </summary>
/// <param name="cBstr"></param>
/// <returns>
/// </returns>
FUNCTION Wide2Multi(cBstr AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

