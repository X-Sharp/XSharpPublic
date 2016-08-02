//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Vulcan

begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION Abs__VOFloat(f AS __VOFloat) AS __VOFloat
	RETURN __VOFloat{Math.Abs(f:Value)}

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="i"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsInt(i AS LONGINT) AS LONG
	RETURN Math.Abs(i)

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsLong(li AS LONGINT) AS LONG
	RETURN Math.Abs(li)

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="r4"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsReal4(r4 AS REAL4) AS REAL4
	RETURN Math.Abs(r4)

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="r8"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsReal8(r8 AS REAL8) AS REAL8
	RETURN Math.Abs(r8)

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="si"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsShort(si AS SHORT) AS LONG
	RETURN Math.Abs(si)

	/// <summary>
	/// Create an uninitialized, one-dimensional __Array.
	/// </summary>
	/// <param name="dwDim">The number of elements in the new __Array.</param>
	/// <returns>
	/// An uninitialized of the given length.
	/// </returns>
	FUNCTION __ArrayCreate(dwDim AS DWORD) AS __Array
	RETURN __Array{(int)dwDim}

	/// <summary>
	/// Create an initialized __Array.
	/// </summary>
	/// <param name="dwDim"></param>
	/// <param name="ptrBuff"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __ArrayInit(dwDim AS DWORD,ptrBuff AS PTR) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Return an uninitialized string of a specified size.
	/// </summary>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	FUNCTION Buffer(dwSize AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Convert an ASCII code to a character value.
	/// </summary>
	/// <param name="dwChar"></param>
	/// <returns>
	/// </returns>
	FUNCTION CHR(dwChar AS DWORD) AS STRING
		local buf := byte[]{1} as byte[]
		buf[0+__ARRAYBASE__] := (byte) dwChar
	RETURN System.Text.Encoding:ASCII:GetString(buf)

	/// <summary>
	/// Format a set of numbers representing a year, month, and day as a __VODate.
	/// </summary>
	/// <param name="dwY"></param>
	/// <param name="dwM"></param>
	/// <param name="dwDay"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS __VODate
	RETURN __VODate{dwY,dwM,dwDay}   

	/// <summary>
	/// Format a set of numbers representing an hour, minute, and second as a time string.
	/// </summary>
	/// <param name="dwHour"></param>
	/// <param name="dwMinute"></param>
	/// <param name="dwSeconds"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConTime(dwHour AS DWORD,dwMinute AS DWORD,dwSeconds AS DWORD) AS STRING
		local ts as TimeSpan
		ts := TimeSpan.FromSeconds(dwHour*60*60+dwMinute*60+dwSeconds)		
	RETURN ts:ToString()   

	/// <summary>
	/// </summary>
	/// <param name="dwVS"></param>
	/// <param name="dwStep"></param>
	/// <returns>
	/// </returns>
	FUNCTION CreateAtomTable(dwVS AS DWORD,dwStep AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a description string for a DOS error number.
	/// </summary>
	/// <param name="nDosErr"></param>
	/// <returns>
	/// </returns>
	FUNCTION DosErrString(nDosErr AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Convert a double word to a string containing a 32-bit unsigned integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION DW2Bin(n AS DWORD) AS STRING
		local byte__Array := BitConverter.GetBytes( n ) as byte[]
	RETURN System.Text.Encoding.ASCII:GetString(byte__Array)

	/// <summary>
	/// Resize the dynamic memory pool to a specific number of pages.
	/// </summary>
	/// <param name="dwPages"></param>
	/// <returns>
	/// </returns>
	FUNCTION DynSize(dwPages AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Copy a typed dynamic object to static allocated memory.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION DynToOldSpace__VOFloat(f AS __VOFloat) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the empty value of a specified data type.
	/// </summary>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	FUNCTION EmptyUsual(dwType AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return an error message associated with a system-generated error code.
	/// </summary>
	/// <param name="nGenCode"></param>
	/// <returns>
	/// </returns>
	FUNCTION ErrString(nGenCode AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="nRetVal"></param>
	/// <returns>
	/// </returns>
	FUNCTION ExitVOThread(nRetVal AS INT) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// Convert a __VOFloat to a string containing an 80-bit __VOFloating point number.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION F2Bin(f AS __VOFloat) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty

	/// <summary>
	/// Calculate the factorial of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION Fact(n AS DWORD) AS __VOFloat
		local result := 1 as double
		if  n > 0
		    local i as dword
		    for i := 1 upto n
			    result := result * i
			next
		endif
	RETURN (__VOFloat)result    

	/// <summary>
	/// Display file attributes as a string.
	/// </summary>
	/// <param name="nAttrib"></param>
	/// <returns>
	/// </returns>
	FUNCTION FAttr2String(nAttrib AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <returns>
	/// </returns>
	FUNCTION FClone(o AS __VOFloat) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Get the contents of a field that is identified by its work area and a __Symbol.
	/// </summary>
	/// <param name="dwArea"></param>
	/// <param name="symField"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldGetArea(dwArea AS DWORD,symField AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return the name of a field as a string.
	/// </summary>
	/// <param name="dwFieldPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Set the value of a field identified by its work area number and field name.
	/// </summary>
	/// <param name="dwArea"></param>
	/// <param name="symField"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPutArea(dwArea AS DWORD,symField AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return the name of a field as a __Symbol.
	/// </summary>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldSym(dwPos AS DWORD) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Set the display format for a __VOFloating point numeric.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION __VOFloatFormat(f AS __VOFloat,nLen AS INT,nDec AS INT) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION __VOFloatNext(f AS __VOFloat) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the fractional portion of a number.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION Frac(f AS __VOFloat) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

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
	/// Check to see if a typed dynamic object is static.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsOldSpace__VOFloat(f AS __VOFloat) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	FUNCTION Item__ArrayNew(dwSize AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="wMonth"></param>
	/// <returns>
	/// </returns>
	FUNCTION JNTOCMONTH(wMonth AS WORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="wYear"></param>
	/// <returns>
	/// </returns>
	FUNCTION JNTOCYEAR(wYear AS WORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

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
	/// Allocate a static memory buffer of a specified size.
	/// </summary>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemAlloc(cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemAllocBlk(cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Allocate static memory buffers of a specified size.
	/// </summary>
	/// <param name="ui"></param>
	/// <param name="cbCell"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemCAlloc(ui AS DWORD,cbCell AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Allocate a new memory buffer in a group.
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemGrpAlloc(dwGroup AS DWORD,cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <param name="cb"></param>
	/// <param name="cbCell"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemGrpCAlloc(dwGroup AS DWORD,cb AS DWORD,cbCell AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Close a memory group.
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemGrpClose(dwGroup AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemGrpCompact(dwGroup AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <param name="pFunction"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemGrpEnum(dwGroup AS DWORD,pFunction AS PTR) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="xd"></param>
	/// <param name="wDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION MyDal__VOFloatVal(xd AS REAL8,wDec AS WORD) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwInst"></param>
	/// <returns>
	/// </returns>
	FUNCTION NationInit(dwInst AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert the number that identifies a day into the name of the day.
	/// </summary>
	/// <param name="dwDay"></param>
	/// <returns>
	/// </returns>
	FUNCTION NToCDoW(dwDay AS DWORD) AS STRING
		local culture := System.Globalization.CultureInfo.CurrentCulture as System.Globalization.CultureInfo
	RETURN culture:DateTimeFormat:GetDayName((System.DayOfWeek)dwDay)

	/// <summary>
	/// Convert the number that identifies a month into the name of the month.
	/// </summary>
	/// <param name="dwMonth"></param>
	/// <returns>
	/// </returns>
	FUNCTION NToCMonth(dwMonth AS DWORD) AS STRING
		local culture := System.Globalization.CultureInfo.CurrentCulture as System.Globalization.CultureInfo
	RETURN culture:DateTimeFormat:GetMonthName((int)dwMonth)   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION OldSpaceFree__VOFloat(f AS __VOFloat) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// </summary>
	/// <param name="hf"></param>
	/// <returns>
	/// </returns>
	FUNCTION ReadAtomTable(hf AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

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
	/// Create a string of spaces.
	/// </summary>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	FUNCTION Space(dwSize AS DWORD) AS STRING
	RETURN string{' ',(int)dwSize}

	/// <summary>
	/// Convert a numeric expression to a string.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str1(f AS __VOFloat) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Convert a numeric expression to a string of a specified length.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str2(f AS __VOFloat,dwLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Convert a numeric expression to a string of specific length and decimal places.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str3(f AS __VOFloat,dwLen AS DWORD,dwDec AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Exchange the right and left halves of a double word.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapDWord(li AS DWORD) AS DWORD
	return (dword)((dword)((li & 0x0000ffff) << 16) | ((li >> 16) & 0x0000ffff))   

	/// <summary>
	/// Exchange the right and left halves of an integer.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapInt(li AS LONG) AS LONG
	RETURN SwapLong(li) 

	/// <summary>
	/// Exchange the right and left halves of a long integer.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapLong(li AS LONG) AS LONG
	return (long)((long)((li & 0x0000ffff) << 16) | ((li >> 16) & 0x0000ffff))

	/// <summary>
	/// Exchange the right and left halves of a short integer.
	/// </summary>
	/// <param name="si"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapShort(si AS SHORT) AS SHORT
	return 0 // (short)((short)((si & 0x00ff) << 8) | ((si >> 8) & 0x00ff))

	/// <summary>
	/// Exchange the right and left halves of a word.
	/// </summary>
	/// <param name="w"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapWord(w AS WORD) AS WORD
	return (word)((word)((w & 0x00ff) << 8) | ((w >> 8) & 0x00ff))

	/// <summary>
	/// Sound a speaker tone for a specified frequency and duration.
	/// </summary>
	/// <param name="dwFreq"></param>
	/// <param name="dwDur"></param>
	/// <returns>
	/// </returns>
	FUNCTION Tone(dwFreq AS DWORD,dwDur AS DWORD) AS __Usual
		System.Media.SystemSounds.Beep:Play()
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	FUNCTION TypeString(dwType AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Convert a word to a string containing a 16-bit unsigned integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION W2Bin(n AS WORD) AS STRING
		local byte__Array := BitConverter.GetBytes( n ) as byte[]
	RETURN System.Text.Encoding.ASCII:GetString(byte__Array)    

	/// <summary>
	/// </summary>
	/// <param name="hf"></param>
	/// <returns>
	/// </returns>
	FUNCTION WriteAtomTable(hf AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
end namespace