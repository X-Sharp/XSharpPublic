//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsFloat(f AS FLOAT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="i"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsInt(i AS LONGINT) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsLong(li AS LONGINT) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="r4"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsReal4(r4 AS REAL4) AS REAL4
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="r8"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsReal8(r8 AS REAL8) AS REAL8
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="si"></param>
	/// <returns>
	/// </returns>
	FUNCTION AbsShort(si AS SHORT) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Create an uninitialized, one-dimensional array.
	/// </summary>
	/// <param name="dwDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayCreate(dwDim AS DWORD) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Create an initialized array.
	/// </summary>
	/// <param name="dwDim"></param>
	/// <param name="ptrBuff"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayInit(dwDim AS DWORD,ptrBuff AS PTR) AS ARRAY
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
	RETURN NULL_STRING   

	/// <summary>
	/// Convert an ASCII code to a character value.
	/// </summary>
	/// <param name="dwChar"></param>
	/// <returns>
	/// </returns>
	FUNCTION CHR(dwChar AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Format a set of numbers representing a year, month, and day as a date.
	/// </summary>
	/// <param name="dwY"></param>
	/// <param name="dwM"></param>
	/// <param name="dwDay"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS DATE
		/// THROW NotImplementedException{}
	RETURN (DATE)0   

	/// <summary>
	/// Format a set of numbers representing an hour, minute, and second as a time string.
	/// </summary>
	/// <param name="dwHour"></param>
	/// <param name="dwMinute"></param>
	/// <param name="dwSeconds"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConTime(dwHour AS DWORD,dwMinute AS DWORD,dwSeconds AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

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
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a double word to a string containing a 32-bit unsigned integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION DW2Bin(n AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

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
	FUNCTION DynToOldSpaceFloat(f AS FLOAT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the empty value of a specified data type.
	/// </summary>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	FUNCTION EmptyUsual(dwType AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return an error message associated with a system-generated error code.
	/// </summary>
	/// <param name="nGenCode"></param>
	/// <returns>
	/// </returns>
	FUNCTION ErrString(nGenCode AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="nRetVal"></param>
	/// <returns>
	/// </returns>
	FUNCTION ExitVOThread(nRetVal AS INT) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// Convert a float to a string containing an 80-bit floating point number.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION F2Bin(f AS FLOAT) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Calculate the factorial of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION Fact(n AS DWORD) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Display file attributes as a string.
	/// </summary>
	/// <param name="nAttrib"></param>
	/// <returns>
	/// </returns>
	FUNCTION FAttr2String(nAttrib AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <returns>
	/// </returns>
	FUNCTION FClone(o AS FLOAT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Get the contents of a field that is identified by its work area and a symbol.
	/// </summary>
	/// <param name="dwArea"></param>
	/// <param name="symField"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldGetArea(dwArea AS DWORD,symField AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the name of a field as a string.
	/// </summary>
	/// <param name="dwFieldPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Set the value of a field identified by its work area number and field name.
	/// </summary>
	/// <param name="dwArea"></param>
	/// <param name="symField"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPutArea(dwArea AS DWORD,symField AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the name of a field as a symbol.
	/// </summary>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldSym(dwPos AS DWORD) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Set the display format for a floating point numeric.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION FloatFormat(f AS FLOAT,nLen AS INT,nDec AS INT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION FloatNext(f AS FLOAT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the fractional portion of a number.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION Frac(f AS FLOAT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the high-order (leftmost) byte in a number.
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION HiByte(dw AS WORD) AS BYTE
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the high-order (leftmost) word in a number.
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION HiWord(dw AS DWORD) AS WORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert a short integer to a string containing a 16-bit signed integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION I2Bin(n AS SHORT) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Check to see if a typed dynamic object is static.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsOldSpaceFloat(f AS FLOAT) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemArrayNew(dwSize AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="wMonth"></param>
	/// <returns>
	/// </returns>
	FUNCTION JNTOCMONTH(wMonth AS WORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="wYear"></param>
	/// <returns>
	/// </returns>
	FUNCTION JNTOCYEAR(wYear AS WORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a long integer to a string containing a 32-bit signed integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION L2Bin(n AS LONG) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the low-order (rightmost) byte in a number.
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION LoByte(dw AS WORD) AS BYTE
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the low-order (rightmost) word in a number.
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION LoWord(dw AS DWORD) AS WORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Allocate a static memory buffer of a specified size.
	/// </summary>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemAlloc(cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemAllocBlk(cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Allocate static memory buffers of a specified size.
	/// </summary>
	/// <param name="ui"></param>
	/// <param name="cbCell"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemCAlloc(ui AS DWORD,cbCell AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Allocate a new memory buffer in a group.
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemGrpAlloc(dwGroup AS DWORD,cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="dwGroup"></param>
	/// <param name="cb"></param>
	/// <param name="cbCell"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemGrpCAlloc(dwGroup AS DWORD,cb AS DWORD,cbCell AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

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
	FUNCTION MemGrpEnum(dwGroup AS DWORD,pFunction AS PTR) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="xd"></param>
	/// <param name="wDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS FLOAT
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
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the number that identifies a month into the name of the month.
	/// </summary>
	/// <param name="dwMonth"></param>
	/// <returns>
	/// </returns>
	FUNCTION NToCMonth(dwMonth AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION OldSpaceFreeFloat(f AS FLOAT) AS VOID
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
	/// Convert a Real4 value to a string containing a 32-bit floating point number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION Real42Bin(n AS REAL4) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a Real8 value to a string containing an 8-byte floating point number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION Real82Bin(n AS REAL8) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Create a string of spaces.
	/// </summary>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	FUNCTION Space(dwSize AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a numeric expression to a string.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str1(f AS FLOAT) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a numeric expression to a string of a specified length.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str2(f AS FLOAT,dwLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a numeric expression to a string of specific length and decimal places.
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Exchange the right and left halves of a double word.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapDWord(li AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Exchange the right and left halves of an integer.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapInt(li AS LONG) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Exchange the right and left halves of a long integer.
	/// </summary>
	/// <param name="li"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapLong(li AS LONG) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Exchange the right and left halves of a short integer.
	/// </summary>
	/// <param name="si"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapShort(si AS SHORT) AS SHORT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Exchange the right and left halves of a word.
	/// </summary>
	/// <param name="w"></param>
	/// <returns>
	/// </returns>
	FUNCTION SwapWord(w AS WORD) AS WORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Sound a speaker tone for a specified frequency and duration.
	/// </summary>
	/// <param name="dwFreq"></param>
	/// <param name="dwDur"></param>
	/// <returns>
	/// </returns>
	FUNCTION Tone(dwFreq AS DWORD,dwDur AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	FUNCTION TypeString(dwType AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a word to a string containing a 16-bit unsigned integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION W2Bin(n AS WORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

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