begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Remove spaces from a file name specified as a string.
	/// </summary>
	/// <param name="cName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFName(cName AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Remove spaces from a file name specified as a string, changing the contents of the original file name as well as the returned file name.
	/// </summary>
	/// <param name="cName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFNameA(cName AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Remove leading and trailing spaces from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION AllTrim(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a 24-hour military time to a 12-hour clock time.
	/// </summary>
	/// <param name="cTime"></param>
	/// <returns>
	/// </returns>
	FUNCTION AmPm(cTime AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string of ANSI characters to OEM characters.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Ansi2Oem(cSource AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Ansi2OemA(cSource AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a character to its ASCII value.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Asc(c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION At(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION At2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, starting at a specified position.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <param name="dwOff"></param>
	/// <returns>
	/// </returns>
	FUNCTION At3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION AtC(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION AtC2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATCLine(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATCLine2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATLine(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATLine2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION B64EncFile(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="cIn"></param>
	/// <returns>
	/// </returns>
	FUNCTION B64EncString(cIn AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string containing a 32-bit binary date to a date data type.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2Date(c AS STRING) AS DATE
		/// THROW NotImplementedException{}
	RETURN 0   

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
	/// Convert a string containing a 80-bit floating point number to a float value.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2F(c AS STRING) AS FLOAT
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
	FUNCTION Bin2Ptr(c AS STRING) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a string containing a 32-bit floating point number to a Real4 value.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2Real4(c AS STRING) AS REAL4
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert a string containing a 32-bit floating point number to a Real8 value.
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
	RETURN NULL_STRING   

	/// <summary>
	/// Return the even-numbered characters in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION CharEven(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return a string whose odd-numbered characters and even-numbered characters are from 2 different strings.
	/// </summary>
	/// <param name="c1"></param>
	/// <param name="c2"></param>
	/// <returns>
	/// </returns>
	FUNCTION CharMix(c1 AS STRING,c2 AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the odd-numbered characters in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION CharOdd(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return a character based on its position in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="nStart"></param>
	/// <returns>
	/// </returns>
	FUNCTION CharPos(c AS STRING,nStart AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Encrypt or decrypt a string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="cKey"></param>
	/// <returns>
	/// </returns>
	FUNCTION Crypt(cSource AS STRING,cKey AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="cKey"></param>
	/// <returns>
	/// </returns>
	FUNCTION CryptA(cSource AS STRING,cKey AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a date string to date format.
	/// </summary>
	/// <param name="cDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION CToD(cDate AS STRING) AS DATE
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert an ANSI date string to date format.
	/// </summary>
	/// <param name="cDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION CToDAnsi(cDate AS STRING) AS DATE
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Decode a file from an e-mail transfer.
	/// </summary>
	/// <param name="cMailPart"></param>
	/// <param name="hfOut"></param>
	/// <returns>
	/// </returns>
	FUNCTION DecodeBase64(cMailPart AS STRING,hfOut AS PTR) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <returns>
	/// </returns>
	FUNCTION DeleteRTRegKey(cSubKey AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="cFile"></param>
	/// <param name="nOptions"></param>
	/// <returns>
	/// </returns>
	FUNCTION DynMemDump(cFile AS STRING,nOptions AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Copy a typed dynamic object to static allocated memory.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION DynToOldSpaceString(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the difference between two time strings.
	/// </summary>
	/// <param name="cStartTime"></param>
	/// <param name="cEndTime"></param>
	/// <returns>
	/// </returns>
	FUNCTION ElapTime(cStartTime AS STRING,cEndTime AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Evaluate an expression contained in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Evaluate(c AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Copy a file to a new file or to a device.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="cDest"></param>
	/// <returns>
	/// </returns>
	FUNCTION FCopy(cSrc AS STRING,cDest AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Create a file or open and truncate an existing file, specifying two strongly typed arguments.
	/// </summary>
	/// <param name="cFile"></param>
	/// <param name="dwAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FCreate2(cFile AS STRING,dwAttr AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Delete a file from disk.
	/// </summary>
	/// <param name="cFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION FErase(cFile AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return a set-get code block for a field that is identified by its name.
	/// </summary>
	/// <param name="cVAr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldBlock(cVAr AS STRING) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// Return the position of a field.
	/// </summary>
	/// <param name="sFieldName"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPos(sFieldName AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a set-get code block for a field, specified as a string, in a specified work area.
	/// </summary>
	/// <param name="cVar"></param>
	/// <param name="nArea"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldWBlock(cVar AS STRING,nArea AS DWORD) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// Determine if any file matches a given file specification.
	/// </summary>
	/// <param name="cFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION File(cFile AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Open a file, specifying two strongly-typed arguments.
	/// </summary>
	/// <param name="cName"></param>
	/// <param name="dwMode"></param>
	/// <returns>
	/// </returns>
	FUNCTION FOpen2(cName AS STRING,dwMode AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Change the name of a file.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="cDest"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRename(cSrc AS STRING,cDest AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Open a file.
	/// </summary>
	/// <param name="cFile"></param>
	/// <param name="dwMode"></param>
	/// <param name="cPath"></param>
	/// <returns>
	/// </returns>
	FUNCTION FxOpen(cFile AS STRING,dwMode AS DWORD,cPath AS STRING) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Replace all soft carriage returns (Chr(141)) in a string with hard carriage returns (Chr(13)).
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION HardCR(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Hex2C(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Indicate whether a substring is contained in a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Instr(cSearch AS STRING,c AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the first character of a string is a kanji character.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ISKANJI(c AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Check to see if a typed dynamic object is static.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsOldSpaceString(c AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the given string is a valid VO string.
	/// </summary>
	/// <param name="cString"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsVOString(cString AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Extract a substring beginning with the first character in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION Left(c AS STRING,dwLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the uppercase and mixed case characters in a string to lowercase.
	/// </summary>
	/// <param name="cSorce"></param>
	/// <returns>
	/// </returns>
	FUNCTION Lower(cSorce AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the uppercase and mixed case characters in a string to lowercase, changing the contents of the argument as well as the return value.
	/// </summary>
	/// <param name="cSorce"></param>
	/// <returns>
	/// </returns>
	FUNCTION LowerA(cSorce AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Remove leading spaces from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION LTrim(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Perform an assignment to a variable whose name is stored in a specified string.
	/// </summary>
	/// <param name="cExp"></param>
	/// <param name="xValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION MAssign(cExp AS STRING,xValue AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Macro compile a string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION MCompile(s AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Count the number of lines in a string or memo field.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemLines(c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a set-get code block for a given memory variable.
	/// </summary>
	/// <param name="cVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarBlock(cVar AS STRING) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// Return the contents of a memory variable.
	/// </summary>
	/// <param name="cVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarGet(cVar AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Assign a value to a memory variable of a given name.
	/// </summary>
	/// <param name="cVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarPut(cVar AS STRING,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Evaluate a macro-compiled string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MExec(c AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION MPrepare(s AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Multi2Wide(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the number of times a substring occurs in a string.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Occurs(cSrc AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the number of times a substring occurs in a string.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Occurs2(cSrc AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the number of times a substring occurs in a string, starting at a specified position.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="c"></param>
	/// <param name="nOffs"></param>
	/// <returns>
	/// </returns>
	FUNCTION Occurs3(cSrc AS STRING,c AS STRING,nOffs AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert a string of OEM characters to ANSI characters.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Oem2Ansi(cSource AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Oem2AnsiA(cSource AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION OldSpaceFreeString(c AS STRING) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Change the first character of each word to uppercase
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Proper(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Capitalize a proper name correctly, changing the contents of the argument as well as the return value.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION ProperA(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="cIn"></param>
	/// <returns>
	/// </returns>
	FUNCTION QPEncString(cIn AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <returns>
	/// </returns>
	FUNCTION QueryRTRegArray(cSubKey AS STRING) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Retrieve a numeric value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
	FUNCTION QueryRTRegInt(cSubKey AS STRING,cKeyName AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Retrieve a string value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
	FUNCTION QueryRTRegString(cSubKey AS STRING,cKeyName AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RAt(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RAt2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <param name="dwOff"></param>
	/// <returns>
	/// </returns>
	FUNCTION RAt3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the last occurrence of a substring within a multiline string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RATLine(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the last occurrence of a substring within a multiline string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RATLine2(cSearch AS STRING,c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Repl(c AS STRING,dwCount AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Repeat a string a specified number of times.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Replicate(c AS STRING,dwCount AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return a substring beginning with the rightmost character.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION Right(c AS STRING,dwLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Remove trailing spaces from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RTrim(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert single-byte kana characters in a string to their double-byte equivalents.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION SBTODB(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Create new character variable with the same characters as the original string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION SClone(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return a time as the number of seconds that have elapsed since midnight.
	/// </summary>
	/// <param name="cTime"></param>
	/// <returns>
	/// </returns>
	FUNCTION Secs(cTime AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the length of a strongly typed string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION SLen(c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert a string to Soundex form.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION SoundEx(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert an ANSI date string to date format.
	/// </summary>
	/// <param name="cDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION SToD(cDate AS STRING) AS DATE
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Allows text substitution in strings entered at runtime.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrEvaluate(s AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string to a symbol.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION String2Atom(c AS STRING) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="cAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION String2FAttr(cAttr AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Convert a string to an uppercase symbol.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION String2Symbol(c AS STRING) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwRadix"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrToFloat(c AS STRING,dwRadix AS DWORD) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwRadix"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrToLong(c AS STRING,dwRadix AS DWORD) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Delete and insert characters in a string.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="n"></param>
	/// <param name="nDel"></param>
	/// <param name="cIns"></param>
	/// <returns>
	/// </returns>
	FUNCTION Stuff(c AS STRING,n AS DWORD,nDel AS DWORD,cIns AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Extract a substring from a string, using strong typing and only two arguments.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwStart"></param>
	/// <returns>
	/// </returns>
	FUNCTION SubStr2(c AS STRING,dwStart AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Extract a substring from a string, using strong typing and three required arguments.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwStart"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION SubStr3(c AS STRING,dwStart AS DWORD,dwLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert single-byte and double-byte katakana characters in a string to their double-byte hiragana equivalents.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION TOHIRA(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the single-byte and double-byte numbers in a string to double-byte kanji numbers.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION TOJNUM(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert single-byte katakana and double-byte hiragana characters in a string to their double-byte katakana equivalents.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION TOKATA(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Remove trailing spaces from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Trim(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Determine the data type of an expression represented as a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION TYPE(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the lowercase and mixed case characters in a string to uppercase.
	/// </summary>
	/// <param name="cSorce"></param>
	/// <returns>
	/// </returns>
	FUNCTION Upper(cSorce AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert the lowercase and mixed case characters in a string to uppercase, changing the contents of the argument as well as the return value.
	/// </summary>
	/// <param name="cSorce"></param>
	/// <returns>
	/// </returns>
	FUNCTION UpperA(cSorce AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="cLine"></param>
	/// <param name="hfOut"></param>
	/// <returns>
	/// </returns>
	FUNCTION UUDecodeLine(cLine AS STRING,hfOut AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION UUEncFile(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION UUEncLine(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert a string containing a numeric value to a numeric data type.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION Val(c AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the contents of a field or a memory variable.
	/// </summary>
	/// <param name="cVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION VarGet(cVar AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Assign a value to a field or a memory variable of a given name.
	/// </summary>
	/// <param name="cVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION VarPut(cVar AS STRING,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="cBstr"></param>
	/// <returns>
	/// </returns>
	FUNCTION Wide2Multi(cBstr AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	#endregion
end namespace