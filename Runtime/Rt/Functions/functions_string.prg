//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Remove spaces from a file name specified as a string.
	/// </summary>
	/// <param name="cName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFName(cName AS STRING) AS STRING
		local adjusted := null as string
		if ( !string.IsNullOrEmpty(cName) ) 
			adjusted := System.IO.Path.GetFileNameWithoutExtension(cName).TrimEnd()
			if ( cName:IndexOf('.') > 0 ) 
				adjusted += System.IO.Path.GetExtension(cName)
			endif
		endif
	RETURN adjusted   

	/// <summary>
	/// Remove spaces from a file name specified as a string, changing the contents of the original file name as well as the returned file name.
	/// </summary>
	/// <param name="cName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFNameA(cName AS STRING) AS STRING
		THROW NotImplementedException{}
	    /// RETURN NULL_STRING   

	/// <summary>
	/// Remove leading and trailing spaces from a string.
	/// </summary>
	/// <param name="c">The string to be trimmed.</param>
	/// <returns>
	/// The original string without leading and trailing spaces
	/// </returns>
	FUNCTION AllTrim(c AS STRING) AS STRING
	   if ( c == null )
		  return c
	   endif
	   c := c:Trim()
	RETURN c

	/// <summary>
	/// Convert a 24-hour military time to a 12-hour clock time.
	/// </summary>
	/// <param name="cTime"> A valid military time in the form hh:mm:ss, where hh is hours in 24-hour format, mm is minutes, and ss is seconds.</param>
	/// <returns>
	/// An 11-character string in 12-hour format with either "am" or "pm."  If <cTime> does not represent a valid military time, a NULL_STRING is returned.
	/// </returns>
	FUNCTION AmPm(cTime AS STRING) AS STRING
		local result:=null as string
		try 
			result := DateTime.Parse(ctime).ToString("hh:mm:ss")
			// The following exceptions may appear but will be ignored currently (VO/VN behaviour)
			// catch ex as FormatException
			//	  NOP 
			// catch ex as ArgumentNullException
			//	  NOP
		end try
	RETURN result

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
		local ascValue := 0 as dword
		if ( !string.IsNullOrEmpty(c) ) 
		   local chrBuffer := c:ToCharArray() as char[]
           local bytBuffer := System.Text.Encoding.GetEncoding(1252).GetBytes(chrBuffer) as byte[]
		   ascValue := (DWORD) bytBuffer[1]
		endif
	RETURN ascValue

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch">The string to search for.</param>
	/// <param name="c">The string to search in.</param>
	/// <returns>
	/// The position of the first occurrence of <cSearch> within <cTarget>.  If <cSearch> is not found, At() returns 0.
	/// If cSearch is empty or c is empty, At() returns 0.
	/// </returns>
	FUNCTION At(cSearch AS STRING,c AS STRING) AS DWORD
		local position := -1 as int
		if ( c != null && cSearch != null )
			position := c:IndexOf(cSearch)
		endif
	RETURN (DWORD) position+1  

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch">The string to search for.</param>
	/// <param name="c">The string to search in.</param>
	/// <returns>
	/// The position of the first occurrence of <cSearch> within <cTarget>.  If <cSearch> is not found, At() returns 0.
	/// If cSearch is empty or c is empty, At() returns 0.
	/// </returns>
	FUNCTION At2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN At(cSearch,c)


	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, starting at a specified position.
	/// </summary>
	/// <param name="cSearch">The string to search for.</param>
	/// <param name="c">The string to search in.</param>
	/// <param name="dwOff">The position to begin the search with.</param>
	/// <returns>
	/// The position of the first occurrence of <cSearch> within <cTarget> behind the give position.  If <cSearch> is not found, At() returns 0.
	/// If cSearch is empty or c is empty, At3() returns 0.
	/// </returns>
	FUNCTION At3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
		local position := -1 as int
		if ( c != null && cSearch != null && dwOff <= c:Length )
			position := c:IndexOf(cSearch,(int)dwOff-1)
		endif
	RETURN (DWORD) position+1    

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION AtC(cSearch AS STRING,c AS STRING) AS DWORD
		local position := -1 as int
		if ( c != null && cSearch != null )
			position := c:IndexOf(cSearch,System.StringComparison.OrdinalIgnoreCase)
		endif
	RETURN (DWORD) position+1    

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, without regard for case.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION AtC2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN AtC(cSearch,c)  

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
	RETURN (DATE)0   

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
	/// <param name="c">The string from which the even characters shall be extracted.</param>
	/// <returns>
	/// A string which is assembled from the even characters in <c>.
	/// </returns>
	FUNCTION CharEven(c AS STRING) AS STRING
	    local evenChars:=null as string
		if ( !string.IsNullOrEmpty(c) ) 
			//local chars  := c:ToCharArray() as char[]
			local isEven := false as  logic
			local sb     := System.Text.StringBuilder{} as System.Text.StringBuilder

			foreach ch as char in c//hars 
				if isEven
				   sb:Append(ch)
				endif
				isEven := !isEven
			next
			evenChars := sb:ToString()
		endif
	RETURN evenChars

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
	/// <param name="c">The string from which the odd characters shall be extracted.</param>
	/// <returns>
	/// A string which is assembled from the odd characters in <c>.
	/// </returns>
	FUNCTION CharOdd(c AS STRING) AS STRING
	    local oddChars:=null as string
		if ( !string.IsNullOrEmpty(c) ) 
			//local chars  := c:ToCharArray() as char[]
			local isOdd  := true as  logic
			local sb     := System.Text.StringBuilder{} as System.Text.StringBuilder

			foreach ch as char in c//chars 
				if isOdd
				   sb:Append(ch)
				endif
				isOdd := !isOdd
			next
			oddChars := sb:ToString()
		endif
	RETURN oddChars
 

	/// <summary>
	/// Return a character based on its position in a string.
	/// </summary>
	/// <param name="c">The strign to be searched</param>
	/// <param name="nStart">The position of the reuested charachter</param>
	/// <returns>
	/// The character at the given position as a string, if position is beyond the length
	/// of the length of the string null_string is returned.
	/// </returns>
	FUNCTION CharPos(c AS STRING,nStart AS DWORD) AS STRING
		local searchedChar := string.Empty as string
		if ( nStart >= 1 && nStart <= c.Length )
		   searchedChar := c.SubString((int)nStart-1,1)
		endif
	RETURN searchedChar

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
		local parsedDate as DateTime
		if !DateTime.TryParse(cDate,out parsedDate)
		   parsedDate := DateTime.MinValue
		endif
	RETURN __VODate{parsedDate}   

	/// <summary>
	/// Convert an ANSI date string to date format.
	/// </summary>
	/// <param name="cDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION CToDAnsi(cDate AS STRING) AS DATE
		/// THROW NotImplementedException{}
	RETURN (DATE)0   

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
	/// <param name="cStartTime">The starting time in the form HH:mm:ss.</param>
	/// <param name="cEndTime">The ending time in the form HH:mm:ss.</param>
	/// <returns>
	/// The amount of time that has elapsed from cStartTime to cEndTime as a time string in the format hh:mm:ss.
	/// </returns>
	/// <remarks>
	/// The behaviour is not compatible with VO ! Needs to be refactured.
	/// </remarks>
	FUNCTION ElapTime(cStartTime AS STRING,cEndTime AS STRING) AS STRING
		local elapedTime := string.Empty as string
		/// TODO: VO compatibility 
		try
		  elapedTime := DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture);
				.Subtract(DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture));
				.ToString()
		catch ex as Exception
			nop
		end try
	RETURN elapedTime

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
	/// <param name="cFile">The name oif the file</param>
	/// <returns>
	/// True if the file exists, otherwise false
	/// </returns>
	FUNCTION File(cFile AS STRING) AS LOGIC
	return System.IO.File.Exists(cFile)

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
	/// <param name="cSearch">The string to search for.</param>
	/// <param name="c">The string to search in.</param>
	/// <returns>
	/// True if the searched string is in the string.
	/// </returns>
	FUNCTION Instr(cSearch AS STRING,c AS STRING) AS LOGIC
		local isInString := false as logic
		try
			isInString := ( c.IndexOf(cSearch) >= 0 ) 
		//catch ex as Exception
			//nop
		end try
	RETURN isInString   

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
	/// <param name="c">A string from which the left part should be extracted.</param>
	/// <param name="dwLen">The length of the substring which should be extracted.</param>
	/// <returns>
	/// A string of the left first characters in the given length.
	/// </returns>
	FUNCTION Left(c AS STRING,dwLen AS DWORD) AS STRING
		if ( c==null )
			return c
		endif
	RETURN	c:Substring(0,Math.Min(c:Length,(int)dwLen))   

	/// <summary>
	/// Convert the uppercase and mixed case characters in a string to lowercase.
	/// </summary>
	/// <param name="cSource">THe string to be converted.</param>
	/// <returns>
	/// Returns the input string with all characters converted to lowercase.
	/// </returns>
	FUNCTION Lower(cSource AS STRING) AS STRING
		local loweredString := null as string
		if ( !string.IsNullOrEMpty(cSource) )
		   loweredString := cSource.ToLower()
		endif
	RETURN loweredString

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
	/// <param name="c">The string from which leading spaces should be cut off.</param>
	/// <returns>
	/// The input strings without eading spaces.
	/// </returns>
	FUNCTION LTrim(c AS STRING) AS STRING
		local trimmedString := null as string
		if ( !string.IsNullOrEMpty(c) )
		   trimmedString := c.TrimStart()
		endif
	RETURN trimmedString  

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
	/// <param name="c">The string to be search in.</param>
	/// <param name="cSearch">THe string of which its occurance should be counted</param>
	/// <returns>
	/// THe number how often the string to be searched for occurs in the original string.
	/// </returns>
	FUNCTION Occurs(cSearch AS STRING,c AS STRING) AS DWORD
		local countedOccurances:=0 as int
		try
			countedOccurances := c.Split(<string>{ cSearch }, StringSplitOptions.None).Length - 1 
		catch ex as Exception
			nop
		end try
	RETURN (dword)countedOccurances

	/// <summary>
	/// Return the number of times a substring occurs in a string.
	/// </summary>
	/// <param name="c">The string to be search in.</param>
	/// <param name="cSearch">THe string of which its occurance should be counted</param>
	/// <returns>
	/// THe number how often the string to be searched for occurs in the original string.
	/// </returns>
	FUNCTION Occurs2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN Occurs(cSearch,c)   

	/// <summary>
	/// Return the number of times a substring occurs in a string, starting at a specified position.
	/// </summary>
	/// <param name="cSrc"></param>
	/// <param name="c"></param>
	/// <param name="nOffs"></param>
	/// <returns>
	/// </returns>
	FUNCTION Occurs3(cSrc AS STRING,c AS STRING,nOffs AS DWORD) AS DWORD
		local countedOccurances:=0 as dword
		try
			countedOccurances := Occurs(cSrc,c.SubString((int)nOffs-1))
		// catch ex as Exception
		// nop
	    end try
	RETURN countedOccurances  

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
	/// <param name="c">The string to be converted.</param>
	/// <returns>
	/// The converted string according to the CurrentCulture
	/// </returns>
	FUNCTION Proper(c AS STRING) AS STRING
		local convertedString:=null as string 
		if ( !string.IsNullOrEmpty(c) )
		   convertedString := System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(c)
		endif
	RETURN convertedString   

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
	/// <param name="cSearch">THe string to be searched.</param>
	/// <param name="c">The string to be searched in.</param>
	/// <returns>
	/// The right most position of the string to be searched inside the searched string.
	/// </returns>
	FUNCTION RAt(cSearch AS STRING,c AS STRING) AS DWORD
		local rightMost := 0 as dword
		try
			rightMost:= (dword) c.LastIndexOf(cSearch) + 1
		// catch ex Exception
		//    nop
		end try
	RETURN rightMost

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch">THe string to be searched.</param>
	/// <param name="c">The string to be searched in.</param>
	/// <returns>
	/// The right most position of the string to be searched inside the searched string.
	/// </returns>
	FUNCTION RAt2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN RAt2(cSearch,c) 

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="c"></param>
	/// <param name="dwOff"></param>
	/// <returns>
	/// </returns>
	FUNCTION RAt3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
		local rightMost := 0 as dword
		try
			rightMost := RAt(cSearch,c.SubString((int)dwOff-1))+dwOff-1
		// catch ex as Exception
		//    nop
		end try
	RETURN rightMost   

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
	/// Repeat a string a specified number of times.
	/// </summary>
	/// <param name="c">The string to be repeated.</param>
	/// <param name="dwCount">The number of replications.</param>
	/// <returns>
	/// A string which consist of dwCount replications of c.
	/// </returns>
	FUNCTION Repl(c AS STRING,dwCount AS DWORD) AS STRING
		local replString:=null as string
        if (!string.IsNullOrEmpty(c))
            local  builder := System.Text.StringBuilder{c.Length * (int)dwCount} as System.Text.StringBuilder
			local i as int
			for i:=1 upto (int)dwCount
				builder.Append(c)
			next
            replString := builder.ToString()
        endif
	RETURN replString   

	/// <summary>
	/// Repeat a string a specified number of times.
	/// </summary>
	/// <param name="c">The string to be repeated.</param>
	/// <param name="dwCount">The number of replications.</param>
	/// <returns>
	/// A string which consist of dwCount replications of c.
	/// </returns>
	FUNCTION Replicate(c AS STRING,dwCount AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN Repl(c,dwCount)   

	/// <summary>
	/// Return a substring beginning with the rightmost character.
	/// </summary>
	/// <param name="c">The string to extract the rightmost characters from.</param>
	/// <param name="dwLen">The length of the string to extract.</param>
	/// <returns>
	/// Returns the right most part in the given length.
	/// </returns>
	FUNCTION Right(c AS STRING,dwLen AS DWORD) AS STRING
		local rightMostPart := null as string
		try
			rightMostPart := c.SubString(c.Length-(int)dwLen)
		// catch ex as Exception
		//    nop
		end try
	return rightMostPart

	/// <summary>
	/// Remove trailing spaces from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION RTrim(c AS STRING) AS STRING
		local trimmedString := null as string
		if ( !string.IsNullOrEMpty(c) )
		   trimmedString := c.TrimEnd()
		endif
	RETURN trimmedString  
  

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
	/// <param name="c">The string be cloned.</param>
	/// <returns>
	/// A opy of the input string.
	/// </returns>
	FUNCTION SClone(c AS STRING) AS STRING
		local clonedString := null as string
		if ( !string.IsNUllOrEMpty(c) )
		   clonedString := string.Copy(c)
		endif
	RETURN clonedString

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
	/// <param name="c">String which length should be calculated.</param>
	/// <returns>
	/// The length of the string.
	/// </returns>
	FUNCTION SLen(c AS STRING) AS DWORD
		local length := 0 as dword
		try
			if (!string.IsNullOrEmpty(c))
			   length := (dword) c.Length
			endif
		end try
	RETURN length  

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
		local convertedDate := __VODate{} as __VODate
		try
			convertedDate := (__VODate)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
		//catch ex as exeption
		//   nop
		end try
	RETURN convertedDate

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