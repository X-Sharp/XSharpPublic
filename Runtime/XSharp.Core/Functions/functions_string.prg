//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Concatenate 2 strings and move trailing spaces to the end
/// </summary>
/// <param name="lhs">The first string .</param>
/// <param name="rhs">The second string.</param>
/// <returns>
/// The combined strings where trailing spaces from the first string are moved to the end.
/// Note: this function is used by the compiler for the "-" operator for strings and/or strings and usuals
/// </returns>

FUNCTION __StringSubtract (lhs as STRING, rhs as STRING) AS STRING
	IF lhs != NULL .and. rhs != null
		VAR len := lhs:Length + rhs:Length
		RETURN (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
	ELSEIF lhs != NULL
		return lhs
	ELSEIF rhs != NULL
		return rhs
	ENDIF
	RETURN String.Empty

/// <summary>
/// Compare 2 strings
/// </summary>
/// <param name="strA">The first string .</param>
/// <param name="strB">The second string.</param>
/// <returns>
/// -1 strA precedes strB in the sort order. 
///  0 strA occurs in the same position as strB in the sort order. 
///  1 strA follows strB in the sort order. 
/// Note this this function should respect SetCollation() and SetInternational() and SetExact()
/// </returns>

FUNCTION __StringCompare(strA AS STRING, strB AS STRING) as INT
	RETURN String.Compare(strA, strB)
   
FUNCTION __StringEquals(strA AS STRING, strB AS STRING) as INT
	RETURN String.Compare(strA, strB) == 0
   
   
FUNCTION __StringNotEquals(strA AS STRING, strB AS STRING) as INT
	RETURN String.Compare(strA, strB) != 0


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
/// Convert a string of ANSI characters to OEM characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2Oem(cSource AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2OemA(cSource AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

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
        local bytBuffer := System.Text.Encoding.GetEncoding(1252):GetBytes(chrBuffer) as byte[]
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
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
FUNCTION B64EncString(cIn AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



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
		//local chars  := c:ToChar__Array() as char[]
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
RETURN String.Empty   

/// <summary>
/// Return the odd-numbered characters in a string.
/// <param name="c">The string from which the odd characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the odd characters in <c>.
/// </returns>
FUNCTION CharOdd(c AS STRING) AS STRING
	local oddChars:=null as string
	if ( !string.IsNullOrEmpty(c) ) 
		//local chars  := c:ToChar__Array() as char[]
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
/// of the length of the string String.Empty is returned.
/// </returns>
FUNCTION CharPos(c AS STRING,nStart AS DWORD) AS STRING
	local searchedChar := string.Empty as string
	if ( nStart >= 1 && nStart <= c:Length )
		searchedChar := c:SubString((int)nStart-1,1)
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
RETURN String.Empty   

/// <summary>
/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION CryptA(cSource AS STRING,cKey AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Decode a file from an e-mail transfer.
/// </summary>
/// <param name="cMailPart"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
unsafe FUNCTION DecodeBase64(cMailPart AS STRING,hfOut AS PTR) AS INT
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
/// Replace all soft carriage returns (Chr(141)) in a string with hard carriage returns (Chr(13)).
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION HardCR(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Hex2C(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

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
		isInString := ( c:IndexOf(cSearch) >= 0 ) 
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
		loweredString := cSource:ToLower()
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
RETURN String.Empty   

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
		trimmedString := c:TrimStart()
	endif
RETURN trimmedString  



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
		countedOccurances := c:Split(<string>{ cSearch }, StringSplitOptions.None):Length - 1 
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
		countedOccurances := Occurs(cSrc,c:SubString((int)nOffs-1))
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
RETURN String.Empty   

/// <summary>
/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Oem2AnsiA(cSource AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

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
		convertedString := System.Globalization.CultureInfo.CurrentCulture:TextInfo:ToTitleCase(c)
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
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
FUNCTION QPEncString(cIn AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   


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
		rightMost:= (dword) c:LastIndexOf(cSearch) + 1
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
		rightMost := RAt(cSearch,c:SubString((int)dwOff-1))+dwOff-1
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
        local  builder := System.Text.StringBuilder{c:Length * (int)dwCount} as System.Text.StringBuilder
		local i as int
		for i:=1 upto (int)dwCount
			builder:Append(c)
		next
        replString := builder:ToString()
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
		rightMostPart := c:SubString(c:Length-(int)dwLen)
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
		trimmedString := c:TrimEnd()
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
RETURN String.Empty   

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
/// Create a string of spaces.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Space(dwSize AS DWORD) AS STRING
RETURN string{' ',(int)dwSize}

/// <summary>
/// Return the length of a strongly typed string.
/// </summary>
/// <param name="c">String which length should be calculated.</param>
/// <returns>
/// The length of the string.
/// </returns>
FUNCTION SLen(c AS STRING) AS DWORD
	local len := 0 as dword
	if (!String.IsNullOrEmpty(c))
		len := (dword) c:Length
	endif
RETURN len  

/// <summary>
/// Convert a string to Soundex form.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION SoundEx(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



/// <summary>
/// Allows text substitution in strings entered at runtime.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
FUNCTION StrEvaluate(s AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   
 



/// <summary>
/// Delete and insert characters in a string.
/// </summary>
/// <param name="c">The string in which the substitution should take place</param>
/// <param name="n">Starting position of the substring to be deleted.</param>
/// <param name="nDel">The number of characters to be deleted.</param>
/// <param name="cIns">The string which should be inserted instead of the deleted characters.</param>
/// <returns>
/// A new string with the substring from the starting position in the given length being subsituted with the insert string.
/// </returns>
FUNCTION Stuff(c AS STRING,n AS DWORD,nDel AS DWORD,cIns AS STRING) AS STRING
	local result := cIns as string
	if !String.IsNullOrEmpty(c)
		var middlePart:=cIns
		if string.IsNullOrEmpty(c)
			middlePart := ""
		endif
		local part1 := c as string
		if ( (int)n <= c:Length )
		    part1 := c:Substring(0,(int)n-1)
		endif
		local part2 := "" as string
		if ( (int)n-1+(int)nDel < c:length )
			part2 := c:Substring((int)n-1+(int)nDel)
		endif
		result := part1+middlePart+part2
	endif
RETURN result

/// <summary>
/// Extract a substring from a string, using strong typing and only two arguments.
/// </summary>
/// <param name="c">The string to be extracted from.</param>
/// <param name="dwStart">The starting position from which the substring should be extracted.</param>
/// <returns>
/// The extracted substring.
/// </returns>
FUNCTION SubStr2(c AS STRING,dwStart AS DWORD) AS STRING
	local result := c as string
	if !String.IsNullOrEmpty(c)
		if (dwStart < c:Length)
		    result := c:Substring((int)dwStart-1)
		else
		    result := ""
		endif
	endif
RETURN result 

/// <summary>
/// Extract a substring from a string, using strong typing and three required arguments.
/// </summary>
/// <param name="c">The string to be extracted from.</param>
/// <param name="dwStart">The starting position from which the substring should be extracted.</param>
/// <param name="dwLen">The length of the substring to beextracted</param>
/// <returns>
/// The extracted substring in the given length.
/// </returns>
FUNCTION SubStr3(c AS STRING,dwStart AS DWORD,dwLen AS DWORD) AS STRING
	local result := c as string
	if !String.IsNullOrEmpty(c)
		if (dwStart < c:Length)
		    result := c:Substring((int)dwStart-1,(int)dwlen)
		else
		    result := ""
		endif
	endif
RETURN result 


/// <summary>
/// Remove trailing spaces from a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Trim(c AS STRING) AS STRING
	local result:=c as string
	if !String.IsNullOrEmpty(c)
		result := result:TrimEnd(' ')
	endif
RETURN result


/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
FUNCTION Upper(cSource AS STRING) AS STRING
	local result:=cSource as string
	if !String.IsNullOrEmpty(cSource)
		result := result:ToUpper()
	endif
RETURN result 

/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
FUNCTION UpperA(cSorce AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="cLine"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
unsafe FUNCTION UUDecodeLine(cLine AS STRING,hfOut AS PTR) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncFile(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncLine(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   
