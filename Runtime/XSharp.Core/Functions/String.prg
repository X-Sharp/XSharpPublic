//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Text
// Array of chars used for the various trim functions
static global trimChars := <char>{ ' ' } as char[]


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

function __StringCompare(strA as string, strB as string) as int
	return String.Compare(strA, strB)

function  __StringEquals(strA as string, strB as string) as logic
	return String.Compare(strA, strB) == 0


function  __StringNotEquals(strA as string, strB as string) as logic
	return String.Compare(strA, strB) != 0


/// <summary>
/// Remove leading and trailing spaces from a string.
/// </summary>
/// <param name="c">The string to be trimmed.</param>
/// <returns>
/// The original string without leading and trailing spaces
/// </returns>
function AllTrim(c as string) as string
	if ( c == null )
		return c
	endif
	return c:Trim(trimChars)


/// <summary>
/// Convert a string of ANSI characters to OEM characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Ansi2Oem(cSource as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

function Ansi2Oem(bSource as BYTE[]) as Byte[]
	/// THROW NotImplementedException{}
	return bSource


/// <summary>
/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Ansi2OemA(cSource as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

internal function _Asc(c as string, lAnsi as LOGIC) as DWORD
	local ascValue := 0 as dword
	local chValue as char
	if ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (DWORD) chValue
		if ascValue > 127
			// Todo: use DOS codepage here
			local encoding as Encoding
			if lAnsi
				encoding := Encoding.Default
			else
				encoding := Encoding.GetEncoding(850) 
			endif
			local buffer as Byte[]
			var chars := <Char> {chValue}
			if encoding:IsSingleByte
				buffer := byte[]{1}
				encoding:GetBytes(chars,0,1,buffer,0)
				ascValue := buffer[1]
			else
				buffer := byte[]{2}
				if encoding:GetBytes(chars,0,1,buffer,0) == 1
					ascValue := buffer[1]
				else
	              IF BitConverter.IsLittleEndian
	                  LOCAL tmp := buffer[1] AS BYTE
		              buffer[1] := buffer[2]
			          buffer[2] := tmp
				   endif
					ascValue := BitConverter.ToUInt16( buffer, 0 )
				endif
			endif
		endif
	endif
	return ascValue

/// <summary>
/// Convert a character to its ASCII value using DOS codepage
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Asc(c as string) as dword
	return _Asc(c, FALSE)


/// <summary>
/// Convert a character to its Unicode ASCII value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function AscW(c as string) as dword
	local ascValue := 0 as dword
	local chValue as char
	if ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (DWORD) chValue
	endif
	return ascValue


/// <summary>
/// Convert a character to its ASCII value using default Windows codepage
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function AscA(c as string) as dword
	return _Asc(c, true)

/// <summary>
/// Return the position of the first occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// The position of the first occurrence of <cSearch> within <cTarget>.  If <cSearch> is not found, At() returns 0.
/// If cSearch is empty or c is empty, At() returns 0.
/// </returns>
function At(cSearch as string,c as string) as dword
	local position := 0 as dword
	if ( c != null .and. cSearch != null )
		position := (dword) c:IndexOf(cSearch, StringComparison.Ordinal) +1
	endif
	return position

/// <summary>
/// Return the position of the first occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// The position of the first occurrence of <cSearch> within <cTarget>.  If <cSearch> is not found, At() returns 0.
/// If cSearch is empty or c is empty, At() returns 0.
/// </returns>
function At2(cSearch as string,c as string) as dword
	return At(cSearch,c)


/// <summary>
/// Return the position of the first occurrence of a substring within a string, starting at a specified position.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <param name="dwOff">The position to begin the search with. This offset is ZERO based</param>
/// <returns>
/// The position of the first occurrence of <cSearch> within <cTarget> behind the give position.  If <cSearch> is not found, At() returns 0.
/// If cSearch is empty or c is empty, At3() returns 0.
/// </returns>
function At3(cSearch as string,c as string,dwOff as dword) as dword
	local position := 0 as dword
	// note dwOffset is ZERO based in VO
	if ( c != null .and. cSearch != null .and. dwOff <= c:Length )
		position := (dword) c:IndexOf(cSearch,(int)dwOff) +1
	endif
	return position

/// <summary>
/// Return the position of the first occurrence of a substring within a string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function AtC(cSearch as string,c as string) as dword
	local position := 0 as dword
	if ( c != null .and. cSearch != null )
		position := (dword) c:IndexOf(cSearch, StringComparison.OrdinalIgnoreCase) +1
	endif
	return position

/// <summary>
/// Return the position of the first occurrence of a substring within a string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function AtC2(cSearch as string,c as string) as dword
	return AtC(cSearch,c)  

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ATCLine(cSearch as string,c as string) as dword
	return AtLine( cSearch:ToUpper(), c:ToUpper() )

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ATCLine2(cSearch as string,c as string) as dword
	return AtLine2( cSearch:ToUpper(), c:ToUpper() )

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ATLine(cSearch as string,c as string) as dword
	LOCAL nPos AS DWORD

	nPos    := At( cSearch, c )
	if (nPos > 0)
		c := Left( c, nPos - 1 )
		nPos := MemLines( c)
	endif
	return nPos


/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ATLine2(cSearch as string,c as string) as dword
	return ATLine(cSearch, c)

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function B64EncFile(c as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
function B64EncString(cIn as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Return the even-numbered characters in a string.
/// </summary>
/// <param name="c">The string from which the even characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the even characters in <c>.
/// </returns>
function CharEven(c as string) as string
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
	return evenChars

/// <summary>
/// Return a string whose odd-numbered characters and even-numbered characters are from 2 different strings.
/// </summary>
/// <param name="c1"></param>
/// <param name="c2"></param>
/// <returns>
/// </returns>
function CharMix(cOdd as string,cEven as string) as string
  LOCAL n1 := 0 AS INT
  LOCAL n2 := 0 AS INT
  LOCAL i1 := 0 AS INT
  LOCAL i2 := 0  AS INT
  LOCAL sb AS StringBuilder 

  IF cEven:Length == 0
     RETURN ""
  ELSE
     sb := StringBuilder{ cOdd:Length * 2 }
     n1 := cOdd:Length - 1
     n2 := cEven:Length - 1

     FOR i1 := 0 UPTO n1
        sb:Append( cOdd[i1] )
        IF i2 > n2
          i2 := 0
        ENDIF
        sb:Append( cEven[i2++] )
     NEXT

     RETURN sb:ToString()
   ENDIF


/// <summary>
/// Return the odd-numbered characters in a string.
/// <param name="c">The string from which the odd characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the odd characters in <c>.
/// </returns>
function CharOdd(c as string) as string
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
	return oddChars


/// <summary>
/// Return a character based on its position in a string.
/// </summary>
/// <param name="c">The strign to be searched</param>
/// <param name="nStart">The position of the reuested charachter</param>
/// <returns>
/// The character at the given position as a string, if position is beyond the length
/// of the length of the string String.Empty is returned.
/// </returns>
function CharPos(c as string, nStart as dword) as string
	local result := string.Empty as string
	if ( nStart >= 1 && nStart <= (dword) c:Length )
		result := c:SubString((int)nStart-1,1)
	endif
	return result



/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
function CHR(dwChar as dword) as string
	var buffer := byte[]{1} 
	buffer[ __ARRAYBASE__] := (byte) dwChar
	return System.Text.Encoding:ASCII:GetString(buffer)


/// <summary>
/// Encrypt or decrypt a string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
function Crypt(cSource as string,cKey as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
function CryptA(cSource as string,cKey as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Decode a file from an e-mail transfer.
/// </summary>
/// <param name="cMailPart"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
unsafe function DecodeBase64(cMailPart as string,hfOut as ptr) as int
	/// THROW NotImplementedException{}
return 0   

/// <summary>
/// </summary>
/// <param name="cSubKey"></param>
/// <returns>
/// </returns>
function DeleteRTRegKey(cSubKey as string) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Replace all soft carriage returns (Chr(141)) in a string with hard carriage returns (Chr(13)).
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function HardCR(c as string) as string
	return c:Replace( (char) 141, (char) 13 )


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Hex2C(c as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Indicate whether a substring is contained in a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// True if the searched string is in the string.
/// </returns>
function Instr(cSearch as string,c as string) as logic
	local result := false as logic
	if cSearch != null .and. c != null
		result := c:IndexOf( cSearch, StringComparison.Ordinal ) > -1
	endif
	return result   



/// <summary>
/// Extract a substring beginning with the first character in a string.
/// </summary>
/// <param name="c">A string from which the left part should be extracted.</param>
/// <param name="dwLen">The length of the substring which should be extracted.</param>
/// <returns>
/// A string of the left first characters in the given length.
/// </returns>function Len(c as string) as dword
function Left(c as string, dwLen as dword) as string
	if ( c!=null )
		if dwLen < c:Length
			c := c:Substring( 0, (int) dwLen )
		else
			nop
		endif
	endif
	return c


/// <summary>
/// Return the length of a string 
/// </summary>
/// <param name="c">The string to measure.</param>
/// <returns>
/// The length of the string..
/// </returns>
function Len(c as string) as dword
	local len := 0 as dword
	if c != null
		len := (dword) c:Length
	endif
	return len



/// <summary>
/// Convert the uppercase and mixed case characters in a string to lowercase.
/// </summary>
/// <param name="cSource">THe string to be converted.</param>
/// <returns>
/// Returns the input string with all characters converted to lowercase.
/// </returns>
function Lower(cSource as string) as string
	if cSource != null
		cSource := cSource:ToLower()
	endif
	return cSource

/// <summary>
/// Convert the uppercase and mixed case characters in a string to lowercase, 
/// changing the contents of the argument as well as the return value.
///
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// Returns the input string with all characters converted to lowercase.
/// </returns>
function LowerA(cSource ref string) as string
	if cSource != null
		cSource := cSource:ToLower()
	endif
	return cSource

/// <summary>
/// Remove leading spaces from a string.
/// </summary>
/// <param name="c">The string from which leading spaces should be cut off.</param>
/// <returns>
/// The input strings without eading spaces.
/// </returns>
function LTrim(c as string) as string
	if (c == null)
		return c
	endif
	return c:TrimStart(trimChars)


/// <summary>
/// Count the number of lines in a string or memo field.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function MemLines(c as string) as dword
	local nLines := 0as dword
	if c != null
		nLines := 1 
		foreach ch as char in c
			if ch == c'\r' 
				nLines++
			endif
		next
	endif
	return nLines

/// <summary>
/// Return the number of times a substring occurs in a string.
/// </summary>
/// <param name="c">The string to be search in.</param>
/// <param name="cSearch">THe string of which its occurance should be counted</param>
/// <returns>
/// THe number how often the string to be searched for occurs in the original string.
/// </returns>
function Occurs(cSearch as string,c as string) as dword
	return Occurs3(cSearch,c, 0)   

/// <summary>
/// Return the number of times a substring occurs in a string.
/// </summary>
/// <param name="c">The string to be search in.</param>
/// <param name="cSearch">THe string of which its occurance should be counted</param>
/// <returns>
/// THe number how often the string to be searched for occurs in the original string.
/// </returns>
function Occurs2(cSearch as string,c as string) as dword
	return Occurs3(cSearch,c, 0)   

/// <summary>
/// Return the number of times a substring occurs in a string, starting at a specified position.
/// </summary>
/// <param name="cSrc"></param>
/// <param name="c"></param>
/// <param name="nOffs"></param>
/// <returns>
/// </returns>
function Occurs3(cSrc as string,c as string,nOffset as dword) as dword
   LOCAL pos AS INT
   LOCAL count AS DWORD

   IF String.IsNullOrEmpty(cSrc) .or. String.IsNullOrEmpty(c)
      RETURN 0
   ENDIF

   IF nOffset > 0
      nOffSet -= 1
   ENDIF

   count := 0
   IF nOffSet < (DWORD) c:Length
      DO WHILE ( pos := c:IndexOf(cSrc, (INT)nOffSet, StringComparison.Ordinal) ) >= 0
         count++
         nOffSet := (DWORD)pos + cSrc:Length
      ENDDO
   ENDIF

   RETURN count

/// <summary>
/// Convert a string of OEM characters to ANSI characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Oem2Ansi(cSource as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

function Oem2Ansi(bSource as BYTE[]) as Byte[]
	/// THROW NotImplementedException{}
	return bSource
	 

/// <summary>
/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Oem2AnsiA(cSource as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Change the first character of each word to uppercase
/// </summary>
/// <param name="c">The string to be converted.</param>
/// <returns>
/// The converted string according to the CurrentCulture
/// </returns>
function Proper(c as string) as string
	local sb as StringBuilder
	local inside as logic
	if c != null
		sb := StringBuilder{c:Length}
		inside := false
		foreach ch as char in c
			var cToAdd := ch
			if inside
				if Char.IsLetterOrDigit(ch)
					cToAdd := char.ToLower(ch)
				else
					inside := false
				endif
			else
				if Char.IsLetterOrDigit(ch)
					cToAdd := char.ToUpper(ch)
					inside := true
				endif
			endif
			sb:append(cToAdd)
		next
		c := sb:ToString()
	endif
	return c

/// <summary>
/// Capitalize a proper name correctly, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ProperA(c REF string) as string
	c := Proper(c)
	return c

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
function QPEncString(cIn as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Return the position of the last occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">THe string to be searched.</param>
/// <param name="c">The string to be searched in.</param>
/// <returns>
/// The right most position of the string to be searched inside the searched string.
/// </returns>
function RAt(cSearch as string,c as string) as dword
	local rightMost := 0 as dword
	if cSearch != null .and. c != null
		rightMost:= (dword) c:LastIndexOf(cSearch, StringComparison.Ordinal) + 1
	endif
	return rightMost

/// <summary>
/// Return the position of the last occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">THe string to be searched.</param>
/// <param name="c">The string to be searched in.</param>
/// <returns>
/// The right most position of the string to be searched inside the searched string.
/// </returns>
function RAt2(cSearch as string,c as string) as dword
	return RAt(cSearch,c) 

/// <summary>
/// Return the position of the last occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <param name="dwOff"></param>
/// <returns>
/// </returns>

function RAt3(cSearch as string,c as string,dwOffSet as dword) as dword
	local nResult := 0 as dword
	if cSearch != null .and. c != null
		if dwOffSet > (dword) c:Length 
			dwOffSet := 0U
		endif
		var cTemp := c:Substring((int) dwOffSet)
		nResult := RAt(cSearch, cTemp)
		if nResult > 0U
			nResult := nResult+dwOffSet
		endif
	endif
	return nResult

/// <summary>
/// Return the line number of the last occurrence of a substring within a multiline string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION RAtLine(cSearch AS STRING, c AS STRING) AS DWORD
  LOCAL nPos AS DWORD
  if cSearch == null .or. c == null
	return 0
  endif
  nPos := RAt(cSearch,c)
  c := Left(c,nPos-1)

  RETURN MemLines(c)

 

/// <summary>
/// Return the line number of the last occurrence of a substring within a multiline string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function RATLine2(cSearch as string,c as string) as dword
	return RatLine(cSearch, c)

/// <summary>
/// Repeat a string a specified number of times.
/// </summary>
/// <param name="c">The string to be repeated.</param>
/// <param name="dwCount">The number of replications.</param>
/// <returns>
/// A string which consist of dwCount replications of c.
/// </returns>
function Repl(c as string,dwCount as dword) as string
	return Replicate(c, dwCount)
/// <summary>
/// Repeat a string a specified number of times.
/// </summary>
/// <param name="c">The string to be repeated.</param>
/// <param name="dwCount">The number of replications.</param>
/// <returns>
/// A string which consist of dwCount replications of c.
/// </returns>
function Replicate(c as string,dwCount as dword) as string
	local cReturn := "" as string
	if dwCount > 0 .and. c != null
		cReturn := System.Text.StringBuilder{}:Insert( 0, c, (int) dwCount ):ToString()
	endif
	return cReturn

/// <summary>
/// Return a substring beginning with the rightmost character.
/// </summary>
/// <param name="c">The string to extract the rightmost characters from.</param>
/// <param name="dwLen">The length of the string to extract.</param>
/// <returns>
/// Returns the right most part in the given length.
/// </returns>
function Right(c as string,dwLen as dword) as string
	if c != null
		if dwLen > c:Length
			nop
		else
			c := c:Substring( c:Length - (int) dwLen, (int) dwLen )
		endif
	endif
	return c


/// <summary>
/// Remove trailing spaces from a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function RTrim(c as string) as string
	if ( c == null )
		return c
	endif
	return c:TrimEnd(trimChars)


/// <summary>
/// Convert single-byte kana characters in a string to their double-byte equivalents.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function SBTODB(c as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Create new character variable with the same characters as the original string.
/// </summary>
/// <param name="c">The string be cloned.</param>
/// <returns>
/// A opy of the input string.
/// </returns>
function SClone(c as string) as string
	local clonedString := null as string
	if (c != null)
		clonedString := String.Copy( c )
	endif
	return clonedString



/// <summary>
/// Create a string of spaces.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
function Space(dwSize as dword) as string
	return string{' ',(int)dwSize}

/// <summary>
/// Return the length of a strongly typed string.
/// </summary>
/// <param name="c">String which length should be calculated.</param>
/// <returns>
/// The length of the string.
/// </returns>
function SLen(c as string) as dword
	local len := 0 as dword
	if c != null
		len := (dword) c:Length
	endif
	return len


/// <summary>
/// Convert a string to Soundex form.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function SoundEx(c as string) as string
    LOCAL sb		AS StringBuilder
    LOCAL iLen		AS INT
    LOCAL ret		AS STRING
    LOCAL i			AS INT
    LOCAL cLastChar AS Char
    LOCAL cSoundExChar AS Char

    IF String.IsNullOrEmpty(c)
        RETURN "0000"
    end if
	cLastChar := (Char) 0
    c	 := c:ToUpper()
    iLen := c:Length - 1
    sb := StringBuilder{}
    sb:Append( c[0] )
    FOR i := 1 TO iLen
        cSoundExChar := _SoundExChar( c[i] )
		if cSoundExChar != '0' .and. cSoundExChar != cLastChar
            sb:Append( cSoundExChar )
        endif
		cLastChar := cSoundExChar
        IF sb:Length == 4
            EXIT
        ENDIF
    NEXT
    ret := sb:ToString()

    RETURN ret:PadRight( 4, '0' ) 

INTERNAL FUNCTION _SoundExChar( c AS Char ) AS Char
    LOCAL ret AS Char
    ret := (char) 0
    switch c
	case c'A' ;	case c'E'; case c'I'; case c'O'; case c'U'; case c'Y'
		ret := c'0'
	case c'H' ;	case c'W'
		ret := c'0'
	case c'B' ;	case c'F';case c'P' ;case c'V'
		ret := c'1'
	case c'C' ;	case c'G';case c'J' ;case c'K';case c'Q';case c'S';case c'X';case c'Z'
		ret := c'2'
	case c'D' ;	case c'T'
		ret := c'3'
	case c'L' 
		ret := c'4'
	case c'M' ;	case c'N'
		ret := c'5'
    CASE c'R'
        ret := '6'
	otherwise
		ret := '0'
	end switch

    RETURN ret



/// <summary>
/// Allows text substitution in strings entered at runtime.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
function StrEvaluate(s as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   




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

function Stuff(c as string,nStart as dword,nToDelete as dword,cIns as string) as string
	local result := cIns as string
	if c != null
		if string.IsNullOrEmpty(cIns)
			cIns := ""
		endif
		if (nStart > 0)
			nStart -= 1
		endif
		local part1 := c as string
		if ( (int) nStart < c:Length )
			part1 := c:Substring(0,(int)nStart)
		endif
		local part2 := "" as string
		var iOffSet := (int) (nStart + nToDelete)
		if ( iOffSet  < c:length )
			part2 := c:Substring( iOffSet )
		endif
		result := part1 + cIns + part2
	endif
	return result

// Note: worker function that accepts negative arguments
// because the untyped Substr() allows these
function __SubStr( c as string, nStart as int, nLength as int ) as string
	if c != null
		if nStart == 0
			nStart := 1
		endif
		
		if nStart < 0
			nStart := c:Length+nStart+1
		endif
		
		if nLength < 0
			nLength := c:Length
		endif
		
		if nStart <= c:Length .and. nStart > 0
			nLength := Math.Min( c:Length - nStart + 1, nLength )
			c := c:Substring( nStart - 1, nLength )
		else
			c := ""
		endif
	endif
	return c

/// <summary>
/// Extract a substring from a string, using strong typing and only two arguments.
/// </summary>
/// <param name="c">The string to be extracted from.</param>
/// <param name="dwStart">The starting position from which the substring should be extracted.</param>
/// <returns>
/// The extracted substring.
/// </returns>
function SubStr2(c as string,dwStart as dword) as string
	if c != null
		c := __SubStr( c, (int) dwStart, ( c:Length - (int) dwStart  + 1 ) )
	endif
	return c

/// <summary>
/// Extract a substring from a string, using strong typing and three required arguments.
/// </summary>
/// <param name="c">The string to be extracted from.</param>
/// <param name="dwStart">The starting position from which the substring should be extracted.</param>
/// <param name="dwLen">The length of the substring to beextracted</param>
/// <returns>
/// The extracted substring in the given length.
/// </returns>
function SubStr3(c as string,dwStart as dword,dwLen as dword) as string
	if c != null
		c := __SubStr( c, (int) dwStart, (int) dwLen )
	endif
	return c



/// <summary>
/// Remove trailing spaces from a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Trim(c as string) as string
	if ( c == null )
		return c
	endif
	return c:TrimEnd(trimChars)



/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
function Upper(cSource as string) as string
	if cSource != null
		cSource := cSource:ToUpper()
	endif
	return cSource


/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
function UpperA(cSource REF string) as string
	if cSource != null
		cSource := cSource:ToUpper()
	endif
	return cSource

/// <summary>
/// </summary>
/// <param name="cLine"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
unsafe function UUDecodeLine(cLine as string,hfOut as ptr) as dword
	/// THROW NotImplementedException{}
return 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function UUEncFile(c as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function UUEncLine(c as string) as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>Determine if the leftmost character in a string is alphabetic.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
function IsAlpha(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      ret := Char.IsLetter( cSource, 0 )
   ENDIF
   RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
function IsAlNum(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      ret := Char.IsLetterOrDigit( cSource, 0 )
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is alphanumeric..</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
function IsAlphaNum(cSource as string) as logic
  return IsAlNum(cSource)


/// <summary>Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is a number from 0 to 9; otherwise FALSE.</returns>
function IsDigit(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      ret := Char.IsDigit(cSource, 0 )
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is a binary digit  (0 or 1)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
function IsBDigit(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      switch cSource[0]
	  case '0'
	  case '1'
		ret := true
	  end switch
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is hex otherwise FALSE.</returns>
function IsXDigit(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   if ! String.IsNullOrEmpty( cSource )
	  local c as char
	  c := cSource[0]
	  if char.IsDigit(c) .or. ;
		(c >= 'A' .and. c <= 'F') .or. ;
		(c >= 'a' .and. c <= 'f')
		ret := true
	  endif
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
function IsSpace(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
 	  ret := char.IsWhiteSpace(cSource,0)
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is uppercase.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is an uppercase letter otherwise, FALSE.</returns>
function IsUpper(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      ret := Char.IsUpper(cSource, 0 )
   ENDIF
   return ret

/// <summary>Determine if the leftmost character in a string is lower.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
function IsLower(cSource as string) as logic
  LOCAL ret := false AS LOGIC
   IF ! String.IsNullOrEmpty( cSource )
      ret := Char.IsLower(cSource, 0 )
   ENDIF
   return ret
