//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
using System.Text
// Array of chars used for the various trim functions
internal global trimChars := <char>{ ' ' } as char[]



function AllTrim(c as string) as string
	if ( c == null )
		return c
	endif
	return c:Trim(trimChars)



internal function _Asc(c as string, lAnsi as logic) as dword
	local ascValue := 0 as dword
	local chValue as char
	if ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (dword) chValue
		if ascValue > 127
			local encoding as Encoding
			if lAnsi
				encoding := Encoding.GetEncoding(RuntimeState.WinCodePage) 
			else
				encoding := Encoding.GetEncoding(RuntimeState.DOSCodePage) 
			endif
			local buffer as byte[]
			var chars := <char> {chValue}
			if encoding:IsSingleByte
				buffer := byte[]{1}
				encoding:GetBytes(chars,0,1,buffer,0)
				ascValue := buffer[0]
			else
				buffer := byte[]{2}
				if encoding:GetBytes(chars,0,1,buffer,0) == 1
					ascValue := buffer[0]
				else
					if BitConverter.IsLittleEndian
						local tmp := buffer[0] as byte
						buffer[0] := buffer[1]
						buffer[1] := tmp
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
	return _Asc(c, false)


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
		ascValue := (dword) chValue
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
/// The position of the first occurrence of cSearch within cTarget.  If cSearch is not found, At() returns 0.
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
/// The position of the first occurrence of cSearch within cTarget.  If cSearch is not found, At() returns 0.
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
/// The position of the first occurrence of cSearch within cTarget behind the give position.  If cSearch is not found, At() returns 0.
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
	IF String.IsNullOrEmpty(c) .or. String.IsNullOrEmpty(cSearch)
		RETURN 0
	endif
	IF c:StartsWith(cSearch) 
		return 1
	endif
	nPos    := At( cSearch, c )
	if (nPos > 0)
		c := Left( c, nPos - 1 )
		nPos := MemLines( c)
		IF c:EndsWith(e"\r\n")
			nPos++
		endif
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
	THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
function B64EncString(cIn as string) as string
	THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Return an uninitialized string of a specified size.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Buffer(dwSize AS DWORD) AS STRING
	RETURN String{'\0', (int) dwSize}




/// <summary>
/// Return the even-numbered characters in a string.
/// </summary>
/// <param name="c">The string from which the even characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the even characters in c.
/// </returns>
function CharEven(c as string) as string
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
	return evenChars

/// <summary>
/// Return a string whose odd-numbered characters and even-numbered characters are from 2 different strings.
/// </summary>
/// <param name="c1"></param>
/// <param name="c2"></param>
/// <returns>
/// </returns>
function CharMix(cOdd as string,cEven as string) as string
	local n1 := 0 as int
	local n2 := 0 as int
	local i1 := 0 as int
	local i2 := 0  as int
	local sb as StringBuilder 
	
	if cEven:Length == 0
		return ""
	else
		sb := StringBuilder{ cOdd:Length * 2 }
		n1 := cOdd:Length - 1
		n2 := cEven:Length - 1
		
		for i1 := 0 upto n1
			sb:Append( cOdd[i1] )
			if i2 > n2
				i2 := 0
			endif
			sb:Append( cEven[i2++] )
		next
		
		return sb:ToString()
	endif


/// <summary>
/// Return the odd-numbered characters in a string.
/// </summary>
/// <param name="c">The string from which the odd characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the odd characters in c.
/// </returns>
function CharOdd(c as string) as string
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
	buffer[0] := (byte) dwChar
	return System.Text.Encoding:ASCII:GetString(buffer)


/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
function ChrA(c as dword) as string
  LOCAL b   AS BYTE
  local ret as STRING
   b := (BYTE)( c & 0xFF )  // VO ignores the high 24 bits

   IF b <= 0x7F
      ret := Convert.ToChar( b ):ToString()
   ELSE
      LOCAL encoding AS Encoding

      encoding := Encoding.Default

      LOCAL chars := Char[]{ 1 } AS Char[]
      LOCAL bytes := BYTE[]{ 1 } AS BYTE[]
      LOCAL decoder := encoding:GetDecoder() AS Decoder
      bytes[__ARRAYBASE__] := b
      decoder:GetChars( bytes, 0, 1, chars, 0 )
      ret := chars[__ARRAYBASE__]:ToString()

   ENDIF

   RETURN ret


/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
function ChrW(c as dword) as string
   IF c > 0xFFFF
      throw Error.ArgumentError( __ENTITY__, "dwChar", "Number too High")
   ENDIF
   RETURN Convert.ToChar( (INT) ( c & 0xFFFF ) ):ToString()
 
/// <summary>
/// Encrypt or decrypt a string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
function Crypt(cSource as string,cKey as string) as string
	THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
function CryptA(cSource as string,cKey as string) as string
	THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Decode a file from an e-mail transfer.
/// </summary>
/// <param name="cMailPart"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
function DecodeBase64(cMailPart as string,hFile as IntPtr) as int
	if cMailPart == null .or. cMailPart:Length== 0
		return 0
	endif
	var aBytes := Convert.FromBase64String( cMailPart )
	return (INT) FWrite3(hFile, aBytes, (DWORD) aBytes:Length)


	
/// <summary>
/// Encode a file for e-mail transfer.
/// </summary>
/// <param name="hfIn"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
function EncodeBase64(hFileIn as IntPtr,hFileOut as IntPtr) as int
	local nSize as dword
	local nRead as dword
	// determine file size
	FSeek3(hFileIn, 0, FS_END)
	nSize := Ftell(hFileIn)
	FSeek3(hFileIn, 0, FS_SET)
	var aBuffer := byte[]{ (int) nSize}
	nRead := Fread3(hFileIn, aBuffer, nSize)
	if nRead != nSize
		return 0
	endif
	var cContents := Convert.ToBase64String(aBuffer, Base64FormattingOptions.InsertLineBreaks)
	nSize := FWrite(hFileOut, cContents)
	return (int) nSize





/// <summary>
/// Replace all soft carriage returns (Chr(141)) in a string with hard carriage returns (Chr(13)).
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function HardCR(c as string) as string
	return c:Replace( (char) 141, (char) 13 )


function _nibble (c as char) as byte
	local b as byte
	switch c
	case '0'
	case '1'
	case '2'
	case '3'
	case '4'
	case '5'
	case '6'
	case '7'
	case '8'
	case '9'
		b := (byte) c - '0'
	case 'A'
	case 'B'
	case 'C'
	case 'D'
	case 'E'
	case 'F'
		b := (byte) c - 'A' + 10
	OTHERWISE
		b := 0
	end switch
	return b
/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Hex2C(c as string) as string
	local i as int
	local sb as StringBuilder
	sb := StringBuilder{c:Length}
	i := 0
	do while i <= c:Length - 2
		var b1 := _nibble(c[i])
		var b2 := _nibble(c[i+1])
		var b  := (b1 << 4) + b2
		sb:Append( chr( b))
		i += 2
	enddo
	return sb:ToString()

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
	local pos as int
	local count as dword
	
	if String.IsNullOrEmpty(cSrc) .or. String.IsNullOrEmpty(c)
		return 0
	endif
	
	if nOffset > 0
		nOffSet -= 1
	endif
	
	count := 0
	if nOffSet < (dword) c:Length
		do while ( pos := c:IndexOf(cSrc, (int)nOffSet, StringComparison.Ordinal) ) >= 0
			count++
			nOffSet := (dword)pos + cSrc:Length
		enddo
	endif
	
	return count

/// <summary>
/// Convert a string of ANSI characters to OEM characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Ansi2Oem(cSource as string) as string
	local aBytes as byte[]
	local iLen	 as int
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	return Bytes2String(aBytes, iLen)

function Ansi2Oem(bSource as byte[]) as byte[]
	return Ansi2Oem(bSource, bSource:Length)


function Ansi2Oem(bSource as byte[], iLen as INT) as byte[]
	local bDest as byte[]
	bDest := byte[]{iLen}
	OemToCharBuffA(bSource, bDest, (dword) iLen)
	return bDest

/// <summary>
/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Ansi2OemA(cSource ref string) as string
	local aBytes as byte[]
	local iLen	 as int
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)  
	return cSource


/// <summary>
/// Convert a string of OEM characters to ANSI characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Oem2Ansi(cSource as string) as string
	local aBytes as byte[]
	local iLen	 as int
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	return Bytes2String(aBytes, iLen)

function Oem2Ansi(bSource as byte[]) as byte[]
	return Oem2Ansi(bSource, bSource:Length)

function Oem2Ansi(bSource as byte[], iLen as int) as byte[]
	local bDest as byte[]
	bDest := byte[]{iLen}
	CharToOemBuffA(bSource, bDest, (dword) iLen)
	return bDest

/// <summary>
/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function Oem2AnsiBuff(bDest as byte[],bSource as byte[],dwCount as dword) as byte[]
	OemToCharBuffA(bSource, bDest, dwCount)
	return bDest

/// <summary>
/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function Ansi2OemBuff(bDest as byte[],bSource as byte[],dwCount as dword) as byte[]
	CharToOemBuffA(bSource, bDest, dwCount)
	return bDest



INTERNAL _DLL FUNCTION CharToOemBuffA( lpszSrc AS byte[], lpszDst AS byte[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.CharToOemBuffA
INTERNAL _DLL FUNCTION OemToCharBuffA( lpszSrc AS byte[], lpszDst AS byte[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.OemToCharBuffA


/// <summary>
/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
function Oem2AnsiA(cSource REF string) as string
	local aBytes as byte[]
	local iLen	 as int
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)
	return cSource


/// <summary>
/// Change the first character of each word to uppercase
/// </summary>
/// <param name="c">The string to be converted.</param>
/// <returns>
/// The converted string according to the CurrentCulture
/// </returns>
function Proper(cString as string) as string
	local sb as StringBuilder
	local inside as logic
	if cString != null
		sb := StringBuilder{cString:Length}
		inside := false
		foreach ch as char in cString
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
		cString := sb:ToString()
	endif
	return cString

/// <summary>
/// Capitalize a proper name correctly, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function ProperA(c ref string) as string
	c := Proper(c)
	return c

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
function QPEncString(cIn as string) as string
	THROW NotImplementedException{}
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
function RAtLine(cSearch as string, c as string) as dword
	local nPos as dword
	if cSearch == null .or. c == null
		return 0
	endif
	nPos := RAt(cSearch,c)
	c := Left(c,nPos-1)
	
	return MemLines(c)



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
/// Create a string of spaces.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
function Space(iSize as int) as string
	return string{' ',iSize}


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
	local sb		as StringBuilder
	local iLen		as int
	local ret		as string
	local i			as int
	local cLastChar as char
	local cSoundExChar as char
	
	if String.IsNullOrEmpty(c)
		return "0000"
	end if
	cLastChar := (char) 0
	c	 := c:ToUpper()
	iLen := c:Length - 1
	sb := StringBuilder{}
	sb:Append( c[0] )
	for i := 1 to iLen
		cSoundExChar := _SoundExChar( c[i] )
		if cSoundExChar != '0' .and. cSoundExChar != cLastChar
			sb:Append( cSoundExChar )
		endif
		cLastChar := cSoundExChar
		if sb:Length == 4
			exit
		endif
	next
	ret := sb:ToString()
	
	return ret:PadRight( 4, '0' ) 

internal function _SoundExChar( c as char ) as char
	local ret as char
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
		case c'R'
			ret := '6'
		otherwise
			ret := '0'
	end switch
	
	return ret



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
		if nStart > 0
			nStart -= 1
		endif
		local part1 := c as string
		if  (int) nStart < c:Length 
			part1 := c:Substring(0,(int)nStart)
		endif
		local part2 := "" as string
		var iOffSet := (int) (nStart + nToDelete)
		if  iOffSet  < c:length 
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
function UpperA(cSource ref string) as string
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
	THROW NotImplementedException{}
return 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function UUEncFile(c as string) as string
	THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function UUEncLine(c as string) as string
	THROW NotImplementedException{}
	return String.Empty   



/// <summary>Determine if the leftmost character in a string is alphabetic.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
function IsAlpha(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetter( cSource, 0 )
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is alphanumeric.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
function IsAlNum(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetterOrDigit( cSource, 0 )
	endif
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
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := Char.IsDigit(cSource, 0 )
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is a binary digit  (0 or 1)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
function IsBDigit(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		switch cSource[0]
			case '0'
			case '1'
				ret := true
		end switch
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is hex otherwise FALSE.</returns>
function IsXDigit(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		local c as char
		c := cSource[0]
		if char.IsDigit(c) .or. ;
			(c >= 'A' .and. c <= 'F') .or. ;
			(c >= 'a' .and. c <= 'f')
			ret := true
		endif
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
function IsSpace(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := char.IsWhiteSpace(cSource,0)
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is uppercase.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is an uppercase letter otherwise, FALSE.</returns>
function IsUpper(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := Char.IsUpper(cSource, 0 )
	endif
	return ret

/// <summary>Determine if the leftmost character in a string is lower.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
function IsLower(cSource as string) as logic
	local ret := false as logic
	if ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLower(cSource, 0 )
	endif
	return ret

