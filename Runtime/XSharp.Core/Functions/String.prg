//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Text
// Array of chars used for the various trim functions
INTERNAL GLOBAL trimChars := <CHAR>{ ' ' } AS CHAR[]



FUNCTION AllTrim(c AS STRING) AS STRING
	IF ( c == NULL )
		RETURN c
	ENDIF
	RETURN c:Trim(trimChars)



INTERNAL FUNCTION _Asc(c AS STRING, lAnsi AS LOGIC) AS DWORD
	LOCAL ascValue := 0 AS DWORD
	LOCAL chValue AS CHAR
	IF ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (DWORD) chValue
		IF ascValue > 127
			LOCAL encoding AS Encoding
			IF lAnsi
				encoding := Encoding.GetEncoding(RuntimeState.WinCodePage) 
			ELSE
				encoding := Encoding.GetEncoding(RuntimeState.DOSCodePage) 
			ENDIF
			LOCAL buffer AS BYTE[]
			VAR chars := <CHAR> {chValue}
			IF encoding:IsSingleByte
				buffer := BYTE[]{1}
				encoding:GetBytes(chars,0,1,buffer,0)
				ascValue := buffer[0]
			ELSE
				buffer := BYTE[]{2}
				IF encoding:GetBytes(chars,0,1,buffer,0) == 1
					ascValue := buffer[0]
				ELSE
					IF BitConverter.IsLittleEndian
						LOCAL tmp := buffer[0] AS BYTE
						buffer[0] := buffer[1]
						buffer[1] := tmp
					ENDIF
					ascValue := BitConverter.ToUInt16( buffer, 0 )
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	RETURN ascValue

/// <summary>
/// Convert a character to its ASCII value using DOS codepage
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Asc(c AS STRING) AS DWORD
	RETURN _Asc(c, FALSE)


/// <summary>
/// Convert a character to its Unicode ASCII value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION AscW(c AS STRING) AS DWORD
	LOCAL ascValue := 0 AS DWORD
	LOCAL chValue AS CHAR
	IF ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (DWORD) chValue
	ENDIF
	RETURN ascValue


/// <summary>
/// Convert a character to its ASCII value using default Windows codepage
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION AscA(c AS STRING) AS DWORD
	RETURN _Asc(c, TRUE)

/// <summary>
/// Return the position of the first occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// The position of the first occurrence of cSearch within cTarget.  If cSearch is not found, At() returns 0.
/// If cSearch is empty or c is empty, At() returns 0.
/// </returns>
FUNCTION At(cSearch AS STRING,c AS STRING) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( c != NULL .AND. cSearch != NULL )
		position := (DWORD) c:IndexOf(cSearch, StringComparison.Ordinal) +1
	ENDIF
	RETURN position

/// <summary>
/// Return the position of the first occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// The position of the first occurrence of cSearch within cTarget.  If cSearch is not found, At() returns 0.
/// If cSearch is empty or c is empty, At() returns 0.
/// </returns>
FUNCTION At2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN At(cSearch,c)


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
FUNCTION At3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	// note dwOffset is ZERO based in VO
	IF ( c != NULL .AND. cSearch != NULL .AND. dwOff <= c:Length )
		position := (DWORD) c:IndexOf(cSearch,(INT)dwOff) +1
	ENDIF
	RETURN position

/// <summary>
/// Return the position of the first occurrence of a substring within a string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION AtC(cSearch AS STRING,c AS STRING) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( c != NULL .AND. cSearch != NULL )
		position := (DWORD) c:IndexOf(cSearch, StringComparison.OrdinalIgnoreCase) +1
	ENDIF
	RETURN position

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
	RETURN AtLine( cSearch:ToUpper(), c:ToUpper() )

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string, without regard for case.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION ATCLine2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN AtLine2( cSearch:ToUpper(), c:ToUpper() )

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION ATLine(cSearch AS STRING,c AS STRING) AS DWORD
	LOCAL nPos AS DWORD
	IF String.IsNullOrEmpty(c) .OR. String.IsNullOrEmpty(cSearch)
		RETURN 0
	ENDIF
	IF c:StartsWith(cSearch) 
		RETURN 1
	ENDIF
	nPos    := At( cSearch, c )
	IF (nPos > 0)
		c := Left( c, nPos - 1 )
		nPos := MemLines( c)
		IF c:EndsWith(e"\r\n")
			nPos++
		ENDIF
	ENDIF
	RETURN nPos


/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION ATLine2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN ATLine(cSearch, c)

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION B64EncFile(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
FUNCTION B64EncString(cIn AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   



/// <summary>
/// Return an uninitialized string of a specified size.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Buffer(dwSize AS DWORD) AS STRING
	RETURN STRING{'\0', (INT) dwSize}




/// <summary>
/// Return the even-numbered characters in a string.
/// </summary>
/// <param name="c">The string from which the even characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the even characters in c.
/// </returns>
FUNCTION CharEven(c AS STRING) AS STRING
	LOCAL evenChars:=NULL AS STRING
	IF ( !string.IsNullOrEmpty(c) ) 
		//local chars  := c:ToCharArray() as char[]
		LOCAL isEven := FALSE AS  LOGIC
		LOCAL sb     := System.Text.StringBuilder{} AS System.Text.StringBuilder
		
		FOREACH ch AS CHAR IN c//hars 
			IF isEven
				sb:Append(ch)
			ENDIF
			isEven := !isEven
		NEXT
		evenChars := sb:ToString()
	ENDIF
	RETURN evenChars

/// <summary>
/// Return a string whose odd-numbered characters and even-numbered characters are from 2 different strings.
/// </summary>
/// <param name="c1"></param>
/// <param name="c2"></param>
/// <returns>
/// </returns>
FUNCTION CharMix(cOdd AS STRING,cEven AS STRING) AS STRING
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
/// </summary>
/// <param name="c">The string from which the odd characters shall be extracted.</param>
/// <returns>
/// A string which is assembled from the odd characters in c.
/// </returns>
FUNCTION CharOdd(c AS STRING) AS STRING
	LOCAL oddChars:=NULL AS STRING
	IF ( !string.IsNullOrEmpty(c) ) 
		//local chars  := c:ToCharArray() as char[]
		LOCAL isOdd  := TRUE AS  LOGIC
		LOCAL sb     := System.Text.StringBuilder{} AS System.Text.StringBuilder
		
		FOREACH ch AS CHAR IN c//chars 
			IF isOdd
				sb:Append(ch)
			ENDIF
			isOdd := !isOdd
		NEXT
		oddChars := sb:ToString()
	ENDIF
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
FUNCTION CharPos(c AS STRING, nStart AS DWORD) AS STRING
	LOCAL result := string.Empty AS STRING
	IF ( nStart >= 1 && nStart <= (DWORD) c:Length )
		result := c:SubString((INT)nStart-1,1)
	ENDIF
	RETURN result



/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
FUNCTION CHR(dwChar AS DWORD) AS STRING
	VAR buffer := BYTE[]{1} 
	buffer[0] := (BYTE) dwChar
	RETURN System.Text.Encoding:ASCII:GetString(buffer)


/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
FUNCTION ChrA(c AS DWORD) AS STRING
  LOCAL b   AS BYTE
  LOCAL ret AS STRING
   b := (BYTE)( c & 0xFF )  // VO ignores the high 24 bits

   IF b <= 0x7F
      ret := Convert.ToChar( b ):ToString()
   ELSE
      LOCAL encoding AS Encoding

      encoding := Encoding.Default

      LOCAL chars := CHAR[]{ 1 } AS CHAR[]
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
FUNCTION ChrW(c AS DWORD) AS STRING
   IF c > 0xFFFF
      THROW Error.ArgumentError( __ENTITY__, "dwChar", "Number too High")
   ENDIF
   RETURN Convert.ToChar( (INT) ( c & 0xFFFF ) ):ToString()
 
/// <summary>
/// Encrypt or decrypt a string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION Crypt(cSource AS STRING,cKey AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION CryptA(cSource AS STRING,cKey AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// Decode a file from an e-mail transfer.
/// </summary>
/// <param name="cMailPart"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
FUNCTION DecodeBase64(cMailPart AS STRING,hFile AS IntPtr) AS INT
	IF cMailPart == NULL .OR. cMailPart:Length== 0
		RETURN 0
	ENDIF
	VAR aBytes := Convert.FromBase64String( cMailPart )
	RETURN (INT) FWrite3(hFile, aBytes, (DWORD) aBytes:Length)


	
/// <summary>
/// Encode a file for e-mail transfer.
/// </summary>
/// <param name="hfIn"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
FUNCTION EncodeBase64(hFileIn AS IntPtr,hFileOut AS IntPtr) AS INT
	LOCAL nSize AS DWORD
	LOCAL nRead AS DWORD
	// determine file size
	FSeek3(hFileIn, 0, FS_END)
	nSize := Ftell(hFileIn)
	FSeek3(hFileIn, 0, FS_SET)
	VAR aBuffer := BYTE[]{ (INT) nSize}
	nRead := Fread3(hFileIn, aBuffer, nSize)
	IF nRead != nSize
		RETURN 0
	ENDIF
	VAR cContents := Convert.ToBase64String(aBuffer, Base64FormattingOptions.InsertLineBreaks)
	nSize := FWrite(hFileOut, cContents)
	RETURN (INT) nSize





/// <summary>
/// Replace all soft carriage returns (Chr(141)) in a string with hard carriage returns (Chr(13)).
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION HardCR(c AS STRING) AS STRING
	RETURN c:Replace( (CHAR) 141, (CHAR) 13 )


FUNCTION _nibble (c AS CHAR) AS BYTE
	LOCAL b AS BYTE
	SWITCH c
	CASE '0'
	CASE '1'
	CASE '2'
	CASE '3'
	CASE '4'
	CASE '5'
	CASE '6'
	CASE '7'
	CASE '8'
	CASE '9'
		b := (BYTE) c - '0'
	CASE 'A'
	CASE 'B'
	CASE 'C'
	CASE 'D'
	CASE 'E'
	CASE 'F'
		b := (BYTE) c - 'A' + 10
	OTHERWISE
		b := 0
	END SWITCH
	RETURN b
/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Hex2C(c AS STRING) AS STRING
	LOCAL i AS INT
	LOCAL sb AS StringBuilder
	sb := StringBuilder{c:Length}
	i := 0
	DO WHILE i <= c:Length - 2
		VAR b1 := _nibble(c[i])
		VAR b2 := _nibble(c[i+1])
		VAR b  := (b1 << 4) + b2
		sb:Append( chr( b))
		i += 2
	ENDDO
	RETURN sb:ToString()

/// <summary>
/// Indicate whether a substring is contained in a string.
/// </summary>
/// <param name="cSearch">The string to search for.</param>
/// <param name="c">The string to search in.</param>
/// <returns>
/// True if the searched string is in the string.
/// </returns>
FUNCTION Instr(cSearch AS STRING,c AS STRING) AS LOGIC
	LOCAL result := FALSE AS LOGIC
	IF cSearch != NULL .AND. c != NULL
		result := c:IndexOf( cSearch, StringComparison.Ordinal ) > -1
	ENDIF
	RETURN result   



/// <summary>
/// Extract a substring beginning with the first character in a string.
/// </summary>
/// <param name="c">A string from which the left part should be extracted.</param>
/// <param name="dwLen">The length of the substring which should be extracted.</param>
/// <returns>
/// A string of the left first characters in the given length.
/// </returns>function Len(c as string) as dword
FUNCTION Left(c AS STRING, dwLen AS DWORD) AS STRING
	IF ( c!=NULL )
		IF dwLen < c:Length
			c := c:Substring( 0, (INT) dwLen )
		ELSE
			NOP
		ENDIF
	ENDIF
	RETURN c


/// <summary>
/// Convert the uppercase and mixed case characters in a string to lowercase.
/// </summary>
/// <param name="cSource">THe string to be converted.</param>
/// <returns>
/// Returns the input string with all characters converted to lowercase.
/// </returns>
FUNCTION Lower(cSource AS STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToLower()
	ENDIF
	RETURN cSource

/// <summary>
/// Convert the uppercase and mixed case characters in a string to lowercase, 
/// changing the contents of the argument as well as the return value.
///
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// Returns the input string with all characters converted to lowercase.
/// </returns>
FUNCTION LowerA(cSource REF STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToLower()
	ENDIF
	RETURN cSource

/// <summary>
/// Remove leading spaces from a string.
/// </summary>
/// <param name="c">The string from which leading spaces should be cut off.</param>
/// <returns>
/// The input strings without eading spaces.
/// </returns>
FUNCTION LTrim(c AS STRING) AS STRING
	IF (c == NULL)
		RETURN c
	ENDIF
	RETURN c:TrimStart(trimChars)



/// <summary>
/// Return the number of times a substring occurs in a string.
/// </summary>
/// <param name="c">The string to be search in.</param>
/// <param name="cSearch">THe string of which its occurance should be counted</param>
/// <returns>
/// THe number how often the string to be searched for occurs in the original string.
/// </returns>
FUNCTION Occurs(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN Occurs3(cSearch,c, 0)   

/// <summary>
/// Return the number of times a substring occurs in a string.
/// </summary>
/// <param name="c">The string to be search in.</param>
/// <param name="cSearch">THe string of which its occurance should be counted</param>
/// <returns>
/// THe number how often the string to be searched for occurs in the original string.
/// </returns>
FUNCTION Occurs2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN Occurs3(cSearch,c, 0)   

/// <summary>
/// Return the number of times a substring occurs in a string, starting at a specified position.
/// </summary>
/// <param name="cSrc"></param>
/// <param name="c"></param>
/// <param name="nOffs"></param>
/// <returns>
/// </returns>
FUNCTION Occurs3(cSrc AS STRING,c AS STRING,nOffset AS DWORD) AS DWORD
	LOCAL pos AS INT
	LOCAL count AS DWORD
	
	IF String.IsNullOrEmpty(cSrc) .OR. String.IsNullOrEmpty(c)
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
/// Convert a string of ANSI characters to OEM characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2Oem(cSource AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	RETURN Bytes2String(aBytes, iLen)

FUNCTION Ansi2Oem(bSource AS BYTE[]) AS BYTE[]
	RETURN Ansi2Oem(bSource, bSource:Length)


FUNCTION Ansi2Oem(bSource AS BYTE[], iLen AS INT) AS BYTE[]
	LOCAL bDest AS BYTE[]
	bDest := BYTE[]{iLen}
	OemToCharBuffA(bSource, bDest, (DWORD) iLen)
	RETURN bDest

/// <summary>
/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2OemA(cSource REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)  
	RETURN cSource


/// <summary>
/// Convert a string of OEM characters to ANSI characters.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Oem2Ansi(cSource AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	RETURN Bytes2String(aBytes, iLen)

FUNCTION Oem2Ansi(bSource AS BYTE[]) AS BYTE[]
	RETURN Oem2Ansi(bSource, bSource:Length)

FUNCTION Oem2Ansi(bSource AS BYTE[], iLen AS INT) AS BYTE[]
	LOCAL bDest AS BYTE[]
	bDest := BYTE[]{iLen}
	CharToOemBuffA(bSource, bDest, (DWORD) iLen)
	RETURN bDest

/// <summary>
/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION Oem2AnsiBuff(bDest AS BYTE[],bSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	OemToCharBuffA(bSource, bDest, dwCount)
	RETURN bDest

/// <summary>
/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2OemBuff(bDest AS BYTE[],bSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	CharToOemBuffA(bSource, bDest, dwCount)
	RETURN bDest



INTERNAL _DLL FUNCTION CharToOemBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.CharToOemBuffA
INTERNAL _DLL FUNCTION OemToCharBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.OemToCharBuffA


/// <summary>
/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource"></param>
/// <returns>
/// </returns>
FUNCTION Oem2AnsiA(cSource REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)
	RETURN cSource


/// <summary>
/// Change the first character of each word to uppercase
/// </summary>
/// <param name="c">The string to be converted.</param>
/// <returns>
/// The converted string according to the CurrentCulture
/// </returns>
FUNCTION Proper(cString AS STRING) AS STRING
	LOCAL sb AS StringBuilder
	LOCAL inside AS LOGIC
	IF cString != NULL
		sb := StringBuilder{cString:Length}
		inside := FALSE
		FOREACH ch AS CHAR IN cString
			VAR cToAdd := ch
			IF inside
				IF Char.IsLetterOrDigit(ch)
					cToAdd := char.ToLower(ch)
				ELSE
					inside := FALSE
				ENDIF
			ELSE
				IF Char.IsLetterOrDigit(ch)
					cToAdd := char.ToUpper(ch)
					inside := TRUE
				ENDIF
			ENDIF
			sb:append(cToAdd)
		NEXT
		cString := sb:ToString()
	ENDIF
	RETURN cString

/// <summary>
/// Capitalize a proper name correctly, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION ProperA(c REF STRING) AS STRING
	c := Proper(c)
	RETURN c

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
FUNCTION QPEncString(cIn AS STRING) AS STRING
	THROW NotImplementedException{}
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
	LOCAL rightMost := 0 AS DWORD
	IF cSearch != NULL .AND. c != NULL
		rightMost:= (DWORD) c:LastIndexOf(cSearch, StringComparison.Ordinal) + 1
	ENDIF
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
	RETURN RAt(cSearch,c) 

/// <summary>
/// Return the position of the last occurrence of a substring within a string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <param name="dwOff"></param>
/// <returns>
/// </returns>

FUNCTION RAt3(cSearch AS STRING,c AS STRING,dwOffSet AS DWORD) AS DWORD
	LOCAL nResult := 0 AS DWORD
	IF cSearch != NULL .AND. c != NULL
		IF dwOffSet > (DWORD) c:Length 
			dwOffSet := 0U
		ENDIF
		VAR cTemp := c:Substring((INT) dwOffSet)
		nResult := RAt(cSearch, cTemp)
		IF nResult > 0U
			nResult := nResult+dwOffSet
		ENDIF
	ENDIF
	RETURN nResult

/// <summary>
/// Return the line number of the last occurrence of a substring within a multiline string.
/// </summary>
/// <param name="cSearch"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION RAtLine(cSearch AS STRING, c AS STRING) AS DWORD
	LOCAL nPos AS DWORD
	IF cSearch == NULL .OR. c == NULL
		RETURN 0
	ENDIF
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
FUNCTION RATLine2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN RatLine(cSearch, c)

/// <summary>
/// Repeat a string a specified number of times.
/// </summary>
/// <param name="c">The string to be repeated.</param>
/// <param name="dwCount">The number of replications.</param>
/// <returns>
/// A string which consist of dwCount replications of c.
/// </returns>
FUNCTION Repl(c AS STRING,dwCount AS DWORD) AS STRING
	RETURN Replicate(c, dwCount)
/// <summary>
/// Repeat a string a specified number of times.
/// </summary>
/// <param name="c">The string to be repeated.</param>
/// <param name="dwCount">The number of replications.</param>
/// <returns>
/// A string which consist of dwCount replications of c.
/// </returns>
FUNCTION Replicate(c AS STRING,dwCount AS DWORD) AS STRING
	LOCAL cReturn := "" AS STRING
	IF dwCount > 0 .AND. c != NULL
		cReturn := System.Text.StringBuilder{}:Insert( 0, c, (INT) dwCount ):ToString()
	ENDIF
	RETURN cReturn

/// <summary>
/// Return a substring beginning with the rightmost character.
/// </summary>
/// <param name="c">The string to extract the rightmost characters from.</param>
/// <param name="dwLen">The length of the string to extract.</param>
/// <returns>
/// Returns the right most part in the given length.
/// </returns>
FUNCTION Right(c AS STRING,dwLen AS DWORD) AS STRING
	IF c != NULL
		IF dwLen > c:Length
			NOP
		ELSE
			c := c:Substring( c:Length - (INT) dwLen, (INT) dwLen )
		ENDIF
	ENDIF
	RETURN c


/// <summary>
/// Remove trailing spaces from a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION RTrim(c AS STRING) AS STRING
	IF ( c == NULL )
		RETURN c
	ENDIF
	RETURN c:TrimEnd(trimChars)




/// <summary>
/// Create new character variable with the same characters as the original string.
/// </summary>
/// <param name="c">The string be cloned.</param>
/// <returns>
/// A opy of the input string.
/// </returns>
FUNCTION SClone(c AS STRING) AS STRING
	LOCAL clonedString := NULL AS STRING
	IF (c != NULL)
		clonedString := String.Copy( c )
	ENDIF
	RETURN clonedString



/// <summary>
/// Create a string of spaces.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Space(dwSize AS DWORD) AS STRING
	RETURN STRING{' ',(INT)dwSize}

/// <summary>
/// Create a string of spaces.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Space(iSize AS INT) AS STRING
	RETURN STRING{' ',iSize}


/// <summary>
/// Return the length of a strongly typed string.
/// </summary>
/// <param name="c">String which length should be calculated.</param>
/// <returns>
/// The length of the string.
/// </returns>
FUNCTION SLen(c AS STRING) AS DWORD
	LOCAL len := 0 AS DWORD
	IF c != NULL
		len := (DWORD) c:Length
	ENDIF
	RETURN len


/// <summary>
/// Convert a string to Soundex form.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION SoundEx(c AS STRING) AS STRING
	LOCAL sb		AS StringBuilder
	LOCAL iLen		AS INT
	LOCAL ret		AS STRING
	LOCAL i			AS INT
	LOCAL cLastChar AS CHAR
	LOCAL cSoundExChar AS CHAR
	
	IF String.IsNullOrEmpty(c)
		RETURN "0000"
	END IF
	cLastChar := (CHAR) 0
	c	 := c:ToUpper()
	iLen := c:Length - 1
	sb := StringBuilder{}
	sb:Append( c[0] )
	FOR i := 1 TO iLen
		cSoundExChar := _SoundExChar( c[i] )
		IF cSoundExChar != '0' .AND. cSoundExChar != cLastChar
			sb:Append( cSoundExChar )
		ENDIF
		cLastChar := cSoundExChar
		IF sb:Length == 4
			EXIT
		ENDIF
	NEXT
	ret := sb:ToString()
	
	RETURN ret:PadRight( 4, '0' ) 

INTERNAL FUNCTION _SoundExChar( c AS CHAR ) AS CHAR
	LOCAL ret AS CHAR
	ret := (CHAR) 0
	SWITCH c
		CASE c'A' ;	CASE c'E'; CASE c'I'; CASE c'O'; CASE c'U'; CASE c'Y'
			ret := c'0'
		CASE c'H' ;	CASE c'W'
			ret := c'0'
		CASE c'B' ;	CASE c'F';CASE c'P' ;CASE c'V'
			ret := c'1'
		CASE c'C' ;	CASE c'G';CASE c'J' ;CASE c'K';CASE c'Q';CASE c'S';CASE c'X';CASE c'Z'
			ret := c'2'
		CASE c'D' ;	CASE c'T'
			ret := c'3'
		CASE c'L' 
			ret := c'4'
		CASE c'M' ;	CASE c'N'
			ret := c'5'
		CASE c'R'
			ret := '6'
		OTHERWISE
			ret := '0'
	END SWITCH
	
	RETURN ret



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

FUNCTION Stuff(c AS STRING,nStart AS DWORD,nToDelete AS DWORD,cIns AS STRING) AS STRING
	LOCAL result := cIns AS STRING
	IF c != NULL
		IF string.IsNullOrEmpty(cIns)
			cIns := ""
		ENDIF
		IF nStart > 0
			nStart -= 1
		ENDIF
		LOCAL part1 := c AS STRING
		IF  (INT) nStart < c:Length 
			part1 := c:Substring(0,(INT)nStart)
		ENDIF
		LOCAL part2 := "" AS STRING
		VAR iOffSet := (INT) (nStart + nToDelete)
		IF  iOffSet  < c:length 
			part2 := c:Substring( iOffSet )
		ENDIF
		result := part1 + cIns + part2
	ENDIF
	RETURN result

// Note: worker function that accepts negative arguments
// because the untyped Substr() allows these
FUNCTION __SubStr( c AS STRING, nStart AS INT, nLength AS INT ) AS STRING
	IF c != NULL
		IF nStart == 0
			nStart := 1
		ENDIF
		
		IF nStart < 0
			nStart := c:Length+nStart+1
		ENDIF
		
		IF nLength < 0
			nLength := c:Length
		ENDIF
		
		IF nStart <= c:Length .AND. nStart > 0
			nLength := Math.Min( c:Length - nStart + 1, nLength )
			c := c:Substring( nStart - 1, nLength )
		ELSE
			c := ""
		ENDIF
	ENDIF
	RETURN c

/// <summary>
/// Extract a substring from a string, using strong typing and only two arguments.
/// </summary>
/// <param name="c">The string to be extracted from.</param>
/// <param name="dwStart">The starting position from which the substring should be extracted.</param>
/// <returns>
/// The extracted substring.
/// </returns>
FUNCTION SubStr2(c AS STRING,dwStart AS DWORD) AS STRING
	IF c != NULL
		c := __SubStr( c, (INT) dwStart, ( c:Length - (INT) dwStart  + 1 ) )
	ENDIF
	RETURN c

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
	IF c != NULL
		c := __SubStr( c, (INT) dwStart, (INT) dwLen )
	ENDIF
	RETURN c



/// <summary>
/// Remove trailing spaces from a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Trim(c AS STRING) AS STRING
	IF ( c == NULL )
		RETURN c
	ENDIF
	RETURN c:TrimEnd(trimChars)



/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
FUNCTION Upper(cSource AS STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToUpper()
	ENDIF
	RETURN cSource


/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSorce"></param>
/// <returns>
/// </returns>
FUNCTION UpperA(cSource REF STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToUpper()
	ENDIF
	RETURN cSource

/// <summary>
/// </summary>
/// <param name="cLine"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION UUDecodeLine(cLine AS STRING,hfOut AS PTR) AS DWORD
	THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncFile(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncLine(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   



/// <summary>Determine if the leftmost character in a string is alphabetic.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
FUNCTION IsAlpha(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetter( cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
FUNCTION IsAlNum(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetterOrDigit( cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric..</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
FUNCTION IsAlphaNum(cSource AS STRING) AS LOGIC
	RETURN IsAlNum(cSource)


/// <summary>Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is a number from 0 to 9; otherwise FALSE.</returns>
FUNCTION IsDigit(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsDigit(cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is a binary digit  (0 or 1)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
FUNCTION IsBDigit(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		SWITCH cSource[0]
			CASE '0'
			CASE '1'
				ret := TRUE
		END SWITCH
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is hex otherwise FALSE.</returns>
FUNCTION IsXDigit(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		LOCAL c AS CHAR
		c := cSource[0]
		IF char.IsDigit(c) .OR. ;
			(c >= 'A' .AND. c <= 'F') .OR. ;
			(c >= 'a' .AND. c <= 'f')
			ret := TRUE
		ENDIF
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
FUNCTION IsSpace(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := char.IsWhiteSpace(cSource,0)
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is uppercase.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is an uppercase letter otherwise, FALSE.</returns>
FUNCTION IsUpper(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsUpper(cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is lower.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
FUNCTION IsLower(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLower(cSource, 0 )
	ENDIF
	RETURN ret

