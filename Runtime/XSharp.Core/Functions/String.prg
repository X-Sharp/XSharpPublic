//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Text
// Array of chars used for the various trim functions
INTERNAL GLOBAL trimChars := <CHAR>{ ' ' } AS CHAR[]

/// <summary>Remove leading and trailing spaces from a string. </summary>
/// <param name="cString">The string to trim.</param>
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <returns>A trimmed string, with leading and trailing spaces removed.</returns>
FUNCTION AllTrim(cString AS STRING) AS STRING
	IF ( cString == NULL )
		RETURN cString
	ENDIF
	RETURN cString:Trim(trimChars)


/// <summary>
/// Convert a character to its ASCII value using default Windows codepage
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>

FUNCTION Asc(c AS STRING) AS DWORD
	LOCAL ascValue := 0 AS DWORD
	LOCAL chValue AS CHAR
	IF ( !String.IsNullOrEmpty(c) ) 
		chValue := c[0]
		ascValue := (DWORD) chValue
		IF ascValue > 127
			LOCAL encoding AS Encoding
			encoding := StringHelpers.WinEncoding
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
		IF c:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) c:IndexOf(cSearch, StringComparison.Ordinal) +1
		END IF
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
/// <param name="dwOff">The position to begin the search with. <em>This offset is ZERO based</em></param>
/// <returns>
/// The position of the first occurrence of cSearch within cTarget behind the give position.  If cSearch is not found, At() returns 0.
/// If cSearch is empty or c is empty, At3() returns 0.
/// </returns>
FUNCTION At3(cSearch AS STRING,c AS STRING,dwOff AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	// note dwOffset is ZERO based in VO
	IF ( c != NULL .AND. cSearch != NULL .AND. dwOff <= c:Length )
		IF c:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) c:IndexOf(cSearch,(INT)dwOff, StringComparison.Ordinal) +1
		ENDIF
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
		IF c:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) c:IndexOf(cSearch, StringComparison.OrdinalIgnoreCase) +1
		ENDIF
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
	RETURN AtLine(cSearch, c)

/// <summary>This function is not implemented yet</summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION B64EncFile(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>This function is not implemented yet</summary>
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
	IF ( nStart >= 1 .AND. nStart <= (DWORD) c:Length )
		result := c:SubString((INT)nStart-1,1)
	ENDIF
	RETURN result


/// <summary>
/// Convert an ASCII code to a character value.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>A single character that corresponds to the value of the parameter</returns>
/// <remarks>
/// The value of dwChar must be between 0 and 255<br/>
/// The return value of Chr() in XSharp depends on the setting of SetAnsi().<br/>
/// When SetAnsi() = TRUE then the active windows Ansi codepage is used to calculate the character.<br/>
/// When SetAnsi() = FALSE then the active windows Oem codepage is used to calculate the character.<br/>
/// </remarks>
FUNCTION Chr(c AS DWORD) AS STRING
  LOCAL b   AS BYTE
  LOCAL ret AS STRING
   b := (BYTE)( c & 0xFF )  // VO ignores the high 24 bits

   IF b <= 0x7F
      ret := Convert.ToChar( b ):ToString()
   ELSE
      LOCAL encoding AS Encoding
      IF RuntimeState.Ansi
        encoding := StringHelpers.WinEncoding
      ELSE
        encoding := StringHelpers.DosEncoding
      ENDIF

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
	nSize := FTell(hFileIn)
	FSeek3(hFileIn, 0, FS_SET)
	VAR aBuffer := BYTE[]{ (INT) nSize}
	nRead := FRead3(hFileIn, aBuffer, nSize)
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


INTERNAL FUNCTION _nibble (c AS CHAR) AS BYTE
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
		b := (BYTE) (c - '0')
	CASE 'A'
	CASE 'B'
	CASE 'C'
	CASE 'D'
	CASE 'E'
	CASE 'F'
		b := (BYTE) (c - 'A' + 10)
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
		sb:Append( Chr( (DWORD) b))
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
	IF cSearch != NULL .AND. c != NULL .AND. cSearch:Length != 0
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
/// <param name="cSource">The string to be converted.</param>
/// <returns>Returns the input string with all characters converted to lowercase.</returns>
/// <seealso cref='M:XSharp.Core.Functions.LowerA(System.String@)'>LowerA</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Upper(System.String)'>Upper</seealso>
FUNCTION Lower(cSource AS STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToLower()
	ENDIF
	RETURN cSource

/// <summary>
/// Convert the uppercase and mixed case characters in a string to lowercase, 
/// changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource">The string to be converted.</param>
/// <returns>Returns the input string with all characters converted to lowercase.</returns>
/// <seealso cref='M:XSharp.Core.Functions.Lower(System.String)'>Lower</seealso>
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
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
FUNCTION LTrim(c AS STRING) AS STRING
	IF (c == NULL)
		RETURN c
	ENDIF
	RETURN c:TrimStart(trimChars)



/// <summary>
/// Return the number of times a substring occurs in a string.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.Occurs3(System.String,System.String,System.UInt32)" />
FUNCTION Occurs(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN Occurs3(cSearch,cTarget, 0)   

/// <inheritdoc cref="M:XSharp.Core.Functions.Occurs(System.String,System.String)" />
FUNCTION Occurs2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN Occurs3(cSearch,cTarget, 0)   

/// <summary>
/// Return the number of times a substring occurs in a string, starting at a specified position.
/// </summary>
/// <param name="cSearch">The substring to search for. </param>
/// <param name="cTarget">The string in which to search. </param>
/// <param name="nOffset">The position in the string at which to start searching.  A value of zero (0) specifies the first byte. </param>
/// <returns>
/// The number of times that the search string occurs in the original string.
/// </returns>
FUNCTION Occurs3(cSearch AS STRING,cTarget AS STRING,nOffset AS DWORD) AS DWORD
	LOCAL pos AS INT
	LOCAL count AS DWORD
	
	IF String.IsNullOrEmpty(cSearch) .OR. String.IsNullOrEmpty(cTarget)
		RETURN 0
	ENDIF
	
	IF nOffset > 0
		nOffSet -= 1
	ENDIF
	
	count := 0
	IF nOffSet < (DWORD) cTarget:Length
		DO WHILE ( pos := cTarget:IndexOf(cSearch, (INT)nOffSet, StringComparison.Ordinal) ) >= 0
			count++
			nOffSet := (DWORD)(pos + cSearch:Length)
		ENDDO
	ENDIF
	
	RETURN count

/// <overloads>
/// <summary>Convert a string of ANSI characters to OEM characters.</summary>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// </overloads>
/// <summary>Convert a string of ANSI characters to OEM characters.</summary>
/// <param name="cSource">String in Ansi format</param>
/// <returns>String converted to Unicode
/// </returns>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Ansi2Oem(cSource AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	RETURN Bytes2String(aBytes, iLen)

/// <summary>
/// Convert an array of bytes from ANSI to OEM.
/// </summary>
/// <param name="bSource">String in Ansi format</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.Ansi2Oem(System.String)" />
FUNCTION Ansi2Oem(bSource AS BYTE[]) AS BYTE[]
	RETURN Ansi2Oem(bSource, bSource:Length)


/// <param name="iLen">Length of the source array</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.Ansi2Oem(System.Byte[])" />
FUNCTION Ansi2Oem(bSource AS BYTE[], iLen AS INT) AS BYTE[]
	LOCAL bDest AS BYTE[]
	bDest := BYTE[]{iLen}
	CharToOemBuffA(bSource, bDest, (DWORD) iLen)
	RETURN bDest

/// <overloads>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// </overloads>
/// <summary>
/// Convert a string of ANSI characters to OEM characters, changing the contents of the original string as well as the returned string.
/// </summary>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Ansi2OemA(cSource REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Ansi2Oem(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)  
	RETURN cSource


/// <summary>
/// Convert an array of ANSI characters to OEM characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="bSource">A byte array that contains the string to convert</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.Ansi2OemA(System.String@)" />
FUNCTION Ansi2OemA(bSource AS BYTE[]) AS VOID
	LOCAL bDest AS BYTE[]
	bDest := Ansi2Oem(bSource, bSource:Length)
	System.Array.Copy(bDest, bSource, bSource:Length)
    RETURN


/// <overloads>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
/// </overloads>
/// <summary>
/// Convert a string of OEM characters to ANSI characters.
/// </summary>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <param name="cSource">String in OEM format</param>
/// <returns>String converted to Ansi</returns>
/// <seealso cref="M:XSharp.Core.Functions.Oem2AnsiBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Oem2Ansi(cSource AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	RETURN Bytes2String(aBytes, iLen)

/// <summary>
/// Convert an array of OEM characters to ANSI characters.
/// </summary>
/// <param name="bSource">A byte array that contains the string to convert</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.Oem2Ansi(System.String)" />
FUNCTION Oem2Ansi(bSource AS BYTE[]) AS BYTE[]
	RETURN Oem2Ansi(bSource, bSource:Length)

/// <overloads>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// </overloads>
/// <summary>
/// Convert a string of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
FUNCTION Oem2AnsiA(cSource REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cSource:Length
	aBytes := String2Bytes(cSource)
	aBytes := Oem2Ansi(aBytes, iLen)
	cSource := Bytes2String(aBytes, iLen)
	RETURN cSource

/// <summary>
/// Convert an array of OEM characters to ANSI characters, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="bSource">A byte array that contains the string to convert</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.Oem2AnsiA(System.String@)" />
FUNCTION Oem2AnsiA(bSource AS BYTE[]) AS VOID
	LOCAL bDest AS BYTE[]
	bDest := Oem2Ansi(bSource, bSource:Length)
	System.Array.Copy(bDest, bSource, bSource:Length)
    RETURN
	

/// <inheritdoc cref="M:XSharp.Core.Functions.Oem2Ansi(System.Byte[])" />
/// <param name="iLen">The number of characters to convert</param>
/// <returns>String converted to Ansi</returns>
/// <seealso cref="M:XSharp.Core.Functions.Ansi2Oem(System.Byte[],System.Int32)" />
FUNCTION Oem2Ansi(bSource AS BYTE[], iLen AS INT) AS BYTE[]
	LOCAL bDest AS BYTE[]
	bDest := BYTE[]{iLen}
	OemToCharBuffA(bSource, bDest, (DWORD) iLen)
	RETURN bDest

/// <summary>
/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
/// </summary>
/// <param name="bDest">A byte array that will contain the converted characters</param>
/// <param name="bSource">A byte array that contains the characters to convert</param>
/// <param name="dwCount">The number of characters to convert</param>
/// <returns>The byte array with the converted characters</returns>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
FUNCTION Oem2AnsiBuff(bDest AS BYTE[],bSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	OemToCharBuffA(bSource, bDest, dwCount)
	RETURN bDest

/// <summary>
/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
/// </summary>
/// <param name="bDest">A byte array that will contain the converted characters</param>
/// <param name="bSource">A byte array that contains the characters to convert</param>
/// <param name="dwCount">The number of characters to convert</param>
/// <returns>The byte array with the converted characters</returns>
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="M:XSharp.Core.Functions.Oem2AnsiBuff(System.Byte[],System.Byte[],System.UInt32)" />
FUNCTION Ansi2OemBuff(bDest AS BYTE[],bSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	CharToOemBuffA(bSource, bDest, dwCount)
	RETURN bDest

INTERNAL _DLL FUNCTION CharToOemBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.CharToOemBuffA
INTERNAL _DLL FUNCTION OemToCharBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.OemToCharBuffA




/// <summary>
/// Change the first character of each word to uppercase
/// </summary>
/// <param name="cString">The string to be converted.</param>
/// <returns>
/// The converted string according to the CurrentCulture
/// </returns>
/// <seealso cref="M:XSharp.Core.Functions.ProperA(System.String@)" />
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
/// <param name="cText"></param>
/// <returns>
/// The converted string according to the CurrentCulture
/// </returns>
/// <inheritdoc cref="M:XSharp.Core.Functions.Proper(System.String)" />
/// <seealso cref="M:XSharp.Core.Functions.Proper(System.String)" />
FUNCTION ProperA(cText REF STRING) AS STRING
	cText := Proper(cText)
	RETURN cText

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
/// <seealso cref="M:XSharp.Core.Functions.RAt3(System.String,System.String,System.UInt32)" />
/// <seealso cref="M:XSharp.Core.Functions.RAt2(System.String,System.String)" />
/// <seealso cref="M:XSharp.Core.Functions.RAt(System.String,System.String)" />
FUNCTION RAt(cSearch AS STRING,c AS STRING) AS DWORD
	LOCAL rightMost := 0 AS DWORD
	IF cSearch != NULL .AND. c != NULL
		IF c:Length != 0 .AND. cSearch:Length != 0
			rightMost:= (DWORD) c:LastIndexOf(cSearch, StringComparison.Ordinal) + 1
		ENDIF
	ENDIF
	RETURN rightMost

/// <inheritdoc cref="M:XSharp.Core.Functions.RAt(System.String,System.String)" />
FUNCTION RAt2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN RAt(cSearch,c) 

/// <param name="dwOffSet">The position in the string at which to start searching.  A value of zero (0) specifies the first byte.</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.RAt(System.String,System.String)" />
FUNCTION RAt3(cSearch AS STRING,c AS STRING,dwOffSet AS DWORD) AS DWORD
	LOCAL nResult := 0 AS DWORD
	IF cSearch != NULL .AND. c != NULL
		IF c:Length != 0 .AND. cSearch:Length != 0
			IF dwOffSet > (DWORD) c:Length 
				dwOffSet := 0U
			ENDIF
			VAR cTemp := c:Substring((INT) dwOffSet)
			nResult := RAt(cSearch, cTemp)
			IF nResult > 0U
				nResult := nResult+dwOffSet
			ENDIF
		ENDIF
	ENDIF
	RETURN nResult

/// <summary>
/// Return the line number of the last occurrence of a substring within a multiline string.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.RAt(System.String,System.String)" />
FUNCTION RAtLine(cSearch AS STRING, c AS STRING) AS DWORD
	LOCAL nPos AS DWORD
	IF cSearch == NULL .OR. c == NULL .OR. cSearch:Length == 0 .OR. c:Length == 0
		RETURN 0
	ENDIF
	nPos := RAt(cSearch,c)
	c := Left(c,nPos-1)
	
	RETURN MemLines(c)



/// <inheritdoc cref="M:XSharp.Core.Functions.RAtLine(System.String,System.String)" />
FUNCTION RATLine2(cSearch AS STRING,c AS STRING) AS DWORD
	RETURN RAtLine(cSearch, c)

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

/// <inheritdoc cref="M:XSharp.Core.Functions.Repl(System.String,System.UInt32)" />
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
/// <param name="c">The string to trim.</param>
/// <returns>The string with the trailing spaces removed.  If the source string  is a NULL_STRING or all spaces, RTrim() returns a NULL_STRING.</returns>
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
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
/// A copy of the input string.
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
/// <param name="count">The number of spaces to return.</param>
/// <returns>A string of spaces.</returns>
FUNCTION Space(count AS DWORD) AS STRING
	RETURN STRING{' ',(INT)count}

/// <inheritdoc cref="M:XSharp.Core.Functions.Space(System.UInt32)" />
FUNCTION Space(count AS INT) AS STRING
	RETURN STRING{' ',count}

/// <summary>
/// Return the length of a strongly typed string.
/// </summary>
/// <param name="c">String which length should be calculated.</param>
/// <returns>
/// The length of the string.
/// </returns>
/// <remarks>
/// The compiler will replace calls to SLen() as much as possible with a direct call to the Length property of a string.
/// </remarks>
FUNCTION SLen(c AS STRING) AS DWORD
	LOCAL len := 0 AS DWORD
	IF c != NULL
		len := (DWORD) c:Length
	ENDIF
	RETURN len


/// <summary>
/// Convert a string to Soundex form.
/// </summary>
/// <param name="c"> The string to convert. </param>
/// <returns>A 4-digit string starting with an alphabetic character and ending with three digits.</returns>
/// <remarks>
/// Soundex() is a character function that indexes and searches for sound-alike or phonetic matches.
/// It is used in applications where the precise spelling of character keys is not known or where there is a
/// high probability of misspelled names.  Misspelling is common in real-time transaction systems where the
/// data entry operator is receiving information over the telephone.  Soundex() works by bringing sound-alikes
/// together under the same key value.  Note, however, the soundex method is not absolute.
/// Keys that are quite different can result in the same soundex value.
/// </remarks>
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



/// <summary>This function is not implemented yet</summary>
// <summary>
// Allows text substitution in strings entered at runtime.
// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
FUNCTION StrEvaluate(s AS STRING) AS STRING
	THROW NotImplementedException{}
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

/// <exclude />
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
/// <param name="c">The string to trim. </param>
/// <returns>The string with the trailing spaces removed.  If the string is a NULL_STRING or all spaces, Trim() returns a NULL_STRING.</returns>
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
FUNCTION Trim(c AS STRING) AS STRING
	IF ( c == NULL )
		RETURN c
	ENDIF
	RETURN c:TrimEnd(trimChars)



/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase.
/// </summary>
/// <param name="cSource"> The string to convert to uppercase. </param>
/// <returns>String with all alphabetical characters converted to uppercase.  All other characters remain the same as in the original string.</returns>
/// <seealso cref='M:XSharp.Core.Functions.UpperA(System.String@)'>UpperA</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Lower(System.String)'>Lower</seealso>
FUNCTION Upper(cSource AS STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToUpper()
	ENDIF
	RETURN cSource


/// <summary>
/// Convert the lowercase and mixed case characters in a string to uppercase, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="cSource"> The string to convert to uppercase. </param>
/// <returns>String with all alphabetical characters converted to uppercase.  All other characters remain the same as in the original string.</returns>
/// <seealso cref='M:XSharp.Core.Functions.Upper(System.String)'>Upper</seealso>
/// <remarks>UpperA() is similar to Upper() except that it changes the contents of the argument as well as the return value.  See Upper() for details.</remarks>
FUNCTION UpperA(cSource REF STRING) AS STRING
	IF cSource != NULL
		cSource := cSource:ToUpper()
	ENDIF
	RETURN cSource

/// <summary>This function is not implemented yet</summary>
/// <param name="cLine"></param>
/// <param name="hfOut"></param>
/// <returns>
/// </returns>
FUNCTION UUDecodeLine(cLine AS STRING,hfOut AS IntPtr) AS DWORD
	THROW NotImplementedException{}
RETURN 0   

/// <summary>This function is not implemented yet</summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncFile(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>This function is not implemented yet</summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION UUEncLine(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   



/// <summary>Determine if the leftmost character in a string is alphabetic.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlNum</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlphaNum(System.String)'>IsAlphaNum</seealso>
FUNCTION IsAlpha(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetter( cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlpha</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlphaNum(System.String)'>IsAlphaNum</seealso>
FUNCTION IsAlNum(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLetterOrDigit( cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric..</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlNum</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlpha(System.String)'>IsAlpha</seealso>
FUNCTION IsAlphaNum(cSource AS STRING) AS LOGIC
	RETURN IsAlNum(cSource)


/// <summary>Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is a number from 0 to 9; otherwise FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsBDigit(System.String)'>IsBDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsXDigit(System.String)'>IsXDigit</seealso>
FUNCTION IsDigit(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsDigit(cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is a binary digit  (0 or 1)).</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsXDigit(System.String)'>IsXDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsDigit(System.String)'>IsDigit</seealso>
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
/// <seealso cref='M:XSharp.Core.Functions.IsBDigit(System.String)'>IsBDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsDigit(System.String)'>IsDigit</seealso>
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
/// <seealso cref='M:XSharp.Core.Functions.IsLower(System.String)'>IsLower</seealso>
FUNCTION IsUpper(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsUpper(cSource, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is lower.</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsUpper(System.String)'>IsUpper</seealso>
FUNCTION IsLower(cSource AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( cSource )
		ret := Char.IsLower(cSource, 0 )
	ENDIF
	RETURN ret



/// <summary>Determine if a string matches a wildcard pattern (like the wildcard pattern for the DIR command in the OS).</summary>
/// <param name="sWildCard">The wildcard to use. '*' matches 0 or more characters until the next non-wildcard character, '?' matches any character, all other characters must match exactly.</param>
/// <param name="sSource">The string to examine.</param>
/// <remarks>This function is case sensitive. If you want to do a case insensitive compare, use Like()</remarks>
/// <seealso cref='M:XSharp.Core.Functions.Like(System.String,System.String)'>Like</seealso>
FUNCTION _Like(sWildCard AS STRING, sSource AS STRING) AS LOGIC
    LOCAL nWildLen AS LONG
    LOCAL nSourceLen AS LONG
    LOCAL nWildPos  AS LONG
    LOCAL lAsterisk := FALSE AS LOGIC
    IF sSource == NULL .OR. sSource:Length == 0
        RETURN FALSE
    ENDIF
    IF sWildCard == NULL .OR. sWildCard:Length == 0
        RETURN FALSE
    ENDIF
    nWildLen    := sWildCard:Length
    nSourceLen  := sSource:Length
    nWildPos    := 0

    FOR VAR nSrcPos := 0 TO nSourceLen -1
        IF nWildPos == nWildLen
            // when we are at the end of the wildcard and we have no match yet
            // then return FALSE
            RETURN FALSE
        ENDIF
        SWITCH sWildCard[nWildPos]
        CASE '*'
            // when the wildcard ends with '*' then we have a match
            IF nWildPos == nWildLen -1
                RETURN TRUE
            ELSE
                lAsterisk := TRUE
                nWildPos++
            ENDIF
        CASE '?'
            nWildPos++
        OTHERWISE
            IF sWildCard[nWildPos] == sSource[nSrcPos]
                // match character after asterisk ?
                IF lAsterisk 
                    lAsterisk := FALSE
                ENDIF
                nWildPos++
            ELSE
                IF ! lAsterisk 
                    RETURN FALSE
                ENDIF
            ENDIF
        END SWITCH
    NEXT
    RETURN TRUE

/// <summary>Determine if a string matches a wildcard pattern (like the wildcard pattern for the DIR command in the OS).</summary>
/// <param name="sWildCard">The wildcard to use. '*' matches 0 or more characters until the next non-wildcard character, '?' matches any character, all other characters must match exactly.</param>
/// <param name="sSource">The string to examine.</param>
/// <remarks>This function is case INsensitive. If you want to do a case sensitive compare, use _Like()</remarks>
/// <seealso cref='M:XSharp.Core.Functions._Like(System.String,System.String)' >_Like</seealso>
FUNCTION Like(sWildCard AS STRING, sSource AS STRING) AS LOGIC
    RETURN _Like(Upper(sWildCard), Upper(sSource))

