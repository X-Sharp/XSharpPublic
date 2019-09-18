//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Text
// Array of chars used for the various trim functions
INTERNAL GLOBAL trimChars := <CHAR>{ ' ' } AS CHAR[]

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alltrim/*" />
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <returns>A trimmed string, with leading and trailing spaces removed.</returns>
FUNCTION AllTrim(cString AS STRING) AS STRING
	IF ( cString == NULL )
		RETURN cString
	ENDIF
	RETURN cString:Trim(trimChars)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asc/*" />
FUNCTION Asc(cString AS STRING) AS DWORD
	LOCAL ascValue := 0 AS DWORD
	LOCAL chValue AS CHAR
	IF ( !String.IsNullOrEmpty(cString) ) 
		chValue := cString[0]
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




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/at/*" />
FUNCTION At(cSearch AS STRING,cTarget AS STRING) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( cTarget != NULL .AND. cSearch != NULL )
		IF cTarget:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) cTarget:IndexOf(cSearch, StringComparison.Ordinal) +1
		END IF
	ENDIF
	RETURN position

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/at2/*" />
FUNCTION At2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN At(cSearch,cTarget)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/at3/*" />
FUNCTION At3(cSearch AS STRING,cTarget AS STRING,dwOffset AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	// note dwOffset is ZERO based in VO
	IF ( cTarget != NULL .AND. cSearch != NULL .AND. dwOffset <= cTarget:Length )
		IF cTarget:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) cTarget:IndexOf(cSearch,(INT)dwOffset, StringComparison.Ordinal) +1
		ENDIF
	ENDIF
	RETURN position

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atc/*" />
FUNCTION AtC(cSearch AS STRING,cTarget AS STRING) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( cTarget != NULL .AND. cSearch != NULL )
		IF cTarget:Length != 0 .AND. cSearch:Length != 0
			position := (DWORD) cTarget:IndexOf(cSearch, StringComparison.OrdinalIgnoreCase) +1
		ENDIF
	ENDIF
	RETURN position

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atc2/*" />
FUNCTION AtC2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtC(cSearch,cTarget)  

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atcline/*" />
FUNCTION ATCLine(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtLine( cSearch:ToUpper(), cTarget:ToUpper() )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atcline2/*" />
FUNCTION ATCLine2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtLine2( cSearch:ToUpper(), cTarget:ToUpper() )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atline/*" />
FUNCTION ATLine(cSearch AS STRING,cTarget AS STRING) AS DWORD
	LOCAL nPos AS DWORD
	IF String.IsNullOrEmpty(cTarget) .OR. String.IsNullOrEmpty(cSearch)
		RETURN 0
	ENDIF
	IF cTarget:StartsWith(cSearch) 
		RETURN 1
	ENDIF
	nPos    := At( cSearch, cTarget )
	IF (nPos > 0)
		cTarget := Left( cTarget, nPos - 1 )
		nPos := MemLines( cTarget)
		IF cTarget:EndsWith(e"\r\n")
			nPos++
		ENDIF
	ENDIF
	RETURN nPos


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atline2/*" />
FUNCTION ATLine2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtLine(cSearch, cTarget)

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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/buffer/*" />
FUNCTION Buffer(dwSize AS DWORD) AS STRING
	RETURN STRING{'\0', (INT) dwSize}




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/chareven/*" />
FUNCTION CharEven(cString AS STRING) AS STRING
	LOCAL evenChars:=NULL AS STRING
    LOCAL c := cString as STRING
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/charmix/*" />
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/charodd/*" />
FUNCTION CharOdd(cString AS STRING) AS STRING
	LOCAL oddChars:=NULL AS STRING
    LOCAL c := cString as STRING
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/charpos/*" />
FUNCTION CharPos(cString AS STRING, wPosition AS DWORD) AS STRING
	LOCAL result := string.Empty AS STRING
	IF ( wPosition >= 1 .AND. wPosition <= (DWORD) cString:Length )
		result := cString:SubString((INT)wPosition-1,1)
	ENDIF
	RETURN result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/chr/*" />
/// <remarks>
/// The value of dwChar must be between 0 and 255<br/>
/// The return value of Chr() in XSharp depends on the setting of SetAnsi().<br/>
/// When SetAnsi() = TRUE then the active windows Ansi codepage is used to calculate the character.<br/>
/// When SetAnsi() = FALSE then the active windows Oem codepage is used to calculate the character.<br/>
/// </remarks>
FUNCTION Chr(dwCode AS DWORD) AS STRING
  LOCAL b   AS BYTE
  LOCAL ret AS STRING
   b := (BYTE)( dwCode & 0xFF )  // VO ignores the high 24 bits

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




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/decodebase64/*" />
FUNCTION DecodeBase64(cAttachment AS STRING,hDestination AS IntPtr) AS INT
	IF cAttachment == NULL .OR. cAttachment:Length== 0
		RETURN 0
	ENDIF
	VAR aBytes := Convert.FromBase64String( cAttachment )
	RETURN (INT) FWrite3(hDestination, aBytes, (DWORD) aBytes:Length)


	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/encodebase64/*" />
FUNCTION EncodeBase64(hSource AS IntPtr,hDestination AS IntPtr) AS INT
	LOCAL nSize AS DWORD
	LOCAL nRead AS DWORD
	// determine file size
	FSeek3(hSource, 0, FS_END)
	nSize := FTell(hSource)
	FSeek3(hSource, 0, FS_SET)
	VAR aBuffer := BYTE[]{ (INT) nSize}
	nRead := FRead3(hSource, aBuffer, nSize)
	IF nRead != nSize
		RETURN 0
	ENDIF
	VAR cContents := Convert.ToBase64String(aBuffer, Base64FormattingOptions.InsertLineBreaks)
	nSize := FWrite(hDestination, cContents)
	RETURN (INT) nSize





/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/hardcr/*" />
FUNCTION HardCR(cString AS STRING) AS STRING
	RETURN cString:Replace( (CHAR) 141, (CHAR) 13 )


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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/instr/*" />
FUNCTION Instr(cSearch AS STRING,cTarget AS STRING) AS LOGIC
	LOCAL result := FALSE AS LOGIC
	IF cSearch != NULL .AND. cTarget != NULL .AND. cSearch:Length != 0
		result := cTarget:IndexOf( cSearch, StringComparison.Ordinal ) > -1
	ENDIF
	RETURN result   



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/left/*" />
FUNCTION Left(cString AS STRING, dwCount AS DWORD) AS STRING
	IF ( cString!=NULL )
		IF dwCount < cString:Length
			cString := cString:Substring( 0, (INT) dwCount )
		ELSE
			NOP
		ENDIF
	ENDIF
	RETURN cString


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lower/*" />
/// <seealso cref='M:XSharp.Core.Functions.LowerA(System.String@)'>LowerA</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Upper(System.String)'>Upper</seealso>
FUNCTION Lower(cString AS STRING) AS STRING
	IF cString != NULL
		cString := cString:ToLower()
	ENDIF
	RETURN cString

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lowera/*" />
/// <seealso cref='M:XSharp.Core.Functions.Lower(System.String)'>Lower</seealso>
FUNCTION LowerA(cString REF STRING) AS STRING
	IF cString != NULL
		cString := cString:ToLower()
	ENDIF
	RETURN cString

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ltrim/*" />
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
FUNCTION LTrim(cString AS STRING) AS STRING
	IF (cString == NULL)
		RETURN cString
	ENDIF
	RETURN cString:TrimStart(trimChars)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/occurs/*" />
FUNCTION Occurs(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN Occurs3(cSearch,cTarget, 0)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/occurs2/*" />
FUNCTION Occurs2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN Occurs3(cSearch,cTarget, 0)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/occurs3/*" />
FUNCTION Occurs3(cSearch AS STRING,cTarget AS STRING,dwOffset AS DWORD) AS DWORD
	LOCAL pos AS INT
	LOCAL count AS DWORD
	
	IF String.IsNullOrEmpty(cSearch) .OR. String.IsNullOrEmpty(cTarget)
		RETURN 0
	ENDIF
	
	IF dwOffset > 0
		dwOffset -= 1
	ENDIF
	
	count := 0
	IF dwOffset < (DWORD) cTarget:Length
		DO WHILE ( pos := cTarget:IndexOf(cSearch, (INT)dwOffset, StringComparison.Ordinal) ) >= 0
			count++
			dwOffset := (DWORD)(pos + cSearch:Length)
		ENDDO
	ENDIF
	
	RETURN count

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ansi2oem/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Ansi2Oem(cAnsiString AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cAnsiString:Length
	aBytes := String2Bytes(cAnsiString)
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
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ansi2oema/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Ansi2OemA(cAnsiString REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cAnsiString:Length
	aBytes := String2Bytes(cAnsiString)
	aBytes := Ansi2Oem(aBytes, iLen)
	cAnsiString := Bytes2String(aBytes, iLen)  
	RETURN cAnsiString


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
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/oem2ansi/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="M:XSharp.Core.Functions.Oem2AnsiBuff(System.Byte[],System.Byte[],System.UInt32)" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2AnsiA" />
FUNCTION Oem2Ansi(cOemString AS STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cOemString:Length
	aBytes := String2Bytes(cOemString)
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
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/oem2ansia/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="O:XSharp.Core.Functions.Ansi2Oem" />
/// <seealso cref="O:XSharp.Core.Functions.Ansi2OemA" />
/// <seealso cref="O:XSharp.Core.Functions.Oem2Ansi" />
FUNCTION Oem2AnsiA(cOemString REF STRING) AS STRING
	LOCAL aBytes AS BYTE[]
	LOCAL iLen	 AS INT
	iLen := cOemString:Length
	aBytes := String2Bytes(cOemString)
	aBytes := Oem2Ansi(aBytes, iLen)
	cOemString := Bytes2String(aBytes, iLen)
	RETURN cOemString


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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/oem2ansibuff/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="M:XSharp.Core.Functions.Ansi2OemBuff(System.Byte[],System.Byte[],System.UInt32)" />
FUNCTION Oem2AnsiBuff(pszTarget AS BYTE[],pszSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	OemToCharBuffA(pszSource, pszTarget, dwCount)
	RETURN pszTarget

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ansi2oembuff/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Ansi2Oem/*" /></remarks>
/// <seealso cref="M:XSharp.Core.Functions.Oem2AnsiBuff(System.Byte[],System.Byte[],System.UInt32)" />
FUNCTION Ansi2OemBuff(pszTarget AS BYTE[],pszSource AS BYTE[],dwCount AS DWORD) AS BYTE[]
	CharToOemBuffA(pszSource, pszTarget, dwCount)
	RETURN pszTarget

INTERNAL _DLL FUNCTION CharToOemBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.CharToOemBuffA
INTERNAL _DLL FUNCTION OemToCharBuffA( lpszSrc AS BYTE[], lpszDst AS BYTE[], cchDstLength AS DWORD ) AS LOGIC PASCAL:USER32.OemToCharBuffA




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/proper/*" />
/// <seealso cref="M:XSharp.Core.Functions.ProperA(System.String@)" />
FUNCTION Proper(cText AS STRING) AS STRING
	LOCAL sb AS StringBuilder
	LOCAL inside AS LOGIC
	IF cText != NULL
		sb := StringBuilder{cText:Length}
		inside := FALSE
		FOREACH ch AS CHAR IN cText
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
		cText := sb:ToString()
	ENDIF
	RETURN cText

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/propera/*" />
/// <seealso cref="M:XSharp.Core.Functions.Proper(System.String)" />
FUNCTION ProperA(cName REF STRING) AS STRING
	cName := Proper(cName)
	RETURN cName

/// <summary>
/// </summary>
/// <param name="cIn"></param>
/// <returns>
/// </returns>
FUNCTION QPEncString(cIn AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rat/*" />
/// <seealso cref="M:XSharp.Core.Functions.RAt3(System.String,System.String,System.UInt32)" />
/// <seealso cref="M:XSharp.Core.Functions.RAt2(System.String,System.String)" />
/// <seealso cref="M:XSharp.Core.Functions.RAt(System.String,System.String)" />
FUNCTION RAt(cSearch AS STRING,cTarget AS STRING) AS DWORD
	LOCAL rightMost := 0 AS DWORD
	IF cSearch != NULL .AND. cTarget != NULL
		IF cTarget:Length != 0 .AND. cSearch:Length != 0
			rightMost:= (DWORD) cTarget:LastIndexOf(cSearch, StringComparison.Ordinal) + 1
		ENDIF
	ENDIF
	RETURN rightMost

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rat/*" />
FUNCTION RAt2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN RAt(cSearch,cTarget) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rat3/*" />
FUNCTION RAt3(cSearch AS STRING,cTarget AS STRING,dwOffSet AS DWORD) AS DWORD
	LOCAL nResult := 0 AS DWORD
	IF cSearch != NULL .AND. cTarget != NULL
		IF cTarget:Length != 0 .AND. cSearch:Length != 0
			IF dwOffSet > (DWORD) cTarget:Length 
				dwOffSet := 0U
			ENDIF
			VAR cTemp := cTarget:Substring((INT) dwOffSet)
			nResult := RAt(cSearch, cTemp)
			IF nResult > 0U
				nResult := nResult+dwOffSet
			ENDIF
		ENDIF
	ENDIF
	RETURN nResult



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ratline/*" />
FUNCTION RAtLine(cSearch AS STRING, cTarget AS STRING) AS DWORD
	LOCAL nPos AS DWORD
	IF cSearch == NULL .OR. cTarget == NULL .OR. cSearch:Length == 0 .OR. cTarget:Length == 0
		RETURN 0
	ENDIF
	nPos := RAt(cSearch,cTarget)
	cTarget := Left(cTarget,nPos-1)
	
	RETURN MemLines(cTarget)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ratline2/*" />
FUNCTION RATLine2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN RAtLine(cSearch, cTarget)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/replicate/*" />
FUNCTION Repl(cString AS STRING,dwCount AS DWORD) AS STRING
	RETURN Replicate(cString, dwCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/replicate/*" />
FUNCTION Replicate(cString AS STRING,dwCount AS DWORD) AS STRING
	LOCAL cReturn := "" AS STRING
	IF dwCount > 0 .AND. cString != NULL
		cReturn := System.Text.StringBuilder{}:Insert( 0, cString, (INT) dwCount ):ToString()
	ENDIF
	RETURN cReturn

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/right/*" />
FUNCTION Right(cString AS STRING,dwCount AS DWORD) AS STRING
	IF cString != NULL
		IF dwCount > cString:Length
			NOP
		ELSE
			cString := cString:Substring( cString:Length - (INT) dwCount, (INT) dwCount )
		ENDIF
	ENDIF
	RETURN cString


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rtrim/*" />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.Trim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
FUNCTION RTrim(cString AS STRING) AS STRING
	IF ( cString == NULL )
		RETURN cString
	ENDIF
	RETURN cString:TrimEnd(trimChars)





FUNCTION SClone(cString AS STRING) AS STRING
	LOCAL clonedString := NULL AS STRING
	IF (cString != NULL)
		clonedString := String.Copy( cString )
	ENDIF
	RETURN clonedString



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/space/*" />
FUNCTION Space(dwSize AS DWORD) AS STRING
	RETURN STRING{' ',(INT)dwSize}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/slen/*" />
/// <remarks>
/// The compiler will replace calls to SLen() as much as possible with a direct call to the Length property of a string.
/// </remarks>
FUNCTION SLen(cString AS STRING) AS DWORD
	LOCAL len := 0 AS DWORD
	IF cString != NULL
		len := (DWORD) cString:Length
	ENDIF
	RETURN len


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/soundex/*" />
FUNCTION SoundEx(cString AS STRING) AS STRING
	LOCAL sb		AS StringBuilder
	LOCAL iLen		AS INT
	LOCAL ret		AS STRING
	LOCAL i			AS INT
	LOCAL cLastChar AS CHAR
	LOCAL cSoundExChar AS CHAR
	
	IF String.IsNullOrEmpty(cString)
		RETURN "0000"
	END IF
	cLastChar := (CHAR) 0
	cString	 := cString:ToUpper()
	iLen := cString:Length - 1
	sb := StringBuilder{}
	sb:Append( cString[0] )
	FOR i := 1 TO iLen
		cSoundExChar := _SoundExChar( cString[i] )
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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strevaluate/*" />
FUNCTION StrEvaluate(cString AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/stuff/*" />
FUNCTION Stuff(cTarget AS STRING,dwStart AS DWORD,dwDelete AS DWORD,cInsert AS STRING) AS STRING
	LOCAL result := cInsert AS STRING
	IF cTarget != NULL
		IF string.IsNullOrEmpty(cInsert)
			cInsert := ""
		ENDIF
		IF dwStart > 0
			dwStart -= 1
		ENDIF
		LOCAL part1 := cTarget AS STRING
		IF  (INT) dwStart < cTarget:Length 
			part1 := cTarget:Substring(0,(INT)dwStart)
		ENDIF
		LOCAL part2 := "" AS STRING
		VAR iOffSet := (INT) (dwStart + dwDelete)
		IF  iOffSet  < cTarget:length 
			part2 := cTarget:Substring( iOffSet )
		ENDIF
		result := part1 + cInsert + part2
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/substr2/*" />
FUNCTION SubStr2(cTarget AS STRING,dwStart AS DWORD) AS STRING
	IF cTarget != NULL
		cTarget := __SubStr( cTarget, (INT) dwStart, ( cTarget:Length - (INT) dwStart  + 1 ) )
	ENDIF
	RETURN cTarget

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/substr3/*" />
FUNCTION SubStr3(cTarget AS STRING,dwStart AS DWORD,dwLen AS DWORD) AS STRING
	IF cTarget != NULL
		cTarget := __SubStr( cTarget, (INT) dwStart, (INT) dwLen )
	ENDIF
	RETURN cTarget



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/trim/*" />
/// <seealso cref='M:XSharp.Core.Functions.RTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)' />
/// <seealso cref='M:XSharp.Core.Functions.AllTrim(System.String)' />
FUNCTION Trim(cString AS STRING) AS STRING
	IF ( cString == NULL )
		RETURN cString
	ENDIF
	RETURN cString:TrimEnd(trimChars)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/upper/*" />
/// <seealso cref='M:XSharp.Core.Functions.UpperA(System.String@)'>UpperA</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Lower(System.String)'>Lower</seealso>
FUNCTION Upper(cString AS STRING) AS STRING
	IF cString != NULL
		cString := cString:ToUpper()
	ENDIF
	RETURN cString


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/uppera/*" />
/// <returns>String with all alphabetical characters converted to uppercase.  All other characters remain the same as in the original string.</returns>
/// <seealso cref='M:XSharp.Core.Functions.Upper(System.String)'>Upper</seealso>
/// <remarks>UpperA() is similar to Upper() except that it changes the contents of the argument as well as the return value.  See Upper() for details.</remarks>
FUNCTION UpperA(cString REF STRING) AS STRING
	IF cString != NULL
		cString := cString:ToUpper()
	ENDIF
	RETURN cString

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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isalpha/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlNum</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlphaNum(System.String)'>IsAlphaNum</seealso>
FUNCTION IsAlpha(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		ret := Char.IsLetter( pszString, 0 )
	ENDIF
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isalnum/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlpha</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlphaNum(System.String)'>IsAlphaNum</seealso>
FUNCTION IsAlNum(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		ret := Char.IsLetterOrDigit( pszString, 0 )
	ENDIF
	RETURN ret

/// <summary>Determine if the leftmost character in a string is alphanumeric..</summary>
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
/// <seealso cref='M:XSharp.Core.Functions.IsAlNum(System.String)'>IsAlNum</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsAlpha(System.String)'>IsAlpha</seealso>
FUNCTION IsAlphaNum(cSource AS STRING) AS LOGIC
	RETURN IsAlNum(cSource)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isdigit/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsBDigit(System.String)'>IsBDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsXDigit(System.String)'>IsXDigit</seealso>
FUNCTION IsDigit(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		ret := Char.IsDigit(pszString, 0 )
	ENDIF
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isbdigit/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsXDigit(System.String)'>IsXDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsDigit(System.String)'>IsDigit</seealso>
FUNCTION IsBDigit(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		SWITCH pszString[0]
			CASE '0'
			CASE '1'
				ret := TRUE
		END SWITCH
	ENDIF
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isxdigit/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsBDigit(System.String)'>IsBDigit</seealso>
/// <seealso cref='M:XSharp.Core.Functions.IsDigit(System.String)'>IsDigit</seealso>
FUNCTION IsXDigit(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		LOCAL c AS CHAR
		c := pszString[0]
		IF char.IsDigit(c) .OR. ;
			(c >= 'A' .AND. c <= 'F') .OR. ;
			(c >= 'a' .AND. c <= 'f')
			ret := TRUE
		ENDIF
	ENDIF
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isspace/*" />
/// <param name="cSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
FUNCTION IsSpace(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	switch (int) pszString[0]
		CASE 9
        CASE 10
        CASE 11
        CASE 12
        CASE 13
        CASE 32
            ret := true
    end switch
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isupper/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsLower(System.String)'>IsLower</seealso>
FUNCTION IsUpper(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		ret := Char.IsUpper(pszString, 0 )
	ENDIF
	RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/islower/*" />
/// <seealso cref='M:XSharp.Core.Functions.IsUpper(System.String)'>IsUpper</seealso>
FUNCTION IsLower(pszString AS STRING) AS LOGIC
	LOCAL ret := FALSE AS LOGIC
	IF ! String.IsNullOrEmpty( pszString )
		ret := Char.IsLower(pszString, 0 )
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

