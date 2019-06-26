//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Globalization
USING System.Text


/// <summary>
/// Encrypt or decrypt a string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION Crypt(cSource AS STRING,cKey AS STRING) AS STRING
	LOCAL bSource AS BYTE[]
	LOCAL bDest  AS BYTE[]
	LOCAL bKey  AS BYTE[]
    LOCAL enc     AS Encoding
    IF cSource?:Length == 0 .OR. cKey?:Length < 2
        RETURN cSource
    ENDIF
    enc         := StringHelpers.WinEncoding
    bSource     := enc:GetBytes(cSource)
    bKey        := enc:GetBytes(cKey)
    bDest 		:= Crypt(bSource, bKey)
    cSource 	:= enc:GetString(bDest)
	RETURN cSource


/// <summary>
/// Encrypt or decrypt a string handling its characters as single bytes (high order byte is ignored)
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION CryptRaw(cSource AS STRING,cKey AS STRING) AS STRING
	LOCAL bSource AS BYTE[]
	LOCAL bDest  AS BYTE[]
	LOCAL bKey  AS BYTE[]
    IF cSource?:Length == 0 .OR. cKey?:Length < 2
        RETURN cSource
    ENDIF
    bSource     := BYTE[]{cSource:Length}
    FOR LOCAL n := 1 AS INT UPTO cSource:Length
    	bSource[n] := unchecked( (BYTE)cSource[n-1] )
    NEXT
    bKey     := BYTE[]{cKey:Length}
    FOR LOCAL n := 1 AS INT UPTO cKey:Length
    	bKey[n] := unchecked( (BYTE)cKey[n-1] )
    NEXT
    
    bDest := Crypt(bSource, bKey)

    LOCAL sDest AS StringBuilder
    sDest := StringBuilder{cSource:Length}
    FOREACH b AS BYTE IN bDest
    	sDest:Append((Char)b)
    NEXT
    
	RETURN sDest:ToString()

/// <summary>
/// Encrypt or decrypt an array of bytes.
/// </summary>
/// <param name="bSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION Crypt(bSource AS BYTE[],bKey AS BYTE[]) AS BYTE[]
	LOCAL bDest   AS BYTE[]
    LOCAL keyLen  AS INT
    LOCAL sourceLen AS INT
    LOCAL uiCode1 AS WORD
    LOCAL uiCode2 AS WORD
    LOCAL uiRotate AS WORD
    LOCAL uiCounter AS WORD
    LOCAL bNibble  AS BYTE
    LOCAL nPos     AS INT
    LOCAL nKeyPos  AS INT
    LOCAL bCurrent AS BYTE
    IF bSource:Length == 0 .OR. bKey?:Length < 2
        RETURN bSource
    ENDIF
    keyLen      := bKey:Length
    sourceLen   := bSource:Length
/*  IF lCreateNewArray
	    bDest       := BYTE[]{sourceLen}
	    System.Array.Copy(bSource, bDest, sourceLen)
    ELSE*/
    	bDest := bSource
//  END IF
    uiCode1     := (WORD) (bKey[1] ~ keyLen)
    uiCode1     := (WORD) (((DWORD)bKey[1] + (DWORD)bKey[2] * (DWORD)256) ~ (DWORD)keyLen)
    uiCode2     := 0xAAAA
    nPos        := 1
    nKeyPos     := keyLen+1
    bNibble     := 0
    DO WHILE nPos <= sourceLen
        IF nKeyPos > keyLen
            nKeyPos := 1
        ENDIF
        bCurrent  := bSource[nPos]  ~ bKey[nKeyPos]
        uiCode1   := (WORD) (uiCode1 ~ (uiCode1 >> 8))
        uiRotate  := (WORD) (uiCode1 & 0x0F)
        uiCode1   := (WORD) ((uiCode1 >> uiRotate)  | (uiCode1 << (16 - UiRotate)))
        uiCode1   := (WORD) (uICode1 ~ uiCode2)
        uiCode1   += 0x10
        uiCounter := (WORD) (uiCode1 & 0x1E)
        uiCounter += 2
        DO WHILE uiCounter > 0
            uiCounter --
            uiRotate := (BYTE) (uiCounter & 0x0F)
            uiCode2 := (WORD) ((uiCode2 >> uiRotate) | (uiCode2 << (16 - uiRotate)) )
            uiCode2 := (WORD) ((uiCode2 << 8) | (INT)( ( (uiCode2 >> 8) ~ 0xFF) ) )
            uiCode2 := (WORD) ((uiCode2 << 1) | (uiCode2 >> 15) )
            uiCode2 := (WORD)(uiCode2 ~ 0xAAAA )
            bNibble := (BYTE)(uiCode2 & 0xFF)

            bNibble := (BYTE)( (bNibble << 1) | (bNibble >> 7) )
            uiCode2 := (WORD)( (uiCode2 & 0xFF00) | bNibble )
            uiCounter--
        ENDDO
        bDest[nPos] := bCurrent ~ bNibble
        nPos ++
        nKeyPos++
    ENDDO
	RETURN bDest

/// <summary>
/// Encrypt or decrypt a string, changing the contents of the original string as well as returning the encrypted string.
/// </summary>
/// <param name="cSource"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION CryptA(cSource AS STRING,cKey AS STRING) AS STRING
	cSource := Crypt(cSource, cKey)
	RETURN cSource
