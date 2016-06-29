//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.Collections
using System.Runtime.InteropServices

STRUCTURE DefineInfo
	EXPORT cDefine AS STRING
	EXPORT nLine AS INT
	CONSTRUCTOR(_cDefine AS STRING , _nLine AS INT)
		SELF:cDefine := _cDefine
		SELF:nLine := _nLine
	RETURN
END STRUCTURE 

INTERNAL STRUCTURE ParseState
	EXPORT lVisFound AS LOGIC
	EXPORT lEntityFound AS LOGIC
	EXPORT lEntityIsClass AS LOGIC
	EXPORT lFirstChar AS LOGIC
	EXPORT lFirstWord AS LOGIC
	EXPORT lInParams AS LOGIC
	EXPORT lNameFound AS LOGIC
	EXPORT lField AS LOGIC
	EXPORT lParam AS LOGIC
	EXPORT lIgnore AS LOGIC
	EXPORT lPartial AS LOGIC
	EXPORT lStatic AS LOGIC
	EXPORT lFindingType AS LOGIC
	EXPORT nBracketCount AS INT
	EXPORT cBracketOpen , cBracketClose AS Char
	METHOD Reset() AS VOID
		lVisFound := FALSE
		lEntityFound := FALSE
		lEntityIsClass := FALSE
		lFirstChar := TRUE
		lFirstWord := TRUE
		lInParams := FALSE
		lNameFound := FALSE
		lField := FALSE
		lParam := FALSE
		lIgnore := FALSE
		lPartial := FALSE
		lStatic := FALSE
		lFindingType := FALSE
		nBracketCount := 0
		cBracketOpen := ' '
		cBracketClose := ' '
	RETURN
END STRUCTURE


STRUCTURE ActionData
	EXPORT cGuid AS STRING
	EXPORT cData AS STRING
	EXPORT oData AS OBJECT
	EXPORT aData AS ICollection
	CONSTRUCTOR(_cGuid AS STRING)
		SELF:cGuid := _cGuid
        SELF:cData := NULL
        SELF:oData := NULL
        SELF:aData := NULL
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING)
		SELF(_cGuid)
		SELF:cData := _cData
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING , _oData AS OBJECT)
		SELF(_cGuid, _cData)
		SELF:oData := _oData
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING , _oData AS OBJECT , _aData AS ICollection)
		SELF(_cGuid, _cData, _oData)
		SELF:aData := _aData
	RETURN
END STRUCTURE


STRUCTURE NameValue
	EXPORT Name AS STRING
	EXPORT Value AS OBJECT
	CONSTRUCTOR(_cName AS STRING , _oValue AS OBJECT)
		SELF:Name := _cName
		SELF:Value := _oValue
	RETURN
END STRUCTURE



[StructLayout(LayoutKind.Sequential)];
INTERNAL STRUCTURE _winTEXTMETRIC
   EXPORT tmHeight AS LONG
   EXPORT tmAscent AS LONG
   EXPORT tmDescent AS LONG
   EXPORT tmInternalLeading AS LONG
   EXPORT tmExternalLeading AS LONG
   EXPORT tmAveCharWidth AS LONG
   EXPORT tmMaxCharWidth AS  LONG
   EXPORT tmWeight AS LONG
   EXPORT tmOverhang AS LONG
   EXPORT tmDigitizedAspectX AS LONG
   EXPORT tmDigitizedAspectY AS LONG
   EXPORT tmFirstChar AS BYTE
   EXPORT tmLastChar AS BYTE
   EXPORT tmDefaultChar AS BYTE
   EXPORT tmBreakChar AS BYTE
   EXPORT tmItalic AS BYTE
   EXPORT tmUnderlined AS BYTE
   EXPORT tmStruckOut AS BYTE
   EXPORT tmPitchAndFamily AS BYTE
   EXPORT tmCharSet AS BYTE
END STRUCTURE


[StructLayout(LayoutKind.Sequential)];
INTERNAL STRUCTURE _winSIZE
   EXPORT cx AS Int32
   EXPORT cy AS Int32
END STRUCTURE

INTERNAL STRUCTURE UnitTranslateInfo
   EXPORT tmWidth AS INT
   EXPORT tmHeight AS INT
   EXPORT nBaseUnitX AS INT
   EXPORT nBaseUnitY AS INT
END STRUCTURE
