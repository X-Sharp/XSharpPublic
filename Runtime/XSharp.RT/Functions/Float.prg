//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/absfloat/*" />
FUNCTION AbsFloat(fValue AS FLOAT) AS FLOAT
	RETURN FLOAT{Math.Abs(fValue:Value)}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fact/*" />
FUNCTION Fact(wValue AS DWORD) AS FLOAT
	LOCAL result := 1 AS double
	IF  wValue > 0
		LOCAL i AS DWORD
		FOR i := 1 UPTO wValue
			result := result * i
		NEXT
	ENDIF
RETURN FLOAT{result}


/// <exclude />
FUNCTION FClone(o AS FLOAT) AS FLOAT
	// no need to clone. Value type
	RETURN o

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/floatformat/*" />
FUNCTION FloatFormat(fValue AS FLOAT,iLen AS INT,iDec AS INT) AS FLOAT
	IF iDec < 0
		iDec := fValue:Decimals
	ENDIF
	IF iLen < 0
		// determine length by creating new float and converting it to string
		// not very efficient but this is what VO does
		LOCAL nDigits AS INT
		nDigits := fValue:Digits
		IF nDigits < 0 
			nDigits := (SHORT) RuntimeState.Digits
		ENDIF
		LOCAL fTemp AS FLOAT
		fTemp := FLOAT{fValue:value, nDigits, iDec}
		VAR cTemp := Ntrim(fTemp)
		iLen := cTemp:Length
	ELSEIF iDec != 0 .AND. iLen != 0 .AND. iLen < iDec +2
		iLen := iDec + 2
	ENDIF
	RETURN FLOAT{fValue:Value, iLen, iDec}  


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/frac/*" />
FUNCTION Frac(fValue AS FLOAT) AS FLOAT
	RETURN fValue - Integer(fValue)


/// <exclude />
FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS FLOAT
	RETURN FLOAT{xd, wDec}

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfloatdelta/*" />
FUNCTION SetFloatDelta(nNewSetting AS REAL8) AS REAL8
	VAR result := RuntimeState.FloatDelta
	RuntimeState.FloatDelta := nNewSetting
	RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfloatdelta/*" />
FUNCTION SetFloatDelta() AS REAL8
	RETURN RuntimeState.FloatDelta

