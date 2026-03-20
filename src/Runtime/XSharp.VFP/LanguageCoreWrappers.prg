//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/left/*" />
[FoxProFunction("LEFT", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Left(cString AS STRING, dwCount AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.Left(cString, dwCount)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/right/*" />
[FoxProFunction("RIGHT", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Right(cString AS STRING, dwCount AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.Right(cString, dwCount)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/len/*" />
[FoxProFunction("LEN", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Len(uValue IN USUAL) AS DWORD
    RETURN XSharp.RT.Functions.Len(uValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/upper/*" />
[FoxProFunction("UPPER", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Upper(cString AS STRING) AS STRING
    RETURN XSharp.Core.Functions.Upper(cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/lower/*" />
[FoxProFunction("LOWER", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Lower(cString AS STRING) AS STRING
    RETURN XSharp.Core.Functions.Lower(cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/empty/*" />
[FoxProFunction("EMPTY", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Empty(uValue AS USUAL) AS LOGIC
    RETURN XSharp.RT.Functions.Empty(uValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/between/*" />
[FoxProFunction("BETWEEN", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Between(uValue AS USUAL, uMin AS USUAL, uMax AS USUAL) AS LOGIC
    RETURN XSharp.RT.Functions.Between(uValue, uMin, uMax)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/inlist/*" />
[FoxProFunction("INLIST", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION InList(uValue AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
    RETURN XSharp.RT.Functions.InList(uValue, uValueList)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/occurs/*" />
[FoxProFunction("OCCURS", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Occurs(uSearch AS STRING, cTarget AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.Occurs(uSearch, cTarget)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strtran/*" />
[FoxProFunction("STRTRAN", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION StrTran(uTarget, uSearch, uReplace, uStart, uCount) AS STRING CLIPPER
    RETURN XSharp.RT.Functions.StrTran(uTarget, uSearch, uReplace, uStart, uCount)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/val/*" />
[FoxProFunction("VAL", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Val(cNumber AS STRING) AS USUAL
    RETURN XSharp.RT.Functions.Val(cNumber)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/int/*" />
[FoxProFunction("INT", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Int(nValue IN USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Integer(nValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/abs/*" />
[FoxProFunction("ABS", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Abs(nValue IN USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Abs(nValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqrt/*" />
[FoxProFunction("SQRT", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION SQrt(nNumber IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.SQrt(nNumber)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/pi/*" />
[FoxProFunction("PI", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION PI() AS REAL8
    RETURN XSharp.Core.Functions.PI

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/day/*" />
[FoxProFunction("DAY", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Day(dDate AS DATE) AS DWORD
    RETURN XSharp.RT.Functions.Day(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/month/*" />
[FoxProFunction("MONTH", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Month(dDate AS DATE) AS DWORD
    RETURN XSharp.RT.Functions.Month(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/year/*" />
[FoxProFunction("YEAR", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Year(dDate AS DATE) AS DWORD
    RETURN XSharp.RT.Functions.Year(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtos/*" />
[FoxProFunction("DTOS", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION DToS(dDate AS DATE) AS STRING
    RETURN XSharp.RT.Functions.DToS(dDate)

//-------------------------------------------------------------------------------//
// TODO(irwin): revisar de aquí en adelante
//-------------------------------------------------------------------------------//
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asc/*" />
[FoxProFunction("ASC", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Asc(cString AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.Asc(cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chr/*" />
[FoxProFunction("CHR", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Chr(dwCode AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.Chr(dwCode)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atcline/*" />
[FoxProFunction("ATCLINE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION ATCLine(cSearch AS STRING, cTarget AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.ATCLine(cSearch, cTarget)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atline/*" />
[FoxProFunction("ATLINE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION ATLine(cSearch AS STRING, cTarget AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.ATLine(cSearch, cTarget)

/// <include file="VfpDocs.xml" path="Runtimefunctions/padl/*" />
[FoxProFunction("PADL", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION PadL(uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING) AS STRING
    RETURN XSharp.RT.Functions.PadL(uValue, nLength, cFillChar)

/// <include file="VfpDocs.xml" path="Runtimefunctions/padr/*" />
[FoxProFunction("PADR", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION PadR(uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING) AS STRING
    RETURN XSharp.RT.Functions.PadR(uValue, nLength, cFillChar)

/// <include file="VfpDocs.xml" path="Runtimefunctions/padc/*" />
[FoxProFunction("PADC", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION PadC(uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING) AS STRING
    RETURN XSharp.RT.Functions.PadC(uValue, nLength, cFillChar)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/proper/*" />
[FoxProFunction("PROPER", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Proper(cText AS STRING) AS STRING
    RETURN XSharp.Core.Functions.Proper(cText)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ratline/*" />
[FoxProFunction("RATLINE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION RAtLine(cSearch AS STRING, cTarget AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.RAtLine(cSearch, cTarget)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/replicate/*" />
[FoxProFunction("REPLICATE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Replicate(cString AS STRING, dwCount AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.Replicate(cString, dwCount)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/soundex/*" />
[FoxProFunction("SOUNDEX", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION SoundEx(cString AS STRING) AS STRING
    RETURN XSharp.Core.Functions.SoundEx(cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/space/*" />
[FoxProFunction("SPACE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Space(dwSize AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.Space(dwSize)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/stuff/*" />
[FoxProFunction("STUFF", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Stuff(cTarget AS STRING, dwStart AS DWORD, dwDelete AS DWORD, cInsert AS STRING) AS STRING
    RETURN XSharp.Core.Functions.Stuff(cTarget, dwStart, dwDelete, cInsert)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/substr/*" />
[FoxProFunction("SUBSTR", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SubStr(cTarget AS USUAL, nStart AS USUAL, nCount AS USUAL) AS STRING
    RETURN XSharp.RT.Functions.SubStr(cTarget, nStart, nCount)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/max/*" />
[FoxProFunction("MAX", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Max(uValue1 IN USUAL, uValue2 IN USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Max(uValue1, uValue2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/min/*" />
[FoxProFunction("MIN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Min(uValue1 IN USUAL, uValue2 IN USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Min(uValue1, uValue2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mod/*" />
[FoxProFunction("MOD", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Mod(nDividend IN USUAL, nDivisor IN USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Mod(nDividend, nDivisor)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rand/*" />
[FoxProFunction("RAND", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Rand() AS FLOAT
    RETURN XSharp.RT.Functions.Rand()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/round/*" />
[FoxProFunction("ROUND", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Round(nNumber IN USUAL, siDecimals AS INT) AS USUAL
    RETURN XSharp.RT.Functions.Round(nNumber, siDecimals)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/acos/*" />
[FoxProFunction("ACOS", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION ACos(nExpression IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.ACos(nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asin/*" />
[FoxProFunction("ASIN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION ASin(nExpression IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.ASin(nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atan/*" />
[FoxProFunction("ATAN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION ATan(nNum IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.ATan(nNum)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atn2/*" />
[FoxProFunction("ATN2", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Atn2(nRow AS REAL8, nColumn AS REAL8) AS REAL8
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cos/*" />
[FoxProFunction("COS", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Cos(nNum IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.Cos(nNum)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sin/*" />
[FoxProFunction("SIN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Sin(nNum IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.Sin(nNum)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tan/*" />
[FoxProFunction("TAN", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Tan(nNum IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.Tan(nNum)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtor/*" />
[FoxProFunction("DTOR", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION DToR(nExpression IN USUAL) AS REAL8
    RETURN XSharp.RT.Functions.DToR(nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rtod/*" />
[FoxProFunction("RTOD", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION RToD(nExpression IN USUAL) AS REAL8
    RETURN XSharp.RT.Functions.RToD(nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/exp/*" />
[FoxProFunction("EXP", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Exp(nExponent IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.Exp(nExponent)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/log/*" />
[FoxProFunction("LOG", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION LOG(nValue IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.LOG(nValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/log10/*" />
[FoxProFunction("LOG10", FoxFunctionCategory.MathAndNumeric, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Log10(nValue IN USUAL) AS FLOAT
    RETURN XSharp.RT.Functions.Log10(nValue)

//-----------------------------------------------------------------------*
// DateAndTime
//-----------------------------------------------------------------------*
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cdow/*" />
[FoxProFunction("CDOW", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CDoW(dDate AS DATE) AS STRING
    RETURN XSharp.RT.Functions.CDoW(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cmonth/*" />
[FoxProFunction("CMONTH", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CMonth(dDate AS DATE) AS STRING
    RETURN XSharp.RT.Functions.CMonth(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dow/*" />
[FoxProFunction("DOW", FoxFunctionCategory.DateAndTime, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION DoW(dDate AS DATE) AS DWORD
    RETURN XSharp.RT.Functions.DoW(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ctod/*" />
[FoxProFunction("CTOD", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION CToD(cDate AS STRING) AS DATE
    RETURN XSharp.RT.Functions.CToD(cDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtoc/*" />
[FoxProFunction("DTOC", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION DToC(dDate AS DATE) AS STRING
    RETURN XSharp.RT.Functions.DToC(dDate)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fdate/*" />
[FoxProFunction("FDATE", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FDate(cFileName AS STRING, nType := 0 AS INT) AS USUAL
    IF XSharp.Core.Functions.File(cFileName)
        VAR cFullPath := XSharp.Core.Functions.FPathName()
        VAR info := System.IO.FileInfo{cFullPath}
        IF nType == 1
            RETURN info:LastWriteTime
        ELSE
            RETURN (DATE) info:LastWriteTime
        ENDIF
    ENDIF

    IF nType == 1
        RETURN System.DateTime.MinValue
    ELSE
        RETURN NULL_DATE
    ENDIF


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ftime/*" />
[FoxProFunction("FTIME", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FTime(cFileName AS STRING) AS STRING
    IF XSharp.Core.Functions.File(cFileName)
        VAR cFullPath := XSharp.Core.Functions.FPathName()
        VAR info := System.IO.FileInfo{cFullPath}
        RETURN info:LastWriteTime:ToString("HH:mm:ss")
    ENDIF
    RETURN ""

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/time/*" />
[FoxProFunction("TIME", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Time(nExpression := 0 AS INT) AS STRING
    RETURN XSharp.Core.Functions.Time()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/acopy/*" />
[FoxProFunction("ACOPY", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION ACopy(aSource AS USUAL, aTarget AS USUAL, nStart := 1 AS USUAL, nCount := -1 AS USUAL, nTargetPos := 1 AS USUAL) AS ARRAY
    RETURN XSharp.RT.Functions.ACopy(aSource, aTarget, nStart, nCount, nTargetPos)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ascan/*" />
[FoxProFunction("ASCAN", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AScan(aTarget AS ARRAY, uSearch IN USUAL) AS DWORD
    RETURN XSharp.RT.Functions.AScan(aTarget, uSearch)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asort/*" />
[FoxProFunction("ASORT", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION ASort(aTarget AS ARRAY, nStart := NIL AS USUAL, nCount := NIL AS USUAL, cbOrder := NIL AS USUAL) AS ARRAY
    RETURN XSharp.RT.Functions.ASort(aTarget, nStart, nCount, cbOrder)

// ----------------------------------------------------------------------- //
// TODO(irwin): functions pending to implement
// ----------------------------------------------------------------------- //
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cast/*" />
[FoxProFunction("CAST", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION Cast(eExpression IN USUAL, cType AS STRING) AS USUAL
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dodefault/*" />
[FoxProFunction("DODEFAULT", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION DoDefault(eArgs AS USUAL[]) AS USUAL
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/iif/*" />
// [FoxProFunction("IIF", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
// FUNCTION Iif(cond AS LOGIC, trueExpr AS USUAL, falseExpr AS USUAL) AS USUAL
    // RETURN IIF(cond, trueExpr, falseExpr)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isalpha/*" />
[FoxProFunction("ISALPHA", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsAlpha(pszString AS STRING) AS LOGIC
    RETURN XSharp.Core.Functions.IsAlpha(pszString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isdigit/*" />
[FoxProFunction("ISDIGIT", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsDigit(pszString AS STRING) AS LOGIC
    RETURN XSharp.Core.Functions.IsDigit(pszString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/islower/*" />
[FoxProFunction("ISLOWER", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsLower(pszString AS STRING) AS LOGIC
     RETURN XSharp.Core.Functions.IsLower(pszString)

/// <include file="VfpDocs.xml" path="Runtimefunctions/isupper/*" />
[FoxProFunction("ISUPPER", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsUpper(pszString AS STRING) AS LOGIC
    RETURN XSharp.Core.Functions.IsUpper(pszString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/like/*" />
[FoxProFunction("LIKE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Like(sWildCard AS STRING, sSource AS STRING) AS LOGIC
    RETURN XSharp.Core.Functions.Like(sWildCard, sSource)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/memlines/*" />
[FoxProFunction("MEMLINES", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION MemLines(cString AS STRING) AS DWORD
    RETURN XSharp.Core.Functions.MemLines(cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mline/*" />
[FoxProFunction("MLINE", FoxFunctionCategory.StringAndCharacter, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION MLine(cString AS STRING, nLine AS DWORD, nOffset := 0 AS DWORD) AS STRING
    RETURN XSharp.Core.Functions.MLine(cString, nLine)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/objnum/*" />
[FoxProFunction("OBJNUM", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.NotSupported, FoxCriticality.Low)];
FUNCTION ObjNum(ObjectName AS STRING) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/objvar/*" />
[FoxProFunction("OBJVAR", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.NotSupported, FoxCriticality.Low)];
FUNCTION ObjVar(ObjectName AS STRING) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/pcount/*" />
// [FoxProFunction("PCOUNT", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
// FUNCTION Pcount() AS INT
    // THROW NotImplementedException{}
