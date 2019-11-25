//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Globalization
USING System.Threading
USING System.Security.Permissions

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mballtrim/*" />
/// <remarks><note type="tip">This function is the same as AllTrim() since .Net has unicode strings</note></remarks>
FUNCTION MBAllTrim(cMBString AS STRING) AS STRING
	RETURN AllTrim(cMBString)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbat/*" />
/// <remarks><note type="tip">This function is the same as At() since .Net has unicode strings</note></remarks>
FUNCTION MBAt(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN At( cMBSearch, cMBTarget ) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbat2/*" />
/// <remarks><note type="tip">This function is the same as At2() since .Net has unicode strings</note></remarks>
FUNCTION MBAt2(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN At2( cMBSearch, cMBTarget )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbat3/*" />
/// <remarks><note type="tip">This function is the same as At3() since .Net has unicode strings</note></remarks>
FUNCTION MBAt3(cMBSearch AS STRING,cMBTarget AS STRING,wOffset AS DWORD) AS DWORD
	RETURN At3( cMBSearch, cMBTarget, wOffset ) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbatc/*" />
/// <remarks><note type="tip">This function is the same as AtC() since .Net has unicode strings</note></remarks>
FUNCTION MBAtC(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN AtC(cMBSearch, cMBTarget)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbatc2/*" />
/// <remarks><note type="tip">This function is the same as AtC2() since .Net has unicode strings</note></remarks>
FUNCTION MBAtC2(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN AtC2(cMBSearch, cMBTarget)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbatline/*" />
/// <remarks><note type="tip">This function is the same as AtLine() since .Net has unicode strings</note></remarks>
FUNCTION MBAtLine(cMBSearch AS STRING,cMBTarget AS STRING) AS LONGINT
	RETURN (LONG) AtLine(cMBSearch, cMBTarget)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbatline2/*" />
/// <remarks><note type="tip">This function is the same as AtLine2() since .Net has unicode strings</note></remarks>
FUNCTION MBAtLine2(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN AtLine2(cMBSearch, cMBTarget)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbleft/*" />
/// <remarks><note type="tip">This function is the same as Left() since .Net has unicode strings</note></remarks>
FUNCTION MBLEFT(cMBString AS STRING,wCount AS DWORD) AS STRING
	RETURN Left(cMBString, wCount)   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mblen/*" />
/// <remarks><note type="tip">This function is the same as SLen() since .Net has unicode strings</note></remarks>
FUNCTION MBLen(uMBValue AS STRING) AS DWORD 
	RETURN SLen(uMBValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbltrim/*" />
/// <remarks><note type="tip">This function is the same as Ltrim() since .Net has unicode strings</note></remarks>
FUNCTION MBLTrim(cMBString AS STRING) AS STRING
	RETURN LTrim(cMBString)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbrat/*" />
/// <remarks><note type="tip">This function is the same as Rat() since .Net has unicode strings</note></remarks>
FUNCTION MBRat(cMBSearch AS STRING,cMBTarget AS STRING) AS LONGINT
	RETURN (LONG) RAt(cMBSearch, cMBTarget)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbrat2/*" />
/// <remarks><note type="tip">This function is the same as Rat2() since .Net has unicode strings</note></remarks>
FUNCTION MBRat2(cMBSearch AS STRING,cMBTarget AS STRING) AS DWORD
	RETURN RAt2(cMBSearch, cMBTarget)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbrat3/*" />
/// <remarks><note type="tip">This function is the same as Rat3() since .Net has unicode strings</note></remarks>
FUNCTION MBRat3(cMBSearch AS STRING,cMBTarget AS STRING,wOffset AS DWORD) AS DWORD
	RETURN RAt3(cMBSearch, cMBTarget, wOffSet)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbright/*" />
/// <remarks><note type="tip">This function is the same as Right() since .Net has unicode strings</note></remarks>
FUNCTION MBRight(cMbString AS STRING,wCount AS DWORD) AS STRING
	RETURN Right(cMbString, wCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbrtrim/*" />
/// <remarks><note type="tip">This function is the same as RTrim() since .Net has unicode strings</note></remarks>
FUNCTION MBRTrim(cMbString AS STRING) AS STRING
	RETURN RTrim(cMbString)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbslen/*" />
/// <remarks><note type="tip">This function is the same as Slen() since .Net has unicode strings</note></remarks>
FUNCTION MBSLen(cMbString AS STRING) AS DWORD
	RETURN SLen(cMbString)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbstuff/*" />
/// <remarks><note type="tip">This function is the same as Stuff() since .Net has unicode strings</note></remarks>
FUNCTION MBStuff(cMBTarget AS STRING,wStart AS DWORD,wDelete AS DWORD,cMBInsert AS STRING) AS STRING
	RETURN Stuff(cMBTarget, wStart, wDelete, cMBInsert)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbsubstr2/*" />
/// <remarks><note type="tip">This function is the same as Substr2() since .Net has unicode strings</note></remarks>
FUNCTION MBSubstr2(cMBTarget AS STRING,wStart AS DWORD) AS STRING
	RETURN SubStr2(cMBTarget, wStart)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbsubstr3/*" />
/// <remarks><note type="tip">This function is the same as Substr3() since .Net has unicode strings</note></remarks>
FUNCTION MBSubstr3(cMBTarget AS STRING,wStart AS DWORD,wCount AS DWORD) AS STRING
	RETURN SubStr3(cMBTarget, wStart, wCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mbtrim/*" />
/// <remarks><note type="tip">This function is the same as Trim() since .Net has unicode strings</note></remarks>
FUNCTION MBTrim(cMBString AS STRING) AS STRING
	RETURN Trim(cMBString)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getapplocaleid/*" />
FUNCTION GetAppLocaleID() AS DWORD
	LOCAL oCI AS CultureInfo
	oCI := CultureInfo.CurrentCulture
	RETURN (DWORD) oCI:LCID


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setapplocaleid/*" />
[SecurityPermissionAttribute(SecurityAction.Demand, ControlThread := TRUE)];
FUNCTION SetAppLocaleID(uiNewLocale AS DWORD) AS DWORD
	VAR ci := CultureInfo{ (INT) uiNewLocale}
	Thread.CurrentThread:CurrentCulture	  := ci
	Thread.CurrentThread:CurrentUICulture := ci
	RETURN uiNewLocale


/// <summary>This function is not implemented yet</summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetClipCompFunc(pFunc AS OBJECT) AS IntPtr
	THROW NotImplementedException{}
	RETURN IntPtr.Zero


/// <summary>This function is not implemented yet</summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFlags(n AS OBJECT) AS LONG
	THROW NotImplementedException{}
	RETURN 0   

/// <summary>This function is not implemented yet</summary>
/// <summary>
/// </summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFunc(pFunc AS OBJECT) AS IntPtr
	THROW NotImplementedException{}
	RETURN IntPtr.Zero

/// <exclude />
[Obsolete];
FUNCTION NationInit(dwInst AS DWORD) AS INT
	RETURN 0   

/// <exclude />
[Obsolete];
FUNCTION NationExit() AS INT
	RETURN 0   


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/nversion/*" />
FUNCTION NVersion() AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty 

/// <exclude />
FUNCTION MAKELANGID( p AS WORD, s AS WORD ) AS WORD
	RETURN (WORD) ( ( s << 10 ) | p )

/// <exclude />
FUNCTION MAKELCID( lgid AS WORD, srtid AS WORD ) AS DWORD
	RETURN (DWORD) ( ( ( (DWORD)(srtid) ) << 16) | ( (INT)(DWORD) lgid ) )

/// <exclude />
FUNCTION IsBiDi() AS LOGIC
   RETURN System.Windows.Forms.SystemInformation.MidEastEnabled   

/// <exclude />
FUNCTION String2W( sz AS STRING ) AS IntPtr
  // The original VO Code was using SysAllocString to allocate the memory.
  // The Marshal class does that too (it uses SysAllocStringLen)
  // and it also takes into account null strings 
  RETURN System.Runtime.InteropServices.Marshal.StringToBSTR(sz)

/// <exclude />
FUNCTION W2String(p AS IntPtr) AS STRING
    // The original code was using WideCharToMultiByte to determine the length of the string inside the ptr
    // The Marshal implementation calls SysStringLen to determine the length
    // and then creates a managed string with PtrToStringUni() passing in the ptr and the length
    //
	// RETURN System.Runtime.InteropServices.Marshal.PtrToStringBSTR(p)
	//
	// Above implementation does not always recognise correctly the size of the string,
	// causing an OutOfMemoryException, especially when pointer is returned by some Win32 function.
    // See https://www.xsharp.info/forum/public-product/867-what-happens-with-w2string#6903
	LOCAL IMPLIED cRet := System.Text.StringBuilder{}
	LOCAL nIndex AS INT
	LOCAL pChar AS WORD PTR
	nIndex := 1
	pChar := (WORD PTR)p
	DO WHILE pChar[nIndex] != 0
		cRet:Append(Convert.ToChar(pChar[nIndex]))
		nIndex ++
	END DO
	RETURN cRet:ToString()    

/// <exclude />
FUNCTION GetNatDllHandle() AS IntPtr STRICT
  LOCAL t AS Type
   LOCAL m AS System.Reflection.Module
   t := typeof( XSharp.Error )  
   m := t:Module
   RETURN System.Runtime.InteropServices.Marshal.GetHINSTANCE( m )

