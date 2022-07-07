//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Runtime.CompilerServices

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(uValue AS STRING, uMin AS STRING, uMax AS STRING) AS LOGIC
    return XSharp.RuntimeState.StringCompare(uMin, uValue) <= 0 .and. ;
        XSharp.RuntimeState.StringCompare(uValue, uMax) <= 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between<T>(uValue AS T, uMin AS T, uMax AS T) AS LOGIC WHERE T IS System.IComparable<T>
    RETURN ( uValue:CompareTo(uMin) >= 0 .AND. uValue:CompareTo(uMax) <= 0 )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS INT, nMin AS INT, nMax AS INT) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS INT64, nMin AS INT64, nMax AS INT64) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS DWORD, nMin AS DWORD, nMax AS DWORD) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS REAL4, nMin AS REAL4, nMax AS REAL4) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS REAL8, nMin AS REAL8, nMax AS REAL8) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS DECIMAL, nMin AS DECIMAL, nMax AS DECIMAL) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(dValue AS DateTime, dMin AS DateTime, dMax AS DateTime) AS LOGIC
    RETURN ( dValue >= dMin .AND. dValue <= dMax )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.RT.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS UINT64, nMin AS UINT64, nMax AS UINT64) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )
