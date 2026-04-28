//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirchange/*" />
FUNCTION DirChange(pszDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirChange(Psz2String(pszDir))
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirmake/*" />
FUNCTION DirMake(pszNewDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirMake(Psz2String(pszNewDir))
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirremove/*" />
FUNCTION DirRemove(pszDirName AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirRemove(Psz2String(pszDirName))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskchange/*" />
FUNCTION DiskChange(pszDrive AS PSZ) AS LOGIC
	RETURN XSharp.Core.Functions.DiskChange(Psz2String(pszDrive))
 
	

/// <include file="XSharp.RT.Docs.xml" path="doc/GetRTFullPath/*" />
FUNCTION GetRTFullPath() AS STRING
	RETURN Assembly.GetAssembly(TYPEOF(XSharp.__Usual)):Location

/// <include file="XSharp.RT.Docs.xml" path="doc/GetThreadCount/*" />
FUNCTION GetThreadCount() AS DWORD
	RETURN (DWORD) System.Diagnostics.Process.GetCurrentProcess():Threads:Count


/// <include file="XSharp.RT.Docs.xml" path="doc/GetTickCountLow/*" />
FUNCTION GetTickCountLow() AS DWORD
	RETURN (DWORD)Environment.TickCount*10	
