//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Sql.xml" path="doc/SQLData/*" />
[XSharp.Internal.TypesChanged];
[Obsolete];
CLASS SQLData
	EXPORT @@Null 			AS LOGIC
	EXPORT ValueChanged 	AS LOGIC
	EXPORT Length			AS DWORD
	EXPORT LongValue		AS STRING


/// <include file="Sql.xml" path="doc/SQLData.ctor/*" />
[Obsolete];
CONSTRUCTOR() STRICT
    RETURN


/// <include file="Sql.xml" path="doc/SQLData.Clear/*" />
[Obsolete];
METHOD Clear() AS VOID STRICT
	RETURN


/// <include file="Sql.xml" path="doc/SQLData.Initialize/*" />
[Obsolete];
METHOD Initialize( pData AS IntPtr, pLength AS IntPtr, nLen AS DWORD, lNull AS LOGIC, lChanged AS LOGIC) AS VOID STRICT
	RETURN
END CLASS


