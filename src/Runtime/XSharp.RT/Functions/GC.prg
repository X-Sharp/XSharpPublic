//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


[Obsolete( "'AllowCollectThread()' is not supported" )] ;
FUNCTION AllowCollectThread() AS VOID
	RETURN


/// <exclude />
[Obsolete( "'Collect()' is not supported" )] ;
FUNCTION Collect() AS VOID
	RETURN


/// <exclude />
[Obsolete( "'CollectCount()' is not supported" )] ;
FUNCTION CollectCount() AS DWORD
	return 0

/// <exclude />
[Obsolete( "'CollectForced()' is not supported" )] ;
FUNCTION CollectForced() AS VOID
	RETURN

/// <exclude/>
[Obsolete( "'AxitCalled()' is not supported and always returns FALSE" )] ;
FUNCTION AxitCalled(o AS OBJECT) AS LOGIC
RETURN FALSE

/// <exclude />
[Obsolete( "'InCollect()' is not supported" )] ;
FUNCTION InCollect() AS LOGIC
	RETURN FALSE


[Obsolete( "'_RegisterExit()' is not supported and has no effect, use 'AppDomain.CurrentDomain:ProcessExit' in stead..",TRUE)];
FUNCTION _RegisterExit(aFunction AS USUAL) AS VOID
	RETURN



/// <exclude />
[Obsolete( "'RegisterAxit()' is not supported and has no effect.")];
FUNCTION RegisterAxit(oSource AS OBJECT) AS VOID
	RETURN


/// <include file="XSharp.RT.Docs.xml" path="doc/UnRegisterAxit/*" />
FUNCTION UnRegisterAxit(oSource AS OBJECT) AS LOGIC
	GC.SuppressFinalize( oSource )
	RETURN FALSE
