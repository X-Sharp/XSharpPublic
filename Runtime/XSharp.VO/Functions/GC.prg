//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


[Obsolete( "'AllowCollectThread()' is not supported" )] ;
FUNCTION AllowCollectThread() AS VOID
	RETURN  


/// <summary>
/// Call the garbage collector if a watermark is reached.
/// </summary>
/// <returns>
/// </returns>
[Obsolete( "'Collect()' is not supported" )] ;
FUNCTION Collect() AS VOID
	RETURN  


/// <summary>
/// Count the number of garbage collections performed by the system.
/// </summary>
/// <returns>
/// </returns>
[Obsolete( "'CollectCount()' is not supported" )] ;
FUNCTION CollectCount() AS DWORD
	return 0

/// <summary>
/// Call the garbage collector.
/// </summary>
/// <returns>
/// </returns>
[Obsolete( "'CollectForced()' is not supported" )] ;
FUNCTION CollectForced() AS VOID
	RETURN  

/// <exclude/>
[Obsolete( "'AxitCalled()' is not supported and always returns FALSE" )] ;
	FUNCTION AxitCalled(o AS OBJECT) AS LOGIC
RETURN FALSE  

/// <summary>
/// Check whether the application is in a garbage collection phase.
/// </summary>
/// <returns>
/// </returns>
[Obsolete( "'InCollect()' is not supported" )] ;
FUNCTION InCollect() AS LOGIC
	RETURN FALSE   



/// <summary>
/// Register an object so that it receives an Axit message before being destroyed by the garbage collector.
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
[Obsolete( "'RegisterAxit()' is not supported and has no effect.")];
FUNCTION RegisterAxit(oSource AS OBJECT) AS VOID
	RETURN 


/// <summary>
/// Terminate the registration of an object that has been registered with RegisterAxit().
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
FUNCTION UnRegisterAxit(oSource AS OBJECT) AS LOGIC
	GC.SuppressFinalize( oSource )
	RETURN FALSE   
