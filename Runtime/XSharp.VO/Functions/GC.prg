//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION AllowCollectThread() AS VOID
	/// THROW NotImplementedException{}
	RETURN  


/// <summary>
/// Call the garbage collector if a watermark is reached.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Collect() AS VOID
	/// THROW NotImplementedException{}
	RETURN  


/// <summary>
/// Count the number of garbage collections performed by the system.
/// </summary>
/// <returns>
/// </returns>
FUNCTION CollectCount() AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Call the garbage collector.
/// </summary>
/// <returns>
/// </returns>
FUNCTION CollectForced() AS VOID
	/// THROW NotImplementedException{}
	RETURN  


[Obsolete( "'AxitCalled()' is not supported and always returns FALSE" )] ;
	FUNCTION AxitCalled(o AS OBJECT) AS LOGIC
RETURN FALSE  

/// <summary>
/// Check whether the application is in a garbage collection phase.
/// </summary>
/// <returns>
/// </returns>
FUNCTION InCollect() AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   



/// <summary>
/// Register an object so that it receives an Axit message before being destroyed by the garbage collector.
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
FUNCTION RegisterAxit(oSource AS OBJECT) AS VOID
	/// THROW NotImplementedException{}
	RETURN 


/// <summary>
/// Terminate the registration of an object that has been registered with RegisterAxit().
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
FUNCTION UnRegisterAxit(oSource AS OBJECT) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   
