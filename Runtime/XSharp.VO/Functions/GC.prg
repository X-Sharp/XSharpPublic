//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function AllowCollectThread() as void
	/// THROW NotImplementedException{}
	return  


/// <summary>
/// Call the garbage collector if a watermark is reached.
/// </summary>
/// <returns>
/// </returns>
function Collect() as void
	/// THROW NotImplementedException{}
	return  


/// <summary>
/// Count the number of garbage collections performed by the system.
/// </summary>
/// <returns>
/// </returns>
function CollectCount() as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Call the garbage collector.
/// </summary>
/// <returns>
/// </returns>
function CollectForced() as void
	/// THROW NotImplementedException{}
	return  


[Obsolete( "'AxitCalled()' is not supported and always returns FALSE" )] ;
	function AxitCalled(o as object) as logic
return false  

/// <summary>
/// Check whether the application is in a garbage collection phase.
/// </summary>
/// <returns>
/// </returns>
function InCollect() as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Register an object so that it receives an Axit message before being destroyed by the garbage collector.
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
function RegisterAxit(oSource as object) as void
	/// THROW NotImplementedException{}
	return 


/// <summary>
/// Terminate the registration of an object that has been registered with RegisterAxit().
/// </summary>
/// <param name="oSource"></param>
/// <returns>
/// </returns>
function UnRegisterAxit(oSource as object) as logic
	/// THROW NotImplementedException{}
	return false   
