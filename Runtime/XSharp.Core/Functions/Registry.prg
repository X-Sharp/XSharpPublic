//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
define VO_APPS			:= "Software\ComputerAssociates\CA-Visual Objects Applications"
define VO_LOCALMACHINE	:= "HKEY_LOCAL_MACHINE\"+VO_APPS 
define VO_CURRENTUSER	:= "HKEY_CURRENT_USER\"+VO_APPS 
using Microsoft.Win32
/// <summary>
/// Save a numeric value to the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <param name="nKeyVal"></param>
/// <returns>
/// </returns>
function SetRTRegInt(cSubKey as string,cKeyName as string,nKeyVal as dword) as logic
	local cKey as string
	cKey := VO_LOCALMACHINE
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	// try HKLM first
	try
		if Registry.GetValue(cKey,cKeyName,null) != null
			Registry.SetValue(cKey,cKeyName,nKeyVal)
		endif
	end try
	// always write to HKCU
	cKey := VO_CURRENTUSER
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	try    
		Registry.SetValue(cKey,cKeyName,nKeyVal)
	end try
	return ! Registry.GetValue(cKey,cKeyName,null) == null
	
	
	/// <summary>
	/// Save a string value to the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <param name="cKeyVal"></param>
	/// <returns>
	/// </returns>
function SetRTRegString(cSubKey as string,cKeyName as string,cKeyVal as string) as logic
	local cKey as string
	cKey := VO_LOCALMACHINE
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	// try HKLM first
	try    
		if Registry.GetValue(cKey,cKeyName,null) != null
			Registry.SetValue(cKey,cKeyName,cKeyVal)
		endif
	end try
	// always write to HKCU
	cKey := VO_CURRENTUSER
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	try    
		Registry.SetValue(cKey,cKeyName,cKeyVal)
	end try
	return ! Registry.GetValue(cKey,cKeyName,null) == null
	
	/// <summary>
	/// Retrieve a numeric value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
function QueryRTRegInt(cSubKey as string,cKeyName as string) as dword
	local cKey as string
	local o as object
	
	// First try to read from HKCU
	cKey := VO_CURRENTUSER
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	o := Registry.GetValue(cKey,cKeyName,null)
	// If that fails then read from HKLM
	if o == null
		cKey := VO_LOCALMACHINE
		if ! String.IsNullOrEmpty( cSubKey )
			cKey += "\" + cSubKey
		endif
		try
			o := Registry.GetValue(cKey,cKeyName,null)
		end try
		if o == null
			o := 0
		endif
	endif
	
	return Convert.ToUInt32(o)
	/// <summary>
	/// Retrieve a string value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
function QueryRTRegString(cSubKey as string,cKeyName as string) as string
	local cKey as string
	local o as object
	
	cKey := VO_CURRENTUSER
	if ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	endif
	
	o := Registry.GetValue(cKey,cKeyName,null)
	if o == null
		cKey := VO_LOCALMACHINE
		if ! String.IsNullOrEmpty( cSubKey )
			cKey += "\" + cSubKey
		endif
		try
			o := Registry.GetValue(cKey,cKeyName,null)
		end try
		if o == null
			o := ""
		endif
	endif
	
	return Convert.ToString(o)
	
	
	/// <summary>
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <returns>
	/// </returns>
function DeleteRTRegKey(cSubKey as string) as logic
local oKey := NULL as RegistryKey 
	try
	oKey := Registry.LocalMachine:OpenSubKey(VO_APPS, true)
		if (oKey != null)
	oKey:DeleteSubKey(cSubKey)
endif
end try
	try
	oKey := Registry.CurrentUser:OpenSubKey(VO_APPS, true)
		if (oKey != null)
	oKey:DeleteSubKey(cSubKey)
endif
end try
return oKey != null_object
