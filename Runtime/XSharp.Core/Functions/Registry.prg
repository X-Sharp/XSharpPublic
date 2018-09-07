//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
DEFINE VO_APPS			:= "Software\ComputerAssociates\CA-Visual Objects Applications"
DEFINE VO_LOCALMACHINE	:= "HKEY_LOCAL_MACHINE\"+VO_APPS 
DEFINE VO_CURRENTUSER	:= "HKEY_CURRENT_USER\"+VO_APPS 
USING Microsoft.Win32
/// <summary>
/// Save a numeric value to the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <param name="nKeyVal"></param>
/// <returns>
/// </returns>
FUNCTION SetRTRegInt(cSubKey AS STRING,cKeyName AS STRING,nKeyVal AS DWORD) AS LOGIC
	LOCAL cKey AS STRING
	cKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY
		IF Registry.GetValue(cKey,cKeyName,NULL) != NULL
			Registry.SetValue(cKey,cKeyName,nKeyVal)
		ENDIF
	END TRY
	// always write to HKCU
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cKey,cKeyName,nKeyVal)
	END TRY
	RETURN ! Registry.GetValue(cKey,cKeyName,NULL) == NULL
	
	
	/// <summary>
	/// Save a string value to the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <param name="cKeyVal"></param>
	/// <returns>
	/// </returns>
FUNCTION SetRTRegString(cSubKey AS STRING,cKeyName AS STRING,cKeyVal AS STRING) AS LOGIC
	LOCAL cKey AS STRING
	cKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY    
		IF Registry.GetValue(cKey,cKeyName,NULL) != NULL
			Registry.SetValue(cKey,cKeyName,cKeyVal)
		ENDIF
	END TRY
	// always write to HKCU
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cKey,cKeyName,cKeyVal)
	END TRY
	RETURN ! Registry.GetValue(cKey,cKeyName,NULL) == NULL
	
	/// <summary>
	/// Retrieve a numeric value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
FUNCTION QueryRTRegInt(cSubKey AS STRING,cKeyName AS STRING) AS DWORD
	LOCAL cKey AS STRING
	LOCAL o AS OBJECT
	
	// First try to read from HKCU
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	o := Registry.GetValue(cKey,cKeyName,NULL)
	// If that fails then read from HKLM
	IF o == NULL
		cKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cKey,cKeyName,NULL)
		END TRY
		IF o == NULL
			o := 0
		ENDIF
	ENDIF
	
	RETURN Convert.ToUInt32(o)
	/// <summary>
	/// Retrieve a string value from the Registry.
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <param name="cKeyName"></param>
	/// <returns>
	/// </returns>
FUNCTION QueryRTRegString(cSubKey AS STRING,cKeyName AS STRING) AS STRING
	LOCAL cKey AS STRING
	LOCAL o AS OBJECT
	
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	
	o := Registry.GetValue(cKey,cKeyName,NULL)
	IF o == NULL
		cKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cKey,cKeyName,NULL)
		END TRY
		IF o == NULL
			o := ""
		ENDIF
	ENDIF
	
	RETURN Convert.ToString(o)
	
	
	/// <summary>
	/// </summary>
	/// <param name="cSubKey"></param>
	/// <returns>
	/// </returns>
FUNCTION DeleteRTRegKey(cSubKey AS STRING) AS LOGIC
LOCAL oKey := NULL AS RegistryKey 
	TRY
	oKey := Registry.LocalMachine:OpenSubKey(VO_APPS, TRUE)
		IF (oKey != NULL)
	oKey:DeleteSubKey(cSubKey)
ENDIF
END TRY
	TRY
	oKey := Registry.CurrentUser:OpenSubKey(VO_APPS, TRUE)
		IF (oKey != NULL)
	oKey:DeleteSubKey(cSubKey)
ENDIF
END TRY
RETURN oKey != NULL_OBJECT
