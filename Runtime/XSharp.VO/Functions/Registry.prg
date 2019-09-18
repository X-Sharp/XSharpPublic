//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
INTERNAL DEFINE VO_APPS			:= "Software\ComputerAssociates\CA-Visual Objects Applications"
INTERNAL DEFINE VO_LOCALMACHINE	:= "HKEY_LOCAL_MACHINE\"+VO_APPS 
INTERNAL DEFINE VO_CURRENTUSER	:= "HKEY_CURRENT_USER\"+VO_APPS 
USING Microsoft.Win32
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setrtregint/*" />
FUNCTION SetRTRegInt(cSubKey AS STRING,cKey AS STRING,dwKeyValue AS DWORD) AS LOGIC
	LOCAL cKey AS STRING
	cKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY
		IF Registry.GetValue(cKey,cKey,NULL) != NULL
			Registry.SetValue(cKey,cKey,dwKeyValue)
		ENDIF
	END TRY
	// always write to HKCU
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cKey,cKey,dwKeyValue)
	END TRY
	RETURN ! Registry.GetValue(cKey,cKey,NULL) == NULL
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setrtregstring/*" />
FUNCTION SetRTRegString(cSubKey AS STRING,cKey AS STRING,cValue AS STRING) AS LOGIC
	LOCAL cKey AS STRING
	cKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY    
		IF Registry.GetValue(cKey,cKey,NULL) != NULL
			Registry.SetValue(cKey,cKey,cValue)
		ENDIF
	END TRY
	// always write to HKCU
	cKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cKey,cKey,cValue)
	END TRY
	RETURN ! Registry.GetValue(cKey,cKey,NULL) == NULL
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/queryrtregint/*" />
FUNCTION QueryRTRegInt(cSubKey AS STRING,cKey AS STRING) AS DWORD
	LOCAL cParentKey AS STRING
	LOCAL o AS OBJECT
	
	// First try to read from HKCU
	cParentKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cParentKey += "\" + cSubKey
	ENDIF
	o := Registry.GetValue(cKey,cKey,NULL)
	// If that fails then read from HKLM
	IF o == NULL
		cParentKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cParentKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cParentKey,cKey,NULL)
		END TRY
		IF o == NULL
			o := 0
		ENDIF
	ENDIF
	
	RETURN Convert.ToUInt32(o)
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/queryrtregstring/*" />
FUNCTION QueryRTRegString(cSubKey AS STRING,cKey AS STRING) AS STRING
	LOCAL cParentKey AS STRING
	LOCAL o AS OBJECT
	
	cParentKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cParentKey += "\" + cSubKey
	ENDIF
	
	o := Registry.GetValue(cParentKey,cKey,NULL)
	IF o == NULL
		cParentKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cParentKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cParentKey,cKey,NULL)
		END TRY
		IF o == NULL
			o := ""
		ENDIF
	ENDIF
	
	RETURN Convert.ToString(o)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/deletertregkey/*" />
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
