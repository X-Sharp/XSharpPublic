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
	LOCAL cFullKey AS STRING
	cFullKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY
		IF Registry.GetValue(cFullKey,cKey,NULL) != NULL
			Registry.SetValue(cFullKey,cKey,dwKeyValue)
		ENDIF
    CATCH
        NOP
	END TRY
	// always write to HKCU
	cFullKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cFullKey,cKey,dwKeyValue)
    CATCH
        NOP
	END TRY
	RETURN ! Registry.GetValue(cFullKey,cKey,NULL) == NULL
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setrtregstring/*" />
FUNCTION SetRTRegString(cSubKey AS STRING,cKey AS STRING,cValue AS STRING) AS LOGIC
	LOCAL cFullKey AS STRING
	cFullKey := VO_LOCALMACHINE
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	// try HKLM first
	TRY    
		IF Registry.GetValue(cFullKey,cKey,NULL) != NULL
			Registry.SetValue(cFullKey,cKey,cValue)
		ENDIF
    CATCH
        NOP
	END TRY
	// always write to HKCU
	cFullKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	TRY    
		Registry.SetValue(cFullKey,cKey,cValue)
    CATCH
        NOP
	END TRY
	RETURN ! Registry.GetValue(cFullKey,cKey,NULL) == NULL
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/queryrtregint/*" />
FUNCTION QueryRTRegInt(cSubKey AS STRING,cKey AS STRING) AS DWORD
	LOCAL cFullKey AS STRING
	LOCAL o AS OBJECT
	
	// First try to read from HKCU
	cFullKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	o := Registry.GetValue(cKey,cKey,NULL)
	// If that fails then read from HKLM
	IF o == NULL
		cFullKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cFullKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cFullKey,cKey,NULL)
        CATCH
            o := NULL
		END TRY
		IF o == NULL
		    o := 0
		ENDIF
	ENDIF
	
	RETURN Convert.ToUInt32(o)
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/queryrtregstring/*" />
FUNCTION QueryRTRegString(cSubKey AS STRING,cKey AS STRING) AS STRING
	LOCAL cFullKey AS STRING
	LOCAL o AS OBJECT
	
	cFullKey := VO_CURRENTUSER
	IF ! String.IsNullOrEmpty( cSubKey )
		cFullKey += "\" + cSubKey
	ENDIF
	
	o := Registry.GetValue(cFullKey,cKey,NULL)
	IF o == NULL
		cFullKey := VO_LOCALMACHINE
		IF ! String.IsNullOrEmpty( cSubKey )
			cFullKey += "\" + cSubKey
		ENDIF
		TRY
			o := Registry.GetValue(cFullKey,cKey,NULL)
        CATCH
            o := NULL
		END TRY
		IF o == NULL
			o := ""
		ENDIF
	ENDIF
	
	RETURN Convert.ToString(o)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/deletertregkey/*" />
FUNCTION DeleteRTRegKey(cSubKey AS STRING) AS LOGIC
    LOCAL oKey := NULL AS RegistryKey
    lOCAL succeeded := FALSE AS LOGIC
	TRY
	    oKey := Registry.LocalMachine:OpenSubKey(VO_APPS, TRUE)
	    IF oKey != NULL
	        oKey:DeleteSubKey(cSubKey)
            succeeded := TRUE
        ENDIF
    CATCH
        NOP
    END TRY
	TRY
	    oKey := Registry.CurrentUser:OpenSubKey(VO_APPS, TRUE)
	    IF oKey != NULL
	        oKey:DeleteSubKey(cSubKey)
            succeeded := TRUE
        ENDIF
    CATCH
        NOP
    END TRY
RETURN succeeded
