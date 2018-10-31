VOSTRUCT _winval_context
	MEMBER   valuelen AS INT
	MEMBER  value_context AS PTR
	MEMBER val_buff_ptr AS PTR


VOSTRUCT _winpvalue
	MEMBER  pv_valuename AS PSZ
	MEMBER  pv_valuelen AS INT
	MEMBER  pv_value_context AS PTR
	MEMBER  pv_type AS DWORD








VOSTRUCT _winprovider_info
	MEMBER pi_R0_1val AS PTR
	MEMBER pi_R0_allvals AS PTR
	MEMBER pi_R3_1val AS PTR
	MEMBER pi_R3_allvals AS PTR
	MEMBER pi_flags AS DWORD
	MEMBER pi_key_context AS PTR



VOSTRUCT _winVALENT
	MEMBER   ve_valuename AS PSZ
	MEMBER   ve_valuelen AS DWORD
	MEMBER   ve_valueptr AS DWORD
	MEMBER   ve_type AS DWORD




_DLL FUNC RegCloseKey(hKey AS PTR) AS LONG PASCAL:ADVAPI32.RegCloseKey



_DLL FUNC RegConnectRegistry(lpMachineName AS PSZ, hKey AS PTR, phkResult AS PTR);
	AS LONG PASCAL:ADVAPI32.RegConnectRegistryA




_DLL FUNC RegCreateKey( hKey AS PTR, lpSubKey AS PSZ, phkResult AS PTR);
	AS LONG PASCAL:ADVAPI32.RegCreateKeyA




_DLL FUNC RegCreateKeyEx(hKey AS PTR, lpSubKey AS PSZ, Reserved AS DWORD,;
	lpClass AS PSZ, dwOptions AS DWORD, samDesired AS DWORD,;
	lpSecurityAttributes AS _winSECURITY_ATTRIBUTES,;
	phkResult AS PTR, lpdwDisposition AS DWORD PTR);
	AS LONG PASCAL:ADVAPI32.RegCreateKeyExA




_DLL FUNC RegDeleteKey(hKey AS PTR, lpSubKey AS PSZ) AS LONG PASCAL:ADVAPI32.RegDeleteKeyA






_DLL FUNC RegDeleteValue(hKey AS PTR, lpValueName AS PSZ) AS LONG PASCAL:ADVAPI32.RegDeleteValueA




_DLL FUNC RegEnumKey(hKey AS PTR, dwIndex AS DWORD, lpName AS PSZ, cbName AS DWORD);
	AS LONG PASCAL:ADVAPI32.RegEnumKeyA




_DLL FUNC RegEnumKeyEx(hKey AS PTR, dwIndex AS DWORD, lpName AS PSZ, lpcbName AS DWORD PTR,;
	lpReserved AS DWORD PTR, lpClasss AS PSZ, lpcbClass AS DWORD PTR,;
	lpftLastWriteTime AS _winFILETIME);
	AS LONG PASCAL:ADVAPI32.RegEnumKeyExA




_DLL FUNC RegEnumValue(hKey AS PTR, dwIndex AS DWORD, lpValueName AS PSZ,;
	lpcbValueName AS DWORD PTR, lpReserved AS DWORD PTR,;
	lpType AS DWORD PTR, lpData AS BYTE PTR, lpcbData AS DWORD PTR);
	AS LONG PASCAL:ADVAPI32.RegEnumValueA



_DLL FUNC RegFlushKey(hKey AS PTR) AS LONG PASCAL:ADVAPI32.RegFlushKey


_DLL FUNC  RegGetKeySecurity(hKey AS PTR, SecurityInformation AS DWORD,;
	pSrcurityDescriptor AS _winSECURITY_DESCRIPTOR,;
	lpcbSecurityDescriptor AS DWORD PTR);
	AS LONG PASCAL:ADVAPI32.RegGetKeySecurity


_DLL FUNC RegLoadKey(hKey AS PTR, lpSubKey AS PSZ, lpFile AS PSZ);
	AS LONG PASCAL:ADVAPI32.RegLoadKeyA



_DLL FUNC RegNotifyChangeKeyValue(hKey AS PTR, bWatchSubtree AS LOGIC,;
	dwNotifyFilter AS DWORD, hEvent AS PTR,;
	fAsynChronus AS LOGIC );
	AS LONG PASCAL:ADVAPI32.RegNotifyChangeKeyValue

_DLL FUNC RegOpenKey(hKey AS PTR, lpSubKey AS PSZ, phkResult AS PTR);
	AS LONG PASCAL:ADVAPI32.RegOpenKeyA





_DLL FUNC RegOpenKeyEx(hKey AS PTR, lpSubKey AS PSZ, ulOPtions AS DWORD,;
	samDesired AS DWORD, phkResult AS PTR);
	AS LONG PASCAL:ADVAPI32.RegOpenKeyExA



_DLL FUNC RegQueryInfoKey(hKey AS PTR, lpClass AS PSZ, lpcbClass AS DWORD PTR,;
	lpReserved AS DWORD PTR, lpcSubKey AS DWORD PTR,;
	lpcbMaxSubkeyLen AS DWORD PTR, lpcbMaxClassLen AS DWORD PTR,;
	lpcValues AS DWORD PTR,lpcbMaxValueNameLen AS DWORD PTR,;
	lpcbMaxValueLen AS DWORD PTR, lpcbSecurityDescriptor AS DWORD PTR,;
	lpftLastWriteTime AS _winFILETIME);
	AS LONG PASCAL:ADVAPI32.RegQueryInfoKeyA





_DLL FUNC RegQueryValue(hKey AS PTR, lpSubKey AS PSZ, lpValue AS PSZ,;
	lpcbValue AS LONG PTR) AS LONG PASCAL:ADVAPI32.RegQueryValueA





_DLL FUNC RegQueryMultipleValues(hKey AS PTR, val_list AS _winVALENT,  num_vals AS DWORD,;
	lpValueBuf AS PSZ, ldwTotsize AS DWORD PTR);
	AS LONG PASCAL:ADVAPI32.RegQueryMultipleValuesA




_DLL FUNC RegQueryValueEx(hKey AS PTR, lpValueName AS PSZ, lpReserved AS  DWORD PTR,;
	lpType AS DWORD PTR, lpData AS BYTE PTR, lpcbData AS DWORD PTR);
	AS LONG PASCAL:ADVAPI32.RegQueryValueExA




_DLL FUNC RegReplaceKey( hKey AS PTR, lpSubKey AS PSZ, lpNewFile AS PSZ,;
	lpOldFile AS PSZ) AS LONG PASCAL:ADVAPI32.RegReplaceKeyA




_DLL FUNC RegRestoreKey(hKey AS PTR, lpFile AS PSZ, dwFlags AS DWORD);
	AS LONG PASCAL:ADVAPI32.RegRestoreKeyA




_DLL FUNC RegSaveKey(hKey AS PTR, lpFile AS PSZ,;
	lpSecurityAttributes AS _winSECURITY_ATTRIBUTES);
	AS LONG PASCAL:ADVAPI32.RegSaveKeyA



_DLL FUNC RegSetKeySecurity( hKey AS PTR, SecurityInformation AS DWORD,;
	pSecurityDescriptor AS _winSECURITY_DESCRIPTOR);
	AS LONG PASCAL:ADVAPI32.RegSetKeySecurity



_DLL FUNC RegSetValue(hKey AS PTR, lpSubKey AS PSZ, dwType AS DWORD, lpData AS PSZ,;
	cbData AS DWORD) AS LONG PASCAL:ADVAPI32.RegSetValueA





_DLL FUNC RegSetValueEx(hKey AS PTR, lpValueName AS PSZ, ReservED AS DWORD,;
	dwType AS DWORD, lpData AS BYTE PTR, cdData AS DWORD);
	AS LONG PASCAL:ADVAPI32.RegSetValueExA





_DLL FUNC RegUnLoadKey(hKey AS PTR, lpSubKey AS PSZ) AS LONG PASCAL:ADVAPI32.RegUnLoadKeyA




_DLL FUNC InitiateSystemShutdown(lpMachineName AS PSZ, lpMessage AS PSZ,;
	dwTimeoutt AS DWORD, bForceAppsClosed AS LOGIC,;
	bRebootAfterShutdown AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.InitiateSystemShutdownA





_DLL FUNC AbortSystemShutdown(lpMachineName AS PSZ) AS LOGIC PASCAL:ADVAPI32.AbortSystemShutdownA




#region defines
DEFINE RRF_RT_REG_NONE        := 0x00000001  // restrict type to REG_NONE      (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_REG_SZ          := 0x00000002  // restrict type to REG_SZ        (other data types will not return ERROR_SUCCESS) (automatically converts REG_EXPAND_SZ to REG_SZ unless RRF_NOEXPAND is specified)
DEFINE RRF_RT_REG_EXPAND_SZ   := 0x00000004  // restrict type to REG_EXPAND_SZ (other data types will not return ERROR_SUCCESS) (must specify RRF_NOEXPAND or RegGetValue will fail with ERROR_INVALID_PARAMETER)
DEFINE RRF_RT_REG_BINARY      := 0x00000008  // restrict type to REG_BINARY    (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_REG_DWORD       := 0x00000010  // restrict type to REG_DWORD     (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_REG_MULTI_SZ    := 0x00000020  // restrict type to REG_MULTI_SZ  (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_REG_QWORD       := 0x00000040  // restrict type to REG_QWORD     (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_DWORD           := (RRF_RT_REG_BINARY | RRF_RT_REG_DWORD) // restrict type to *32-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_QWORD           := (RRF_RT_REG_BINARY | RRF_RT_REG_QWORD) // restrict type to *64-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
DEFINE RRF_RT_ANY             := 0x0000ffff                             // no type restriction
DEFINE RRF_NOEXPAND           := 0x10000000  // do not automatically expand environment strings if value is of type REG_EXPAND_SZ
DEFINE RRF_ZEROONFAILURE      := 0x20000000  // if pvData is not NULL, set content to all zeros on failure
//
// Reserved Key Handles.
//
DEFINE HKEY_CLASSES_ROOT          := PTR (_CAST,0x80000000)
DEFINE HKEY_CURRENT_USER          := PTR (_CAST,0x80000001)
DEFINE HKEY_LOCAL_MACHINE         := PTR (_CAST,0x80000002)
DEFINE HKEY_USERS                 := PTR (_CAST,0x80000003)
DEFINE HKEY_PERFORMANCE_DATA      := PTR (_CAST,0x80000004)
DEFINE HKEY_PERFORMANCE_TEXT     := PTR (_CAST,0x80000050) 
DEFINE HKEY_PERFORMANCE_NLSTEXT  := PTR (_CAST,0x80000060) 
DEFINE HKEY_CURRENT_CONFIG        := PTR (_CAST,0x80000005)
DEFINE HKEY_DYN_DATA              := PTR (_CAST,0x80000006)
DEFINE PROVIDER_KEEPS_VALUE_LENGTH   := 0x1
DEFINE WIN31_CLASS                := NULL
#endregion
