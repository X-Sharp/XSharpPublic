VOSTRUCT _WINRASCONN 
	MEMBER dwSize AS DWORD
	MEMBER hrasconn AS PTR
	MEMBER DIM szEntryName[ RAS_MaxEntryName + 1 ] AS BYTE
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE
	MEMBER DIM szPhonebook [ MAX_PATH ] AS BYTE
	MEMBER dwSubEntry AS DWORD 
	// RvdH 070411 added
	MEMBER guidEntry	IS _WINGUID
	MEMBER dwFlags	AS DWORD
	MEMBER luid	IS _WINLUID    


VOSTRUCT _winRASCONNSTATUS 
	MEMBER dwSize AS DWORD
	MEMBER rasconnstate AS DWORD
	MEMBER dwError AS DWORD
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE
	MEMBER DIM szPhoneNumber[ RAS_MaxPhoneNumber + 1 ] AS BYTE


VOSTRUCT _winRASDIALPARAMS
	MEMBER dwSize AS DWORD
	MEMBER DIM szEntryName[ RAS_MaxEntryName + 1 ] AS BYTE
	MEMBER DIM szPhoneNumber[ RAS_MaxPhoneNumber + 1 ] AS BYTE
	MEMBER DIM szCallbackNumber[ RAS_MaxCallbackNumber + 1 ] AS BYTE
	MEMBER DIM szUserName[ UNLEN + 1 ] AS BYTE
	MEMBER DIM szPassword[ PWLEN + 1 ] AS BYTE
	MEMBER DIM szDomain[ DNLEN + 1 ] AS BYTE
	MEMBER dwSubEntry AS DWORD
	MEMBER dwCallbackId AS DWORD


VOSTRUCT _winRASDIALEXTENSIONS 
	MEMBER dwSize AS DWORD
	MEMBER dwfOptions AS DWORD
	MEMBER hwndParent AS DWORD
	MEMBER reserved AS DWORD 
	// RvdH 070411 added
	MEMBER reserved1	AS DWORD
	MEMBER RasEapInfo	IS _WinRasEAPInfo
VOSTRUCT _WINRASEAPINFO	// RvdH 070411 added 
	MEMBER dwSizeofEapInfo AS DWORD
	MEMBER pbEapInfo		AS PTR

VOSTRUCT _winRASENTRYNAME ALIGN 4
	MEMBER dwSize AS DWORD
	MEMBER DIM szEntryName[ RAS_MaxEntryName + 1 ] AS BYTE
	// RvdH 070411 added
	MEMBER dwFlags AS DWORD
	MEMBER DIM szPhonebookPath[MAX_PATH + 1] AS BYTE





VOSTRUCT _winRASAMB ALIGN 2    // RvdH 070411 changed alignment from 4 to 2
	MEMBER dwSize AS DWORD
	MEMBER dwError AS DWORD
	MEMBER DIM szNetBiosError[ NETBIOS_NAME_LEN + 1 ] AS BYTE
	MEMBER bLana AS BYTE






VOSTRUCT _winRASPPPNBF     // RvdH 070411 removed alignment
	MEMBER dwSize AS DWORD
	MEMBER dwError AS DWORD
	MEMBER dwNetBiosError AS DWORD
	MEMBER DIM szNetBiosError[ NETBIOS_NAME_LEN + 1 ] AS BYTE
	MEMBER DIM szWorkstationName[ NETBIOS_NAME_LEN + 1 ] AS BYTE
	MEMBER bLana AS  BYTE


VOSTRUCT _winRASPPPIPX  
	MEMBER dwSize AS DWORD
	MEMBER dwError AS DWORD
	MEMBER DIM szIpxAddress[ RAS_MaxIpxAddress + 1 ] AS BYTE

VOSTRUCT _winRASPPPIP    // RvdH 070411 removed alignment
	MEMBER dwSize AS DWORD
	MEMBER dwError AS DWORD
	MEMBER DIM szIpAddress[ RAS_MaxIpAddress + 1 ] AS BYTE
	MEMBER DIM szServerIpAddress[ RAS_MaxIpAddress + 1 ] AS BYTE
	MEMBER dwOptions AS DWORD
	MEMBER dwServerOptions AS DWORD





	/* RASPPPLCP 'dwAuthenticatonProtocol' values.
	*/
VOSTRUCT _winRASPPPLCP ALIGN 2     // RvdH 070411 changed alignment from 4 to 2
	MEMBER dwSize AS DWORD
	MEMBER fBundled AS LOGIC
	
	MEMBER dwError                                 AS DWORD
	MEMBER dwAuthenticationProtocol                AS DWORD
	MEMBER dwAuthenticationData                    AS DWORD
	MEMBER dwEapTypeId                             AS DWORD
	MEMBER dwServerAuthenticationProtocol          AS DWORD
	MEMBER dwServerAuthenticationData              AS DWORD
	MEMBER dwServerEapTypeId                       AS DWORD
	MEMBER fMultilink                             AS LOGIC
	MEMBER dwTerminateReason                       AS DWORD
	MEMBER dwServerTerminateReason                 AS DWORD
	MEMBER DIM szReplyMessage[RAS_MaxReplyMessage]  AS BYTE
	MEMBER dwOptions                               AS DWORD
	MEMBER dwServerOptions                         AS DWORD








VOSTRUCT _winRASSLIP ALIGN 2    // RvdH 070411 changed alignment from 4 to 2
	MEMBER dwSize AS DWORD
	MEMBER dwError AS DWORD
	MEMBER DIM szIpAddress[ RAS_MaxIpAddress + 1 ] AS BYTE





VOSTRUCT _winRASDEVINFO ALIGN 2    // RvdH 070411 changed alignment from 4 to 2
	MEMBER dwSize AS DWORD
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE




VOSTRUCT _winRASCTRYINFO ALIGN 4
	MEMBER dwSize AS DWORD
	MEMBER dwCountryID AS DWORD
	MEMBER dwNextCountryID AS DWORD
	MEMBER dwCountryCode AS DWORD
	MEMBER dwCountryNameOffset AS DWORD




VOSTRUCT _winRASIPADDR     // RvdH 070411 removed alignment 
	MEMBER a AS BYTE
	MEMBER b AS BYTE
	MEMBER c AS BYTE
	MEMBER d AS BYTE
VOSTRUCT _winRASENTRY     // RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER dwfOptions AS DWORD
	MEMBER dwCountryID AS DWORD
	MEMBER dwCountryCode AS DWORD
	MEMBER DIM szAreaCode[ RAS_MaxAreaCode + 1 ] AS BYTE
	MEMBER DIM szLocalPhoneNumber[ RAS_MaxPhoneNumber + 1 ] AS BYTE
	MEMBER dwAlternateOffset AS DWORD
	MEMBER ipaddr IS _winRASIPADDR
	MEMBER ipaddrDns IS _winRASIPADDR
	MEMBER ipaddrDnsAlt IS _winRASIPADDR
	MEMBER ipaddrWins IS _winRASIPADDR
	MEMBER ipaddrWinsAlt IS _winRASIPADDR
	MEMBER dwFrameSize AS DWORD
	MEMBER dwfNetProtocols AS DWORD
	MEMBER dwFramingProtocol AS DWORD
	MEMBER DIM szScript[ MAX_PATH ] AS BYTE
	MEMBER DIM szAutodialDll[ MAX_PATH ] AS BYTE
	MEMBER DIM szAutodialFunc[ MAX_PATH ] AS BYTE
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE
	MEMBER DIM szX25PadType[ RAS_MaxPadType + 1 ] AS BYTE
	MEMBER DIM szX25Address[ RAS_MaxX25Address + 1 ] AS BYTE
	MEMBER DIM szX25Facilities[ RAS_MaxFacilities + 1 ] AS BYTE
	MEMBER DIM szX25UserData[ RAS_MaxUserData + 1 ] AS BYTE
	MEMBER dwChannels AS DWORD
	MEMBER dwReserved1 AS DWORD
	MEMBER dwReserved2 AS DWORD
	MEMBER dwSubEntries AS DWORD
	MEMBER dwDialMode AS DWORD
	MEMBER dwDialExtraPercent AS DWORD
	MEMBER dwDialExtraSampleSeconds AS DWORD
	MEMBER dwHangUpExtraPercent AS DWORD
	MEMBER dwHangUpExtraSampleSeconds AS DWORD
	MEMBER dwIdleDisconnectSeconds AS DWORD


	//
	// Entry Type
	//
	MEMBER dwType AS DWORD

	//
	// Encryption type
	//
	MEMBER dwEncryptionType AS DWORD

	//
	// CustomAuthKey to be used for EAP
	//
	MEMBER dwCustomAuthKey AS DWORD       
	//
	// Guid of the connection
	//
	MEMBER guidId IS _WINGUID
	//
	// Custom Dial Dll
	//
	MEMBER DIM szCustomDialDll[MAX_PATH] AS BYTE
	//
	// DwVpnStrategy
	//
	MEMBER dwVpnStrategy AS DWORD
	//
	// More RASEO_* options
	//
	MEMBER dwfOptions2 AS DWORD		
	//
	// For future use
	//
	MEMBER dwfOptions3 AS DWORD
	MEMBER DIM szDnsSuffix[RAS_MaxDnsSuffix] AS BYTE
	MEMBER dwTcpWindowSize AS DWORD
	MEMBER DIM szPrerequisitePbk[MAX_PATH] AS BYTE
	MEMBER DIM szPrerequisiteEntry[RAS_MaxEntryName + 1] AS BYTE
	MEMBER dwRedialCount AS DWORD
	MEMBER dwRedialPause  AS DWORD

VOSTRUCT _WINRASSUBENTRY  // RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER dwfFlags AS DWORD
	//
	// Device
	//
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE
	//
	// Phone numbers
	//
	MEMBER DIM szLocalPhoneNumber[ RAS_MaxPhoneNumber + 1 ] AS BYTE
	MEMBER dwAlternateOffset AS DWORD



	/* Ras{Get,Set}Credentials structure.  These calls
	** supercede Ras{Get,Set}EntryDialParams.
	*/

VOSTRUCT _winRASCREDENTIALS ALIGN 2
	MEMBER dwSize AS DWORD
	MEMBER dwMask AS DWORD
	MEMBER DIM szUserName[ UNLEN + 1 ] AS BYTE
	MEMBER DIM szPassword[ PWLEN + 1 ] AS BYTE
	MEMBER DIM szDomain[ DNLEN + 1 ] AS BYTE


VOSTRUCT _WINRASAUTODIALENTRY 
	MEMBER dwSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER dwDialingLocation AS DWORD
	MEMBER DIM zEntry[ RAS_MaxEntryName + 1] AS BYTE

	/* AutoDial control parameter values for
	** Ras{Get,Set}AutodialParam.
	*/

VOSTRUCT _WINRASEAPUSERIDENTITY ALIGN 2
	MEMBER DIM szUserName[ UNLEN + 1 ] AS BYTE
	MEMBER dwSizeofEapInfo AS DWORD
	MEMBER DIM pbEapInfo[ 1 ] AS BYTE

	/* RasGetEapUserIdentity bit flags.
	** These have the same values as the RAS_EAP_FLAG_ flags in raseapif.h
	*/
VOSTRUCT _WINRASCOMMSETTINGS ALIGN 2
	MEMBER dwSize 	AS DWORD
	MEMBER bParity AS BYTE
	MEMBER bStop	 AS BYTE
	MEMBER bByteSize AS BYTE
	MEMBER bAlign	 AS BYTE      
	
VOSTRUCT RASCUSTOMSCRIPTEXTENSIONS ALIGN 2
	MEMBER dwSize 	AS DWORD
	MEMBER pfnRasSetCommSettings AS PTR
	
VOSTRUCT _winSECURITY_MESSAGE  // RvdH 070411 removed alignment 
	MEMBER dwMsgId AS DWORD

	MEMBER hPort AS DWORD

	MEMBER dwError AS DWORD



	MEMBER DIM UserName[UNLEN+1] AS BYTE


	MEMBER DIM Domain[DNLEN+1] AS BYTE





VOSTRUCT _winRAS_SECURITY_INFO // RvdH 070411 removed alignment 
	MEMBER LastError AS DWORD
	MEMBER BytesReceived AS DWORD
	MEMBER DIM DeviceName[RASSAPI_MAX_DEVICE_NAME+1] AS DWORD



VOSTRUCT _winRASNOUSER     // RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER dwTimeoutMs AS DWORD
	MEMBER DIM szUserName[ UNLEN + 1 ] AS BYTE
	MEMBER DIM szPassword[ PWLEN + 1 ] AS BYTE
	MEMBER DIM szDomain[ DNLEN + 1 ] AS BYTE





VOSTRUCT _winRASENTRYDLG ALIGN 4
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS DWORD
	MEMBER yDlg AS DWORD
	MEMBER DIM szEntry[ RAS_MaxEntryName + 1 ] AS BYTE
	MEMBER dwError AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD


VOSTRUCT String_win ALIGN 1
	MEMBER	Length  AS DWORD
	MEMBER	Data  AS PSZ


VOSTRUCT _winRAS_PARAMETERS  // RvdH 070411 removed alignment 

	MEMBER DIM P_Key[RASSAPI_MAX_PARAM_KEY_SIZE]  AS BYTE
	MEMBER P_Type  AS DWORD
	MEMBER P_Attributes AS BYTE
	MEMBER P_Value  IS u_winRAS_PARAMS_VALUE



VOSTRUCT  _winRAS_USER_0   // RvdH 070411 removed alignment 
	MEMBER bfPrivilege AS BYTE
	MEMBER DIM szPhoneNumber[ RASSAPI_MAX_PHONENUMBER_SIZE + 1] AS WORD	//RvdH 070412 changed from BYTE to WORD


VOSTRUCT _winRAS_PORT_0   // RvdH 070411 removed alignment                      
	//RvdH 070412 changed DIM members from BYTE to WORD
	MEMBER DIM wszPortName[RASSAPI_MAX_PORT_NAME] AS WORD	
	MEMBER DIM wszDeviceType[RASSAPI_MAX_DEVICETYPE_NAME] AS WORD	
	MEMBER DIM wszDeviceName[RASSAPI_MAX_DEVICE_NAME] AS WORD	
	MEMBER DIM wszMediaName[RASSAPI_MAX_MEDIA_NAME] AS WORD	
	MEMBER MediaId AS DWORD
	MEMBER Flags AS DWORD
	MEMBER DIM wszUserName[UNLEN + 1] AS WORD	   		
	MEMBER DIM wszComputer[NETBIOS_NAME_LEN] AS WORD		
	MEMBER dwStartSessionTime AS DWORD
	MEMBER DIM wszLogonDomain[DNLEN + 1] AS WORD	
	MEMBER fAdvancedServer AS LOGIC



VOSTRUCT _winRAS_PPP_NBFCP_RESULT  // RvdH 070411 removed alignment
	MEMBER dwError AS DWORD
	MEMBER dwNetBiosError AS DWORD
	MEMBER DIM szName[ NETBIOS_NAME_LEN + 1 ] AS BYTE
	MEMBER DIM wszWksta[ NETBIOS_NAME_LEN + 1 ] AS WORD //RvdH 070412 changed from BYTE to WORD

VOSTRUCT _winRAS_PPP_IPCP_RESULT  // RvdH 070411 removed alignmen
	MEMBER dwError AS DWORD
	MEMBER DIM wszAddress[ RAS_IPADDRESSLEN + 1 ] AS WORD //RvdH 070412 changed from BYTE to WORD

VOSTRUCT _winRAS_PPP_IPXCP_RESULT  // RvdH 070411 removed alignmen
	MEMBER dwError AS DWORD
	MEMBER DIM wszAddress[ RAS_IPXADDRESSLEN + 1 ] AS WORD //RvdH 070412 changed from BYTE to WORD

VOSTRUCT _winRAS_PPP_ATCP_RESULT // RvdH 070411 removed alignmen
	MEMBER dwError AS DWORD
	MEMBER DIM wszAddress[ RAS_ATADDRESSLEN + 1 ] AS WORD //RvdH 070412 changed from BYTE to WORD

VOSTRUCT _winRAS_PPP_PROJECTION_RESULT 
	MEMBER nbf IS _winRAS_PPP_NBFCP_RESULT
	MEMBER ip  IS _winRAS_PPP_IPCP_RESULT
	MEMBER ipx IS _winRAS_PPP_IPXCP_RESULT
	MEMBER at  IS _winRAS_PPP_ATCP_RESULT

VOSTRUCT _winRAS_PORT_1 ALIGN 1
	MEMBER rasport0 IS _winRAS_PORT_0
	MEMBER LineCondition AS DWORD
	MEMBER HardwareCondition AS DWORD
	MEMBER LineSpeed AS DWORD
	MEMBER NumStatistics AS	WORD
	MEMBER NumMediaParms AS WORD
	MEMBER SizeMediaParms AS DWORD
	MEMBER ProjResult IS _winRAS_PPP_PROJECTION_RESULT


VOSTRUCT _winRAS_PORT_STATISTICS ALIGN 1
	MEMBER dwBytesXmited AS DWORD
	MEMBER dwBytesRcved AS DWORD
	MEMBER dwFramesXmited AS DWORD
	MEMBER dwFramesRcved AS DWORD
	MEMBER dwCrcErr AS DWORD
	MEMBER dwTimeoutErr AS DWORD
	MEMBER dwAlignmentErr AS DWORD
	MEMBER dwHardwareOverrunErr AS DWORD
	MEMBER dwFramingErr AS DWORD
	MEMBER dwBufferOverrunErr AS DWORD
	MEMBER dwBytesXmitedUncompressed AS DWORD
	MEMBER dwBytesRcvedUncompressed AS DWORD
	MEMBER dwBytesXmitedCompressed AS DWORD
	MEMBER dwBytesRcvedCompressed AS DWORD  
	// RvdH 070411 added
	// the following are the port statistics
	MEMBER dwPortBytesXmited AS DWORD
	MEMBER dwPortBytesRcved AS DWORD
	MEMBER dwPortFramesXmited AS DWORD
	MEMBER dwPortFramesRcved AS DWORD
	MEMBER dwPortCrcErr AS DWORD
	MEMBER dwPortTimeoutErr AS DWORD
	MEMBER dwPortAlignmentErr AS DWORD
	MEMBER dwPortHardwareOverrunErr AS DWORD
	MEMBER dwPortFramingErr AS DWORD
	MEMBER dwPortBufferOverrunErr AS DWORD
	MEMBER dwPortBytesXmitedUncompressed AS DWORD
	MEMBER dwPortBytesRcvedUncompressed AS DWORD
	MEMBER dwPortBytesXmitedCompressed AS DWORD
	MEMBER dwPortBytesRcvedCompressed AS DWORD


VOSTRUCT _winRAS_SERVER_0 ALIGN 1
	MEMBER TotalPorts AS WORD
	MEMBER PortsInUse AS WORD
	MEMBER RasVersion AS DWORD



VOSTRUCT _WINRAS_STATS
	MEMBER dwSize AS DWORD
	MEMBER dwBytesXmited AS DWORD
	MEMBER dwBytesRcved AS DWORD
	MEMBER dwFramesXmited AS DWORD
	MEMBER dwFramesRcved AS DWORD
	MEMBER dwCrcErr AS DWORD
	MEMBER dwTimeoutErr AS DWORD
	MEMBER dwAlignmentErr AS DWORD
	MEMBER dwHardwareOverrunErr AS DWORD
	MEMBER dwFramingErr AS DWORD
	MEMBER dwBufferOverrunErr AS DWORD
	MEMBER dwCompressionRatioIn AS DWORD
	MEMBER dwCompressionRatioOut AS DWORD
	MEMBER dwBps AS DWORD
	MEMBER dwConnectDuration AS DWORD

VOSTRUCT _winRASADPARAMS // RvdH 070411 removed alignment  
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONGINT
	MEMBER yDlg AS LONGINT



	/* AutoDial DLL function parameter block 'dwFlags.'
	*/
VOSTRUCT _winRASPBDLG 		// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONGINT
	MEMBER yDlg AS LONGINT
	MEMBER dwCallbackId AS DWORD
	MEMBER pCallback AS PTR
	MEMBER dwError	AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD





VOSTRUCT _winRASDIALDLG 		// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONGINT
	MEMBER yDlg AS LONGINT
	MEMBER dwSubEntry AS DWORD
	MEMBER dwError AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD





VOSTRUCT _winRASMONITORDLG 	// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER dwStartPage AS DWORD
	MEMBER xDlg AS LONGINT
	MEMBER yDlg AS LONGINT
	MEMBER dwError AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD




_DLL FUNC RasDial( P1 AS _winRASDIALEXTENSIONS, P2 AS PSZ, P3 AS _winRASDIALPARAMS, P4 AS DWORD, P5 AS PTR,;
		P6 AS PTR ) AS DWORD PASCAL:RASAPI32.RasDialA


_DLL FUNC RasEnumConnections( P1 AS _winRASCONN, P2 AS DWORD PTR, P3 AS DWORD PTR);
		AS DWORD PASCAL:RASAPI32.RasEnumConnectionsA


_DLL FUNC  RasEnumEntries( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRYNAME, P4 AS DWORD  PTR, P5 AS DWORD PTR );
		AS DWORD PASCAL:RASAPI32.RasEnumEntriesA


_DLL FUNC RasGetConnectStatus( P1 AS PTR, P2 AS _winRASCONNSTATUS ) AS DWORD PASCAL:RASAPI32.RasGetConnectStatusA


_DLL FUNC RasGetErrorString( P1 AS DWORD, P2 AS PSZ, P3 AS DWORD ) AS DWORD PASCAL:RASAPI32.RasGetErrorStringA


_DLL FUNC RasHangUp( P1 AS PTR) AS DWORD PASCAL:RASAPI32.RasHangUpA


_DLL FUNC RasGetProjectionInfo(P1 AS PTR, P2 AS DWORD, P3 AS PTR, P4 AS DWORD PTR);
		AS DWORD PASCAL:RASAPI32.RasGetProjectionInfoA


_DLL FUNC RasCreatePhonebookEntry( P1 AS PTR , P2 AS PSZ ) AS DWORD PASCAL:RASAPI32.RasCreatePhonebookEntryA


_DLL FUNC RasEditPhonebookEntry( P1 AS PTR, P2 AS PSZ, P3 AS PSZ ) AS DWORD PASCAL:RASAPI32.RasEditPhonebookEntryA


_DLL FUNC RasSetEntryDialParams(P1 AS PSZ, P2 AS _winRASDIALPARAMS, P3 AS LOGIC);
		AS DWORD PASCAL:RASAPI32.RasSetEntryDialParamsA


_DLL FUNC RasGetEntryDialParams( P1 AS PSZ, P2 AS _winRASDIALPARAMS, P3 AS LOGIC PTR );
		AS DWORD PASCAL:RASAPI32.RasGetEntryDialParamsA


_DLL FUNC RasEnumDevices( P1 AS _winRASDEVINFO, P2 AS DWORD PTR, P3 AS DWORD PTR );
		AS DWORD PASCAL:RASAPI32.RasEnumDevicesA


_DLL FUNC RasGetCountryInfo( P1 AS _winRASCTRYINFO, P2 AS DWORD PTR) AS DWORD PASCAL:RASAPI32.RasGetCountryInfoA


_DLL FUNC RasGetEntryProperties( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRY, P4 AS DWORD PTR, P5 AS BYTE PTR, P6 AS DWORD PTR );
		AS DWORD PASCAL:RASAPI32.RasGetEntryPropertiesA


_DLL FUNC RasSetEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRY, P4 AS DWORD, P5 AS BYTE PTR, P6 AS DWORD );
		AS DWORD PASCAL:RASAPI32.RasSetEntryPropertiesA

_DLL FUNC RasRenameEntry(P1 AS PSZ, P2 AS PSZ, P3 AS PSZ ) AS DWORD PASCAL:RASAPI32.RasRenameEntryA


_DLL FUNC RasDeleteEntry(P1 AS PSZ, P2 AS PSZ ) AS DWORD PASCAL:RASAPI32.RasDeleteEntry


_DLL FUNC RasValidateEntryName(P1 AS PSZ, P2 AS PSZ) AS DWORD PASCAL:RASAPI32.RasValidateEntryNameA


_DLL FUNC RasGetSubEntryHandle(P1 AS PTR, P2 AS DWORD, P3 AS PTR ) AS DWORD PASCAL:RASAPI32.RasGetSubEntryHandleA


_DLL FUNC RasGetCredentials(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASCREDENTIALS);
		AS DWORD PASCAL:RASAPI32.RasGetCredentialsA


_DLL FUNC RasSetCredentials(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASCREDENTIALS, P4 AS LOGIC );
		AS DWORD PASCAL:RASAPI32.RasSetCredentialsA


_DLL FUNC RasConnectionNotification(P1 AS PTR, P2 AS PTR, P3 AS DWORD) AS DWORD PASCAL:RASAPI.RasConnectionNotification


_DLL FUNC RasGetSubEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS DWORD,;
		P4 AS _winRASSUBENTRY, P5 AS DWORD PTR , P6 AS BYTE PTR, P7 AS DWORD PTR);
		AS DWORD PASCAL:RASAPI32.RasGetSubEntryPropertiesA


_DLL FUNC RasSetSubEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS DWORD,;
		P4 AS _winRASSUBENTRY, P5 AS DWORD, P6 AS BYTE PTR,;
		P7 AS DWORD ) AS DWORD PASCAL:RASAPI32.RasSetSubEntryProperties


_DLL FUNC RasGetAutodialAddress(P1 AS PSZ, P2 AS DWORD PTR, P3 AS _winRASAUTODIALENTRY,;
		P4 AS DWORD PTR, P5 AS DWORD PTR );
		AS DWORD PASCAL:RASAPI32.RasGetAutodialAddressA


_DLL FUNC RasSetAutodialAddress( P1 AS PSZ, P2 AS DWORD, P3 AS _winRASAUTODIALENTRY,;
		P4 AS DWORD, P5 AS DWORD );
		AS DWORD PASCAL:RASAPI.RasSetAutodialAddressA


_DLL FUNC RasEnumAutodialAddresses( P1 AS PTR, P2 AS DWORD PTR, P3 AS DWORD PTR);
		AS DWORD PASCAL:RASAPI32.RasEnumAutodialAddressesA


_DLL FUNC  RasGetAutodialEnable(P1 AS  DWORD, P2 AS LOGIC PTR ) AS DWORD PASCAL:RASAPI32.RasGetAutodialEnableA


_DLL FUNC RasSetAutodialEnable(P1 AS  DWORD, P2 AS LOGIC ) AS DWORD PASCAL:RASAPI32.RasSetAutodialEnableA


_DLL FUNC RasGetAutodialParam( P1 AS DWORD, P2 AS PTR, P3 AS DWORD PTR);
		AS DWORD PASCAL:RASAPI32.RasGetAutodialParamA


_DLL FUNC RasSetAutodialParam(P1 AS DWORD, P2 AS PTR, P3 AS DWORD ) AS DWORD PASCAL:RASAPI32.RasSetAutodialParamA


	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASERROR.PRG




_DLL FUNC RasSecurityDialogSend(HPort AS DWORD, pBuffer AS BYTE PTR, BufferLength AS WORD );
		AS DWORD PASCAL:RASMAN.RasSecurityDialogSend


_DLL FUNC RasSecurityDialogReceive(hPort AS DWORD , pBuffer AS BYTE PTR,pBufferLength AS WORD PTR,;
		Timeout AS DWORD, hEvent AS PTR);
		AS DWORD PASCAL:RASMAN.RasSecurityDialogReceive


_DLL FUNC RasSecurityDialogGetInfo(hPort AS DWORD, pBuffer AS _winRAS_SECURITY_INFO);
		AS DWORD PASCAL:RASMAN.RasSecurityDialogGetInfo

	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASDLG.PRG









_DLL FUNC RasPhonebookDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpInfo AS _winRASPBDLG );
		AS LOGIC PASCAL:RASDLG.RasPhonebookDlgA


_DLL FUNC RasEntryDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpInfo AS _winRASENTRYDLG);
		AS LOGIC PASCAL:RASDLG.RasEntryDlg


	// RvdH 050710 Next line was missing the 'A' for ANsi Version
_DLL FUNC RasDialDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpszPhoneNumber AS PSZ,;
		lpInfo  AS _winRASDIALDLG) AS LOGIC PASCAL:RASDLG.RasDialDlgA


_DLL FUNC RasMonitorDlg(lpszDeviceName AS PSZ, lpInfo AS _winRASMONITORDLG );
		AS LOGIC PASCAL:RASDLG.RasMonitorDlg


	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASSAPI.PRG















UNION u_winRAS_PARAMS_VALUE

	MEMBER	Number  AS DWORD
	MEMBER	_STRING IS String_win


_DLL FUNC RasAdminServerGetInfo(lpszServer AS PSZ, pRasServer0 AS _winRAS_SERVER_0);
		AS DWORD PASCAL:RASSAPI.RasAdminServerGetInfo

_DLL FUNC RasAdminGetUserAccountServer(lpszDomain AS PSZ, lpszServer AS PSZ,;
		lpszUserAccountServer  AS PSZ  ) AS DWORD PASCAL:RASSAPI.RasAdminGetUserAccountServer

_DLL FUNC RasAdminUserGetInfo(lpszUserAccountServer AS PSZ, lpszUser AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD PASCAL:RASSAPI.RasAdminUserGetInfo

_DLL FUNC RasAdminUserSetInfo(lpszUserAccountServer AS PSZ, lpszUser AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD PASCAL:RASSAPI.RasAdminUserSetInfo

_DLL FUNC RasAdminPortEnum( lpszServer AS PSZ, ppRasPort0 AS _winRAS_PORT_0, pcEntriesRead AS WORD PTR);
		AS DWORD PASCAL:RASSAPI.RasAdminPortEnum

_DLL FUNC RasAdminPortGetInfo(lpszServer AS PSZ, lpszPort AS PSZ,  pRasPort1 AS _winRAS_PORT_1,;
		pRasStats AS _winRAS_PORT_STATISTICS, ppRasParams AS PTR);
		AS DWORD PASCAL:RASSAPI.RasAdminPortGetInfo

_DLL FUNC RasAdminPortClearStatistics(lpszServer AS PSZ, lpszPort AS PSZ);
		AS DWORD PASCAL:RASSAPI.RasAdminPortClearStatistics

_DLL FUNC RasAdminPortDisconnect(lpszServer AS PSZ, lpszPort AS PSZ);
		AS DWORD PASCAL:RASSAPI.RasAdminPortDisconnect

_DLL FUNC RasAdminFreeBuffer(Pointer AS PTR) AS DWORD PASCAL:RASSAPI.RasAdminFreeBuffer

_DLL FUNC RasAdminGetErrorString(ResourceId AS DWORD, lpszString AS PSZ, InBufSize AS DWORD);
		AS DWORD PASCAL:RASSAPI.RasAdminGetErrorString






_DLL FUNC RasAdminGetUserParms(lpszParms AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD PASCAL:RASSAPI.RasAdminGetUserParms
_DLL FUNC  RasAdminSetUserParms(lpszParms AS PSZ, cchNewParms AS DWORD, pRasUser0 AS _winRAS_USER_0);
		AS DWORD PASCAL:RASSAPI.RasAdminSetUserParms


FUNCTION RASADFUNC( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASADPARAMS, P4 AS DWORD PTR ) AS LOGIC
	RETURN FALSE

	/* A RAS phone book multilinked sub-entry.
	*/
FUNCTION RASSECURITYPROC() AS DWORD
	RETURN 0











FUNCTION RASPBDLGFUNC(P1 AS  DWORD, P2 AS DWORD, P3 AS PSZ, P4 AS PTR ) AS VOID STRICT
	RETURN



#region defines
DEFINE RAS_MaxDeviceType     := 16
DEFINE RAS_MaxPhoneNumber    := 128
DEFINE RAS_MaxIpAddress      := 15
DEFINE RAS_MaxIpxAddress     := 21
DEFINE RAS_MaxEntryName      := 256
DEFINE RAS_MaxDeviceName     := 128
DEFINE RAS_MaxCallbackNumber := RAS_MaxPhoneNumber
DEFINE RAS_MaxAreaCode       := 10
DEFINE RAS_MaxPadType        := 32
DEFINE RAS_MaxX25Address     := 200
DEFINE RAS_MaxFacilities     := 200
DEFINE RAS_MaxUserData       := 200
DEFINE RAS_MaxReplyMessage   := 1024
DEFINE RAS_MaxDnsSuffix      := 256
DEFINE RASCF_AllUsers		:= 0x00000001
DEFINE RASCF_GlobalCreds	:= 0x00000002
DEFINE RASCS_PAUSED := 0x1000
DEFINE RASCS_DONE   := 0x2000
DEFINE RASCS_OpenPort := 0
DEFINE RASCS_PortOpened:= 1
DEFINE RASCS_ConnectDevice := 2
DEFINE RASCS_DeviceConnected :=3
DEFINE RASCS_AllDevicesConnected :=4
DEFINE RASCS_Authenticate :=5
DEFINE RASCS_AuthNotify :=6
DEFINE RASCS_AuthRetry := 7
DEFINE RASCS_AuthCallback := 8
DEFINE RASCS_AuthChangePassword := 9
DEFINE RASCS_AuthProject := 10
DEFINE RASCS_AuthLinkSpeed := 11
DEFINE RASCS_AuthAck := 12
DEFINE RASCS_ReAuthenticate := 13
DEFINE RASCS_Authenticated := 14
DEFINE RASCS_PrepareForCallback := 15
DEFINE RASCS_WaitForModemReset := 16
DEFINE RASCS_WaitForCallback := 17
DEFINE RASCS_Projected := 18
DEFINE RASCS_StartAuthentication := 19
DEFINE RASCS_CallbackComplete := 20
DEFINE RASCS_LogonNetwork := 21
DEFINE RASCS_SubEntryConnected := 22
DEFINE RASCS_SubEntryDisconnected := 23
DEFINE RASCS_Interactive := RASCS_PAUSED
DEFINE RASCS_RetryAuthentication := 24
DEFINE RASCS_CallbackSetByCaller := 25
DEFINE RASCS_PasswordExpired := 26
DEFINE RASCS_InvokeEapUI := 27
DEFINE RASCS_Connected :=RASCS_DONE
DEFINE RASCS_Disconnected := RASCS_DONE+1
DEFINE RDEOPT_UsePrefixSuffix           := 0x00000001
DEFINE RDEOPT_PausedStates              := 0x00000002
DEFINE RDEOPT_IgnoreModemSpeaker        := 0x00000004
DEFINE RDEOPT_SetModemSpeaker           := 0x00000008
DEFINE RDEOPT_IgnoreSoftwareCompression := 0x00000010
DEFINE RDEOPT_SetSoftwareCompression    := 0x00000020
DEFINE RDEOPT_DisableConnectedUI        := 0x00000040
DEFINE RDEOPT_DisableReconnectUI        := 0x00000080
DEFINE RDEOPT_DisableReconnect          := 0x00000100
DEFINE RDEOPT_NoUser                    := 0x00000200
DEFINE RDEOPT_PauseOnScript            :=  0x00000400
DEFINE RDEOPT_Router                   :=  0x00000800
DEFINE RDEOPT_CustomDial                := 0x00001000
DEFINE RDEOPT_UseCustomScripting        := 0x00002000
	//
	// This flag when set in the RASENTRYNAME structure
	// indicates that the phonebook to which this entry
	// belongs is a system phonebook.
	//
DEFINE REN_User                         := 0x00000000
DEFINE REN_AllUsers                     := 0x00000001
DEFINE RASP_Amb := 0x10000
DEFINE RASP_PppNbf := 0x803F
DEFINE RASP_PppIpx := 0x802B
DEFINE RASP_PppIp := 0x8021
DEFINE RASP_PppCcp := 0x80FD
DEFINE RASP_PppLcp := 0xC021
DEFINE RASP_Slip := 0x20000
DEFINE RASLCPAP_PAP         :=  0xC023
DEFINE RASLCPAP_SPAP        :=  0xC027
DEFINE RASLCPAP_CHAP        :=  0xC223
DEFINE RASLCPAP_EAP         :=  0xC227
	/* RASPPPLCP 'dwAuthenticatonData' values.
	*/
DEFINE RASLCPAD_CHAP_MD5    :=  0x05
DEFINE RASLCPAD_CHAP_MS     :=  0x80
DEFINE RASLCPAD_CHAP_MSV2   :=  0x81
	/* RASPPPLCP 'dwOptions' and 'dwServerOptions' flags.
	*/
DEFINE RASLCPO_PFC          := 0x00000001
DEFINE RASLCPO_ACFC         := 0x00000002
DEFINE RASLCPO_SSHF         := 0x00000004
DEFINE RASLCPO_DES_56       := 0x00000008
DEFINE RASLCPO_3_DES        := 0x00000010
DEFINE RASDIALEVENT   := "RasDialEvent"
DEFINE WM_RASDIALEVENT := 0xCCCD
DEFINE ET_None        := 0  // No encryption
DEFINE ET_Require     := 1  // Require Encryption
DEFINE ET_RequireMax  := 2  // Require max encryption
DEFINE ET_Optional    := 3  // Do encryption if possible. None Ok.
DEFINE VS_Default		:= 0   // default (PPTP for now)
DEFINE VS_PptpOnly	:=     1	// Only PPTP is attempted.
DEFINE VS_PptpFirst	:= 2   // PPTP is tried first.
DEFINE VS_L2tpOnly 	:= 3	// Only L2TP is attempted.
DEFINE VS_L2tpFirst	:= 4	// L2TP is tried first.
DEFINE RASEO_UseCountryAndAreaCodes    := 0x00000001
DEFINE RASEO_SpecificIpAddr            := 0x00000002
DEFINE RASEO_SpecificNameServers       := 0x00000004
DEFINE RASEO_IpHeaderCompression       := 0x00000008
DEFINE RASEO_RemoteDefaultGateway      := 0x00000010
DEFINE RASEO_DisableLcpExtensions      := 0x00000020
DEFINE RASEO_TerminalBeforeDial        := 0x00000040
DEFINE RASEO_TerminalAfterDial         := 0x00000080
DEFINE RASEO_ModemLights               := 0x00000100
DEFINE RASEO_SwCompression             := 0x00000200
DEFINE RASEO_RequireEncryptedPw        := 0x00000400
DEFINE RASEO_RequireMsEncryptedPw      := 0x00000800
DEFINE RASEO_RequireDataEncryption     := 0x00001000
DEFINE RASEO_NetworkLogon              := 0x00002000
DEFINE RASEO_UseLogonCredentials       := 0x00004000
DEFINE RASEO_PromoteAlternates         := 0x00008000
DEFINE RASEO_SecureLocalFiles          := 0x00010000
DEFINE RASEO_RequireEAP               := 0x00020000
DEFINE RASEO_RequirePAP               := 0x00040000
DEFINE RASEO_RequireSPAP              := 0x00080000
DEFINE RASEO_Custom                   := 0x00100000
DEFINE RASEO_PreviewPhoneNumber       := 0x00200000
DEFINE RASEO_SharedPhoneNumbers       := 0x00800000
DEFINE RASEO_PreviewUserPw            := 0x01000000
DEFINE RASEO_PreviewDomain            := 0x02000000
DEFINE RASEO_ShowDialingProgress      := 0x04000000
DEFINE RASEO_RequireCHAP              := 0x08000000
DEFINE RASEO_RequireMsCHAP            := 0x10000000
DEFINE RASEO_RequireMsCHAP2           := 0x20000000
DEFINE RASEO_RequireW95MSCHAP         := 0x40000000
DEFINE RASEO_CustomScript             := 0x80000000
	//
	// RASENTRY dwfOptions2 bit flags
	//
DEFINE RASEO2_SecureFileAndPrint      :=0x00000001
DEFINE RASEO2_SecureClientForMSNet    :=0x00000002
DEFINE RASEO2_DontNegotiateMultilink  :=0x00000004
DEFINE RASEO2_DontUseRasCredentials   :=0x00000008
DEFINE RASEO2_UsePreSharedKey         :=0x00000010
DEFINE RASEO2_Internet                :=0x00000020
DEFINE RASEO2_DisableNbtOverIP        :=0x00000040
DEFINE RASEO2_UseGlobalDeviceSettings :=0x00000080
DEFINE RASEO2_ReconnectIfDropped      :=0x00000100
DEFINE RASEO2_SharePhoneNumbers       :=0x00000200
	/* RASENTRY 'dwProtocols' bit flags.
	*/
DEFINE RASNP_NetBEUI                  :=  0x00000001
DEFINE RASNP_Ipx                      :=  0x00000002
DEFINE RASNP_Ip                       :=  0x00000004
	/* RASENTRY 'dwFramingProtocols' bit flags.
	*/
DEFINE RASFP_Ppp                       := 0x00000001
DEFINE RASFP_Slip                      := 0x00000002
DEFINE RASFP_Ras                       := 0x00000004
	/* RASENTRY 'szDeviceType' default strings.
	*/
DEFINE RASDT_Modem                     := "modem"
DEFINE RASDT_Isdn                      := "isdn"
DEFINE RASDT_X25                       := "x25"
DEFINE RASDT_Vpn                      :="vpn"
DEFINE RASDT_Pad                      :="pad"
DEFINE RASDT_Generic                  :="GENERIC"
DEFINE RASDT_Serial        				 :="SERIAL"
DEFINE RASDT_FrameRelay               :="FRAMERELAY"
DEFINE RASDT_Atm                      :="ATM"
DEFINE RASDT_Sonet                    :="SONET"
DEFINE RASDT_SW56                     :="SW56"
DEFINE RASDT_Irda                     :="IRDA"
DEFINE RASDT_Parallel                 :="PARALLEL"
DEFINE RASDT_PPPoE                    :="PPPoE"
	// The entry type used to determine which UI properties
	// are to be presented to user.  This generally corresponds
	// to a Connections "add" wizard selection.
	//
DEFINE RASET_Phone     := 1  // Phone lines: modem, ISDN, X.25, etc
DEFINE RASET_Vpn       := 2  // Virtual private network
DEFINE RASET_Direct    := 3  // Direct connect: serial, parallel
DEFINE RASET_Internet  := 4  // BaseCamp internet
DEFINE RASET_Broadband := 5  // Broadband
	/* Flags for RasConnectionNotification().
	*/
DEFINE RASCN_Connection        := 0x00000001
DEFINE RASCN_Disconnection     := 0x00000002
DEFINE RASCN_BandwidthAdded    := 0x00000004
DEFINE RASCN_BandwidthRemoved  := 0x00000008
	/* RASENTRY 'dwDialMode' values.
	*/
DEFINE RASEDM_DialAll                  := 1
DEFINE RASEDM_DialAsNeeded             := 2
	/* RASENTRY 'dwIdleDisconnectSeconds' constants.
	*/
DEFINE RASIDS_Disabled                 := 0xffffffff
DEFINE RASIDS_UseGlobalValue           := 0
DEFINE RASADFLG_PositionDlg            := 0x00000001
	/* Prototype AutoDial DLL function.
	*/
DEFINE RASCM_UserName       := 0x00000001
DEFINE RASCM_Password       := 0x00000002
DEFINE RASCM_Domain         := 0x00000004
DEFINE RASCM_DefaultCreds      :=0x00000008
DEFINE RASCM_PreSharedKey      :=0x00000010
DEFINE RASCM_ServerPreSharedKey:=0x00000020
DEFINE RASCM_DDMPreSharedKey   :=0x00000040
	/* AutoDial address properties.
	*/
DEFINE RASADP_DisableConnectionQuery           := 0
DEFINE RASADP_LoginSessionDisable              := 1
DEFINE RASADP_SavedAddressesLimit              := 2
DEFINE RASADP_FailedConnectionTimeout          := 3
DEFINE RASADP_ConnectionQueryTimeout           := 4
DEFINE RASEAPF_NonInteractive         := 0x00000002
DEFINE RASEAPF_Logon                  := 0x00000004
DEFINE RASEAPF_Preview                := 0x00000008
DEFINE RASBASE := 600
DEFINE SUCCESS := 0
DEFINE PENDING                              := (RASBASE+0)
DEFINE ERROR_INVALID_PORT_HANDLE            := (RASBASE+1)
DEFINE ERROR_PORT_ALREADY_OPEN              := (RASBASE+2)
DEFINE ERROR_BUFFER_TOO_SMALL               := (RASBASE+3)
DEFINE ERROR_WRONG_INFO_SPECIFIED           := (RASBASE+4)
DEFINE ERROR_CANNOT_SET_PORT_INFO           := (RASBASE+5)
DEFINE ERROR_PORT_NOT_CONNECTED             := (RASBASE+6)
DEFINE ERROR_EVENT_INVALID                  := (RASBASE+7)
DEFINE ERROR_DEVICE_DOES_NOT_EXIST          := (RASBASE+8)
DEFINE ERROR_DEVICETYPE_DOES_NOT_EXIST      := (RASBASE+9)
DEFINE ERROR_BUFFER_INVALID                 := (RASBASE+10)
DEFINE ERROR_ROUTE_NOT_AVAILABLE            := (RASBASE+11)
DEFINE ERROR_ROUTE_NOT_ALLOCATED            := (RASBASE+12)
DEFINE ERROR_INVALID_COMPRESSION_SPECIFIED  := (RASBASE+13)
DEFINE ERROR_OUT_OF_BUFFERS                 := (RASBASE+14)
DEFINE ERROR_PORT_NOT_FOUND                 := (RASBASE+15)
DEFINE ERROR_ASYNC_REQUEST_PENDING          := (RASBASE+16)
DEFINE ERROR_ALREADY_DISCONNECTING          := (RASBASE+17)
DEFINE ERROR_PORT_NOT_OPEN                  := (RASBASE+18)
DEFINE ERROR_PORT_DISCONNECTED              := (RASBASE+19)
DEFINE ERROR_NO_ENDPOINTS                   := (RASBASE+20)
DEFINE ERROR_CANNOT_OPEN_PHONEBOOK          := (RASBASE+21)
DEFINE ERROR_CANNOT_LOAD_PHONEBOOK          := (RASBASE+22)
DEFINE ERROR_CANNOT_FIND_PHONEBOOK_ENTRY    := (RASBASE+23)
DEFINE ERROR_CANNOT_WRITE_PHONEBOOK         := (RASBASE+24)
DEFINE ERROR_CORRUPT_PHONEBOOK              := (RASBASE+25)
DEFINE ERROR_CANNOT_LOAD_STRING             := (RASBASE+26)
DEFINE ERROR_KEY_NOT_FOUND                  := (RASBASE+27)
DEFINE ERROR_DISCONNECTION                  := (RASBASE+28)
DEFINE ERROR_REMOTE_DISCONNECTION           := (RASBASE+29)
DEFINE ERROR_HARDWARE_FAILURE               := (RASBASE+30)
DEFINE ERROR_USER_DISCONNECTION             := (RASBASE+31)
DEFINE ERROR_INVALID_SIZE                   := (RASBASE+32)
DEFINE ERROR_PORT_NOT_AVAILABLE             := (RASBASE+33)
DEFINE ERROR_CANNOT_PROJECT_CLIENT          := (RASBASE+34)
DEFINE ERROR_UNKNOWN                        := (RASBASE+35)
DEFINE ERROR_WRONG_DEVICE_ATTACHED          := (RASBASE+36)
DEFINE ERROR_BAD_STRING                     := (RASBASE+37)
DEFINE ERROR_REQUEST_TIMEOUT                := (RASBASE+38)
DEFINE ERROR_CANNOT_GET_LANA                := (RASBASE+39)
DEFINE ERROR_NETBIOS_ERROR                  := (RASBASE+40)
DEFINE ERROR_SERVER_OUT_OF_RESOURCES        := (RASBASE+41)
DEFINE ERROR_NAME_EXISTS_ON_NET             := (RASBASE+42)
DEFINE ERROR_SERVER_GENERAL_NET_FAILURE     := (RASBASE+43)
DEFINE WARNING_MSG_ALIAS_NOT_ADDED          := (RASBASE+44)
DEFINE ERROR_AUTH_INTERNAL                  := (RASBASE+45)
DEFINE ERROR_RESTRICTED_LOGON_HOURS         := (RASBASE+46)
DEFINE ERROR_ACCT_DISABLED                  := (RASBASE+47)
DEFINE ERROR_PASSWD_EXPIRED                 := (RASBASE+48)
DEFINE ERROR_NO_DIALIN_PERMISSION           := (RASBASE+49)
DEFINE ERROR_SERVER_NOT_RESPONDING          := (RASBASE+50)
DEFINE ERROR_FROM_DEVICE                    := (RASBASE+51)
DEFINE ERROR_UNRECOGNIZED_RESPONSE          := (RASBASE+52)
DEFINE ERROR_MACRO_NOT_FOUND                := (RASBASE+53)
DEFINE ERROR_MACRO_NOT_DEFINED              := (RASBASE+54)
DEFINE ERROR_MESSAGE_MACRO_NOT_FOUND        := (RASBASE+55)
DEFINE ERROR_DEFAULTOFF_MACRO_NOT_FOUND     := (RASBASE+56)
DEFINE ERROR_FILE_COULD_NOT_BE_OPENED       := (RASBASE+57)
DEFINE ERROR_DEVICENAME_TOO_LONG            := (RASBASE+58)
DEFINE ERROR_DEVICENAME_NOT_FOUND           := (RASBASE+59)
DEFINE ERROR_NO_RESPONSES                   := (RASBASE+60)
DEFINE ERROR_NO_COMMAND_FOUND               := (RASBASE+61)
DEFINE ERROR_WRONG_KEY_SPECIFIED            := (RASBASE+62)
DEFINE ERROR_UNKNOWN_DEVICE_TYPE            := (RASBASE+63)
DEFINE ERROR_ALLOCATING_MEMORY              := (RASBASE+64)
DEFINE ERROR_PORT_NOT_CONFIGURED            := (RASBASE+65)
DEFINE ERROR_DEVICE_NOT_READY               := (RASBASE+66)
DEFINE ERROR_READING_INI_FILE               := (RASBASE+67)
DEFINE ERROR_NO_CONNECTION                  := (RASBASE+68)
DEFINE ERROR_BAD_USAGE_IN_INI_FILE          := (RASBASE+69)
DEFINE ERROR_READING_SECTIONNAME            := (RASBASE+70)
DEFINE ERROR_READING_DEVICETYPE             := (RASBASE+71)
DEFINE ERROR_READING_DEVICENAME             := (RASBASE+72)
DEFINE ERROR_READING_USAGE                  := (RASBASE+73)
DEFINE ERROR_READING_MAXCONNECTBPS          := (RASBASE+74)
DEFINE ERROR_READING_MAXCARRIERBPS          := (RASBASE+75)
DEFINE ERROR_LINE_BUSY                      := (RASBASE+76)
DEFINE ERROR_VOICE_ANSWER                   := (RASBASE+77)
DEFINE ERROR_NO_ANSWER                      := (RASBASE+78)
DEFINE ERROR_NO_CARRIER                     := (RASBASE+79)
DEFINE ERROR_NO_DIALTONE                    := (RASBASE+80)
DEFINE ERROR_IN_COMMAND                     := (RASBASE+81)
DEFINE ERROR_WRITING_SECTIONNAME            := (RASBASE+82)
DEFINE ERROR_WRITING_DEVICETYPE             := (RASBASE+83)
DEFINE ERROR_WRITING_DEVICENAME             := (RASBASE+84)
DEFINE ERROR_WRITING_MAXCONNECTBPS          := (RASBASE+85)
DEFINE ERROR_WRITING_MAXCARRIERBPS          := (RASBASE+86)
DEFINE ERROR_WRITING_USAGE                  := (RASBASE+87)
DEFINE ERROR_WRITING_DEFAULTOFF             := (RASBASE+88)
DEFINE ERROR_READING_DEFAULTOFF             := (RASBASE+89)
DEFINE ERROR_EMPTY_INI_FILE                 := (RASBASE+90)
DEFINE ERROR_AUTHENTICATION_FAILURE         := (RASBASE+91)
DEFINE ERROR_PORT_OR_DEVICE                 := (RASBASE+92)
DEFINE ERROR_NOT_BINARY_MACRO               := (RASBASE+93)
DEFINE ERROR_DCB_NOT_FOUND                  := (RASBASE+94)
DEFINE ERROR_STATE_MACHINES_NOT_STARTED     := (RASBASE+95)
DEFINE ERROR_STATE_MACHINES_ALREADY_STARTED := (RASBASE+96)
DEFINE ERROR_PARTIAL_RESPONSE_LOOPING       := (RASBASE+97)
DEFINE ERROR_UNKNOWN_RESPONSE_KEY           := (RASBASE+98)
DEFINE ERROR_RECV_BUF_FULL                  := (RASBASE+99)
DEFINE ERROR_CMD_TOO_LONG                   := (RASBASE+100)
DEFINE ERROR_UNSUPPORTED_BPS                := (RASBASE+101)
DEFINE ERROR_UNEXPECTED_RESPONSE            := (RASBASE+102)
DEFINE ERROR_INTERACTIVE_MODE               := (RASBASE+103)
DEFINE ERROR_BAD_CALLBACK_NUMBER            := (RASBASE+104)
DEFINE ERROR_INVALID_AUTH_STATE             := (RASBASE+105)
DEFINE ERROR_WRITING_INITBPS                := (RASBASE+106)
DEFINE ERROR_X25_DIAGNOSTIC                 := (RASBASE+107)
DEFINE ERROR_ACCT_EXPIRED                   := (RASBASE+108)
DEFINE ERROR_CHANGING_PASSWORD              := (RASBASE+109)
DEFINE ERROR_OVERRUN                        := (RASBASE+110)
DEFINE ERROR_RASMAN_CANNOT_INITIALIZE	    :=  (RASBASE+111)
DEFINE ERROR_BIPLEX_PORT_NOT_AVAILABLE      := (RASBASE+112)
DEFINE ERROR_NO_ACTIVE_ISDN_LINES           := (RASBASE+113)
DEFINE ERROR_NO_ISDN_CHANNELS_AVAILABLE     := (RASBASE+114)
DEFINE ERROR_TOO_MANY_LINE_ERRORS           := (RASBASE+115)
DEFINE ERROR_IP_CONFIGURATION               := (RASBASE+116)
DEFINE ERROR_NO_IP_ADDRESSES                := (RASBASE+117)
DEFINE ERROR_PPP_TIMEOUT                    := (RASBASE+118)
DEFINE ERROR_PPP_REMOTE_TERMINATED          := (RASBASE+119)
DEFINE ERROR_PPP_NO_PROTOCOLS_CONFIGURED    := (RASBASE+120)
DEFINE ERROR_PPP_NO_RESPONSE                := (RASBASE+121)
DEFINE ERROR_PPP_INVALID_PACKET             := (RASBASE+122)
DEFINE ERROR_PHONE_NUMBER_TOO_LONG          := (RASBASE+123)
DEFINE ERROR_IPXCP_NO_DIALOUT_CONFIGURED    := (RASBASE+124)
DEFINE ERROR_IPXCP_NO_DIALIN_CONFIGURED     := (RASBASE+125)
DEFINE ERROR_IPXCP_DIALOUT_ALREADY_ACTIVE   := (RASBASE+126)
DEFINE ERROR_ACCESSING_TCPCFGDLL            := (RASBASE+127)
DEFINE ERROR_NO_IP_RAS_ADAPTER              := (RASBASE+128)
DEFINE ERROR_SLIP_REQUIRES_IP               := (RASBASE+129)
DEFINE ERROR_PROJECTION_NOT_COMPLETE        := (RASBASE+130)
DEFINE ERROR_PROTOCOL_NOT_CONFIGURED        := (RASBASE+131)
DEFINE ERROR_PPP_NOT_CONVERGING             := (RASBASE+132)
DEFINE ERROR_PPP_CP_REJECTED                := (RASBASE+133)
DEFINE ERROR_PPP_LCP_TERMINATED             := (RASBASE+134)
DEFINE ERROR_PPP_REQUIRED_ADDRESS_REJECTED  := (RASBASE+135)
DEFINE ERROR_PPP_NCP_TERMINATED             := (RASBASE+136)
DEFINE ERROR_PPP_LOOPBACK_DETECTED          := (RASBASE+137)
DEFINE ERROR_PPP_NO_ADDRESS_ASSIGNED        := (RASBASE+138)
DEFINE ERROR_CANNOT_USE_LOGON_CREDENTIALS   := (RASBASE+139)
DEFINE ERROR_TAPI_CONFIGURATION             := (RASBASE+140)
DEFINE ERROR_NO_LOCAL_ENCRYPTION            := (RASBASE+141)
DEFINE ERROR_NO_REMOTE_ENCRYPTION           := (RASBASE+142)
DEFINE ERROR_REMOTE_REQUIRES_ENCRYPTION     := (RASBASE+143)
DEFINE ERROR_IPXCP_NET_NUMBER_CONFLICT      := (RASBASE+144)
DEFINE ERROR_INVALID_SMM                    := (RASBASE+145)
DEFINE ERROR_SMM_UNINITIALIZED              := (RASBASE+146)
DEFINE ERROR_NO_MAC_FOR_PORT                := (RASBASE+147)
DEFINE ERROR_SMM_TIMEOUT                    := (RASBASE+148)
DEFINE ERROR_BAD_PHONE_NUMBER               := (RASBASE+149)
DEFINE ERROR_WRONG_MODULE                   := (RASBASE+150)
DEFINE ERROR_INVALID_CALLBACK_NUMBER        := (RASBASE+151)
DEFINE RASBASEEND                           := (RASBASE+151)
	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASSHOST.PRG
DEFINE SECURITYMSG_SUCCESS     := 1
DEFINE SECURITYMSG_FAILURE     := 2
DEFINE SECURITYMSG_ERROR       := 3
DEFINE RASPBDEVENT_AddEntry    := 1
DEFINE RASPBDEVENT_EditEntry   := 2
DEFINE RASPBDEVENT_RemoveEntry := 3
DEFINE RASPBDEVENT_DialEntry   := 4
DEFINE RASPBDEVENT_EditGlobals := 5
DEFINE RASPBDEVENT_NoUser      := 6
DEFINE RASPBDEVENT_NoUserEdit  := 7
DEFINE RASPBDFLAG_PositionDlg      := 0x00000001
DEFINE RASPBDFLAG_ForceCloseOnDial := 0x00000002
DEFINE RASPBDFLAG_NoUser           := 0x00000010
DEFINE RASPBDFLAG_UpdateDefaults   := 0x80000000
DEFINE RASEDFLAG_PositionDlg := 0x00000001
DEFINE RASEDFLAG_NewEntry    := 0x00000002
DEFINE RASEDFLAG_CloneEntry  := 0x00000004
DEFINE RASEDFLAG_NoRename    := 0x00000008
DEFINE RASDDFLAG_PositionDlg := 0x00000001
DEFINE RASMDPAGE_Status            := 0
DEFINE RASMDPAGE_Summary           := 1
DEFINE RASMDPAGE_Preferences       := 2
DEFINE RASMDFLAG_PositionDlg       := 0x00000001
DEFINE RASMDFLAG_UpdateDefaults    := 0x80000000
DEFINE RASSAPI_MAX_PHONENUMBER_SIZE      := 128
DEFINE RASSAPI_MAX_MEDIA_NAME	         := 16
DEFINE RASSAPI_MAX_PORT_NAME	         := 16
DEFINE RASSAPI_MAX_DEVICE_NAME           := 128
DEFINE RASSAPI_MAX_DEVICETYPE_NAME       := 16
DEFINE RASSAPI_MAX_PARAM_KEY_SIZE        := 32
DEFINE RASPRIV_NoCallback        := 0x01
DEFINE RASPRIV_AdminSetCallback  := 0x02
DEFINE RASPRIV_CallerSetCallback := 0x04
DEFINE RASPRIV_DialinPrivilege   := 0x08
DEFINE RASPRIV_CallbackType  := 0x07
DEFINE	RAS_MODEM_OPERATIONAL	     := 1
DEFINE	RAS_MODEM_NOT_RESPONDING     := 2
DEFINE	RAS_MODEM_HARDWARE_FAILURE   := 3
DEFINE	RAS_MODEM_INCORRECT_RESPONSE := 4
DEFINE	RAS_MODEM_UNKNOWN 	     := 5
DEFINE	RAS_PORT_NON_OPERATIONAL := 1
DEFINE	RAS_PORT_DISCONNECTED	 := 2
DEFINE	RAS_PORT_CALLING_BACK    := 3
DEFINE	RAS_PORT_LISTENING	 := 4
DEFINE	RAS_PORT_AUTHENTICATING  := 5
DEFINE	RAS_PORT_AUTHENTICATED	 := 6
DEFINE	RAS_PORT_INITIALIZING	 := 7
DEFINE ParamNumber     := 0
DEFINE ParamString	    := 1
DEFINE MEDIA_UNKNOWN       := 0
DEFINE MEDIA_SERIAL        := 1
DEFINE MEDIA_RAS10_SERIAL  := 2
DEFINE MEDIA_X25           := 3
DEFINE MEDIA_ISDN          := 4
DEFINE USER_AUTHENTICATED    := 0x0001
DEFINE MESSENGER_PRESENT     := 0x0002
DEFINE PPP_CLIENT            := 0x0004
DEFINE GATEWAY_ACTIVE        := 0x0008
DEFINE REMOTE_LISTEN         := 0x0010
DEFINE PORT_MULTILINKED      := 0x0020
DEFINE RAS_IPADDRESSLEN  := 15
DEFINE RAS_IPXADDRESSLEN := 22
DEFINE RAS_ATADDRESSLEN  := 32
DEFINE RASDOWNLEVEL       := 10
DEFINE RASADMIN_35        := 35
DEFINE RASADMIN_CURRENT   := 40
DEFINE RCD_SingleUser  := 0
DEFINE RCD_AllUsers    := 0x00000001
DEFINE RCD_Eap         := 0x00000002
DEFINE RCD_Logon       := 0x00000004
	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\LMCONS.PRG
DEFINE CNLEN       := 15
DEFINE LM20_CNLEN  := 15
DEFINE DNLEN       := CNLEN
DEFINE LM20_DNLEN  := LM20_CNLEN
DEFINE UNCLEN      := (CNLEN+2)
DEFINE LM20_UNCLEN := (LM20_CNLEN+2)
DEFINE NNLEN       := 80
DEFINE LM20_NNLEN  := 12
DEFINE RMLEN       := (UNCLEN+1+NNLEN)
DEFINE LM20_RMLEN  := (LM20_UNCLEN+1+LM20_NNLEN)
DEFINE SNLEN       := 80
DEFINE LM20_SNLEN  := 15
DEFINE STXTLEN     := 256
DEFINE LM20_STXTLEN := 63
DEFINE PATHLEN     := 256
DEFINE LM20_PATHLEN := 256
DEFINE DEVLEN      := 80
DEFINE LM20_DEVLEN := 8
DEFINE EVLEN       := 16
DEFINE UNLEN       := 256
DEFINE LM20_UNLEN  := 20
DEFINE GNLEN       := UNLEN
DEFINE LM20_GNLEN  := LM20_UNLEN
DEFINE PWLEN       := 256
DEFINE LM20_PWLEN  := 14
DEFINE SHPWLEN     := 8
DEFINE CLTYPE_LEN  := 12
DEFINE MAXCOMMENTSZ      := 256
DEFINE LM20_MAXCOMMENTSZ := 48
DEFINE QNLEN       := NNLEN
DEFINE LM20_QNLEN  := LM20_NNLEN
DEFINE ALERTSZ     := 128
DEFINE MAXDEVENTRIES := 32
DEFINE NETBIOS_NAME_LEN  := 16
DEFINE MAX_PREFERRED_LENGTH    := ((DWORD) -1)
DEFINE CRYPT_KEY_LEN           := 7
DEFINE CRYPT_TXT_LEN           := 8
DEFINE ENCRYPTED_PWLEN         := 16
DEFINE SESSION_PWLEN           := 24
DEFINE SESSION_CRYPT_KLEN      := 21
DEFINE PARM_ERROR_UNKNOWN      := (-1)
DEFINE PARM_ERROR_NONE         := 0
DEFINE PARMNUM_BASE_INFOLEVEL  := 1000
DEFINE MESSAGE_FILENAME        := "NETMSG"
DEFINE OS2MSG_FILENAME         := "BASE"
DEFINE HELP_MSG_FILENAME       := "NETH"
DEFINE BACKUP_MSG_FILENAME     := "BAK.MSG"
DEFINE PLATFORM_ID_DOS := 300
DEFINE PLATFORM_ID_OS2 := 400
DEFINE PLATFORM_ID_NT  := 500
DEFINE PLATFORM_ID_OSF := 600
DEFINE PLATFORM_ID_VMS := 700
DEFINE MIN_LANMAN_MESSAGE_ID  := NERR_BASE
DEFINE MAX_LANMAN_MESSAGE_ID  := 5799
	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\LMERR.PRG
DEFINE NERR_Success            :=0
DEFINE NERR_BASE       :=2100
DEFINE NERR_NetNotStarted      := (NERR_BASE+2)
DEFINE NERR_UnknownServer      := (NERR_BASE+3)
DEFINE NERR_ShareMem           := (NERR_BASE+4)
DEFINE NERR_NoNetworkResource  := (NERR_BASE+5)
DEFINE NERR_RemoteOnly         := (NERR_BASE+6)
DEFINE NERR_DevNotRedirected   := (NERR_BASE+7)
DEFINE NERR_ServerNotStarted  :=  (NERR_BASE+14)
DEFINE NERR_ItemNotFound      :=  (NERR_BASE+15)
DEFINE NERR_UnknownDevDir     :=  (NERR_BASE+16)
DEFINE NERR_RedirectedPath    :=  (NERR_BASE+17)
DEFINE NERR_DuplicateShare    :=  (NERR_BASE+18)
DEFINE NERR_NoRoom            :=  (NERR_BASE+19)
DEFINE NERR_TooManyItems      :=  (NERR_BASE+21)
DEFINE NERR_InvalidMaxUsers   :=  (NERR_BASE+22)
DEFINE NERR_BufTooSmall       :=  (NERR_BASE+23)
DEFINE NERR_RemoteErr         :=  (NERR_BASE+27)
DEFINE NERR_LanmanIniError    :=  (NERR_BASE+31)
DEFINE NERR_NetworkError     :=   (NERR_BASE+36)
DEFINE NERR_WkstaInconsistentState  := (NERR_BASE+37)
DEFINE NERR_WkstaNotStarted    := (NERR_BASE+38)
DEFINE NERR_BrowserNotStarted  := (NERR_BASE+39)
DEFINE NERR_InternalError      := (NERR_BASE+40)
DEFINE NERR_BadTransactConfig  := (NERR_BASE+41)
DEFINE NERR_InvalidAPI         := (NERR_BASE+42)
DEFINE NERR_BadEventName       := (NERR_BASE+43)
DEFINE NERR_DupNameReboot      := (NERR_BASE+44)
DEFINE NERR_CfgCompNotFound    := (NERR_BASE+46)
DEFINE NERR_CfgParamNotFound   := (NERR_BASE+47)
DEFINE NERR_LineTooLong        := (NERR_BASE+49)
DEFINE NERR_QNotFound         :=  (NERR_BASE+50)
DEFINE NERR_JobNotFound       :=  (NERR_BASE+51)
DEFINE NERR_DestNotFound      :=  (NERR_BASE+52)
DEFINE NERR_DestExists        :=  (NERR_BASE+53)
DEFINE NERR_QExists           :=  (NERR_BASE+54)
DEFINE NERR_QNoRoom           :=  (NERR_BASE+55)
DEFINE NERR_JobNoRoom         :=  (NERR_BASE+56)
DEFINE NERR_DestNoRoom        :=  (NERR_BASE+57)
DEFINE NERR_DestIdle          :=  (NERR_BASE+58)
DEFINE NERR_DestInvalidOp     :=  (NERR_BASE+59)
DEFINE NERR_ProcNoRespond     :=  (NERR_BASE+60)
DEFINE NERR_SpoolerNotLoaded  :=  (NERR_BASE+61)
DEFINE NERR_DestInvalidState  :=  (NERR_BASE+62)
DEFINE NERR_QInvalidState     :=  (NERR_BASE+63)
DEFINE NERR_JobInvalidState   :=  (NERR_BASE+64)
DEFINE NERR_SpoolNoMemory     :=  (NERR_BASE+65)
DEFINE NERR_DriverNotFound    :=  (NERR_BASE+66)
DEFINE NERR_DataTypeInvalid   :=  (NERR_BASE+67)
DEFINE NERR_ProcNotFound      :=  (NERR_BASE+68)
DEFINE NERR_ServiceTableLocked := (NERR_BASE+80)
DEFINE NERR_ServiceTableFull   := (NERR_BASE+81)
DEFINE NERR_ServiceInstalled   := (NERR_BASE+82)
DEFINE NERR_ServiceEntryLocked := (NERR_BASE+83)
DEFINE NERR_ServiceNotInstalled := (NERR_BASE+84)
DEFINE NERR_BadServiceName     := (NERR_BASE+85)
DEFINE NERR_ServiceCtlTimeout  := (NERR_BASE+86)
DEFINE NERR_ServiceCtlBusy     := (NERR_BASE+87)
DEFINE NERR_BadServiceProgName := (NERR_BASE+88)
DEFINE NERR_ServiceNotCtrl     := (NERR_BASE+89)
DEFINE NERR_ServiceKillProc    := (NERR_BASE+90)
DEFINE NERR_ServiceCtlNotValid := (NERR_BASE+91)
DEFINE NERR_NotInDispatchTbl   := (NERR_BASE+92)
DEFINE NERR_BadControlRecv     := (NERR_BASE+93)
DEFINE NERR_ServiceNotStarting := (NERR_BASE+94)
DEFINE NERR_AlreadyLoggedOn    := (NERR_BASE+100)
DEFINE NERR_NotLoggedOn        := (NERR_BASE+101)
DEFINE NERR_BadUsername        := (NERR_BASE+102)
DEFINE NERR_BadPassword        := (NERR_BASE+103)
DEFINE NERR_UnableToAddName_W  := (NERR_BASE+104)
DEFINE NERR_UnableToAddName_F  := (NERR_BASE+105)
DEFINE NERR_UnableToDelName_W  := (NERR_BASE+106)
DEFINE NERR_UnableToDelName_F  := (NERR_BASE+107)
DEFINE NERR_LogonsPaused       := (NERR_BASE+109)
DEFINE NERR_LogonServerConflict := (NERR_BASE+110)
DEFINE NERR_LogonNoUserPath    := (NERR_BASE+111)
DEFINE NERR_LogonScriptError   := (NERR_BASE+112)
DEFINE NERR_StandaloneLogon    := (NERR_BASE+114)
DEFINE NERR_LogonServerNotFound := (NERR_BASE+115)
DEFINE NERR_LogonDomainExists  := (NERR_BASE+116)
DEFINE NERR_NonValidatedLogon  := (NERR_BASE+117)
DEFINE NERR_ACFNotFound        := (NERR_BASE+119)
DEFINE NERR_GroupNotFound      := (NERR_BASE+120)
DEFINE NERR_UserNotFound       := (NERR_BASE+121)
DEFINE NERR_ResourceNotFound   := (NERR_BASE+122)
DEFINE NERR_GroupExists        := (NERR_BASE+123)
DEFINE NERR_UserExists         := (NERR_BASE+124)
DEFINE NERR_ResourceExists     := (NERR_BASE+125)
DEFINE NERR_NotPrimary         := (NERR_BASE+126)
DEFINE NERR_ACFNotLoaded       := (NERR_BASE+127)
DEFINE NERR_ACFNoRoom          := (NERR_BASE+128)
DEFINE NERR_ACFFileIOFail      := (NERR_BASE+129)
DEFINE NERR_ACFTooManyLists    := (NERR_BASE+130)
DEFINE NERR_UserLogon          := (NERR_BASE+131)
DEFINE NERR_ACFNoParent        := (NERR_BASE+132)
DEFINE NERR_CanNotGrowSegment  := (NERR_BASE+133)
DEFINE NERR_SpeGroupOp         := (NERR_BASE+134)
DEFINE NERR_NotInCache         := (NERR_BASE+135)
DEFINE NERR_UserInGroup        := (NERR_BASE+136)
DEFINE NERR_UserNotInGroup     := (NERR_BASE+137)
DEFINE NERR_AccountUndefined   := (NERR_BASE+138)
DEFINE NERR_AccountExpired     := (NERR_BASE+139)
DEFINE NERR_InvalidWorkstation := (NERR_BASE+140)
DEFINE NERR_InvalidLogonHours  := (NERR_BASE+141)
DEFINE NERR_PasswordExpired    := (NERR_BASE+142)
DEFINE NERR_PasswordCantChange := (NERR_BASE+143)
DEFINE NERR_PasswordHistConflict := (NERR_BASE+144)
DEFINE NERR_PasswordTooShort   := (NERR_BASE+145)
DEFINE NERR_PasswordTooRecent  := (NERR_BASE+146)
DEFINE NERR_InvalidDatabase    := (NERR_BASE+147)
DEFINE NERR_DatabaseUpToDate   := (NERR_BASE+148)
DEFINE NERR_SyncRequired       := (NERR_BASE+149)
DEFINE NERR_UseNotFound        := (NERR_BASE+150)
DEFINE NERR_BadAsgType         := (NERR_BASE+151)
DEFINE NERR_DeviceIsShared     := (NERR_BASE+152)
DEFINE NERR_NoComputerName     := (NERR_BASE+170)
DEFINE NERR_MsgAlreadyStarted  := (NERR_BASE+171)
DEFINE NERR_MsgInitFailed      := (NERR_BASE+172)
DEFINE NERR_NameNotFound       := (NERR_BASE+173)
DEFINE NERR_AlreadyForwarded   := (NERR_BASE+174)
DEFINE NERR_AddForwarded       := (NERR_BASE+175)
DEFINE NERR_AlreadyExists      := (NERR_BASE+176)
DEFINE NERR_TooManyNames       := (NERR_BASE+177)
DEFINE NERR_DelComputerName    := (NERR_BASE+178)
DEFINE NERR_LocalForward       := (NERR_BASE+179)
DEFINE NERR_GrpMsgProcessor    := (NERR_BASE+180)
DEFINE NERR_PausedRemote       := (NERR_BASE+181)
DEFINE NERR_BadReceive         := (NERR_BASE+182)
DEFINE NERR_NameInUse          := (NERR_BASE+183)
DEFINE NERR_MsgNotStarted      := (NERR_BASE+184)
DEFINE NERR_NotLocalName       := (NERR_BASE+185)
DEFINE NERR_NoForwardName      := (NERR_BASE+186)
DEFINE NERR_RemoteFull         := (NERR_BASE+187)
DEFINE NERR_NameNotForwarded   := (NERR_BASE+188)
DEFINE NERR_TruncatedBroadcast := (NERR_BASE+189)
DEFINE NERR_InvalidDevice      := (NERR_BASE+194)
DEFINE NERR_WriteFault         := (NERR_BASE+195)
DEFINE NERR_DuplicateName      := (NERR_BASE+197)
DEFINE NERR_DeleteLater        := (NERR_BASE+198)
DEFINE NERR_IncompleteDel      := (NERR_BASE+199)
DEFINE NERR_MultipleNets       := (NERR_BASE+200)
DEFINE NERR_NetNameNotFound    := (NERR_BASE+210)
DEFINE NERR_DeviceNotShared    := (NERR_BASE+211)
DEFINE NERR_ClientNameNotFound := (NERR_BASE+212)
DEFINE NERR_FileIdNotFound     := (NERR_BASE+214)
DEFINE NERR_ExecFailure        := (NERR_BASE+215)
DEFINE NERR_TmpFile            := (NERR_BASE+216)
DEFINE NERR_TooMuchData        := (NERR_BASE+217)
DEFINE NERR_DeviceShareConflict := (NERR_BASE+218)
DEFINE NERR_BrowserTableIncomplete := (NERR_BASE+219)
DEFINE NERR_NotLocalDomain     := (NERR_BASE+220)
DEFINE NERR_IsDfsShare         := (NERR_BASE+221)
DEFINE NERR_DevInvalidOpCode   := (NERR_BASE+231)
DEFINE NERR_DevNotFound        := (NERR_BASE+232)
DEFINE NERR_DevNotOpen         := (NERR_BASE+233)
DEFINE NERR_BadQueueDevString  := (NERR_BASE+234)
DEFINE NERR_BadQueuePriority   := (NERR_BASE+235)
DEFINE NERR_NoCommDevs         := (NERR_BASE+237)
DEFINE NERR_QueueNotFound      := (NERR_BASE+238)
DEFINE NERR_BadDevString       := (NERR_BASE+240)
DEFINE NERR_BadDev             := (NERR_BASE+241)
DEFINE NERR_InUseBySpooler     := (NERR_BASE+242)
DEFINE NERR_CommDevInUse       := (NERR_BASE+243)
DEFINE NERR_InvalidComputer   := (NERR_BASE+251)
DEFINE NERR_MaxLenExceeded    := (NERR_BASE+254)
DEFINE NERR_BadComponent      := (NERR_BASE+256)
DEFINE NERR_CantType          := (NERR_BASE+257)
DEFINE NERR_TooManyEntries    := (NERR_BASE+262)
DEFINE NERR_ProfileFileTooBig  := (NERR_BASE+270)
DEFINE NERR_ProfileOffset      := (NERR_BASE+271)
DEFINE NERR_ProfileCleanup     := (NERR_BASE+272)
DEFINE NERR_ProfileUnknownCmd  := (NERR_BASE+273)
DEFINE NERR_ProfileLoadErr     := (NERR_BASE+274)
DEFINE NERR_ProfileSaveErr     := (NERR_BASE+275)
DEFINE NERR_LogOverflow         :=   (NERR_BASE+277)
DEFINE NERR_LogFileChanged      :=   (NERR_BASE+278)
DEFINE NERR_LogFileCorrupt      :=   (NERR_BASE+279)
DEFINE NERR_SourceIsDir  :=  (NERR_BASE+280)
DEFINE NERR_BadSource    :=  (NERR_BASE+281)
DEFINE NERR_BadDest      :=  (NERR_BASE+282)
DEFINE NERR_DifferentServers  :=  (NERR_BASE+283)
DEFINE NERR_RunSrvPaused       := (NERR_BASE+285)
DEFINE NERR_ErrCommRunSrv      := (NERR_BASE+289)
DEFINE NERR_ErrorExecingGhost  := (NERR_BASE+291)
DEFINE NERR_ShareNotFound      := (NERR_BASE+292)
DEFINE NERR_InvalidLana       :=  (NERR_BASE+300)
DEFINE NERR_OpenFiles         :=  (NERR_BASE+301)
DEFINE NERR_ActiveConns       :=  (NERR_BASE+302)
DEFINE NERR_BadPasswordCore   :=  (NERR_BASE+303)
DEFINE NERR_DevInUse          :=  (NERR_BASE+304)
DEFINE NERR_LocalDrive        :=  (NERR_BASE+305)
DEFINE NERR_AlertExists       :=  (NERR_BASE+330)
DEFINE NERR_TooManyAlerts     :=  (NERR_BASE+331)
DEFINE NERR_NoSuchAlert       :=  (NERR_BASE+332)
DEFINE NERR_BadRecipient      :=  (NERR_BASE+333)
DEFINE NERR_AcctLimitExceeded :=  (NERR_BASE+334)
DEFINE NERR_InvalidLogSeek    :=  (NERR_BASE+340)
DEFINE NERR_BadUasConfig      :=  (NERR_BASE+350)
DEFINE NERR_InvalidUASOp      :=  (NERR_BASE+351)
DEFINE NERR_LastAdmin         :=  (NERR_BASE+352)
DEFINE NERR_DCNotFound         := (NERR_BASE+353)
DEFINE NERR_LogonTrackingError := (NERR_BASE+354)
DEFINE NERR_NetlogonNotStarted := (NERR_BASE+355)
DEFINE NERR_CanNotGrowUASFile  := (NERR_BASE+356)
DEFINE NERR_TimeDiffAtDC       := (NERR_BASE+357)
DEFINE NERR_PasswordMismatch   := (NERR_BASE+358)
DEFINE NERR_NoSuchServer       := (NERR_BASE+360)
DEFINE NERR_NoSuchSession      := (NERR_BASE+361)
DEFINE NERR_NoSuchConnection   := (NERR_BASE+362)
DEFINE NERR_TooManyServers     := (NERR_BASE+363)
DEFINE NERR_TooManySessions    := (NERR_BASE+364)
DEFINE NERR_TooManyConnections := (NERR_BASE+365)
DEFINE NERR_TooManyFiles       := (NERR_BASE+366)
DEFINE NERR_NoAlternateServers := (NERR_BASE+367)
DEFINE NERR_TryDownLevel       := (NERR_BASE+370)
DEFINE NERR_UPSDriverNotStarted    := (NERR_BASE+380)
DEFINE NERR_UPSInvalidConfig       := (NERR_BASE+381)
DEFINE NERR_UPSInvalidCommPort     := (NERR_BASE+382)
DEFINE NERR_UPSSignalAsserted      := (NERR_BASE+383)
DEFINE NERR_UPSShutdownFailed      := (NERR_BASE+384)
DEFINE NERR_BadDosRetCode      := (NERR_BASE+400)
DEFINE NERR_ProgNeedsExtraMem  := (NERR_BASE+401)
DEFINE NERR_BadDosFunction     := (NERR_BASE+402)
DEFINE NERR_RemoteBootFailed   := (NERR_BASE+403)
DEFINE NERR_BadFileCheckSum    := (NERR_BASE+404)
DEFINE NERR_NoRplBootSystem    := (NERR_BASE+405)
DEFINE NERR_RplLoadrNetBiosErr := (NERR_BASE+406)
DEFINE NERR_RplLoadrDiskErr    := (NERR_BASE+407)
DEFINE NERR_ImageParamErr      := (NERR_BASE+408)
DEFINE NERR_TooManyImageParams := (NERR_BASE+409)
DEFINE NERR_NonDosFloppyUsed   := (NERR_BASE+410)
DEFINE NERR_RplBootRestart     := (NERR_BASE+411)
DEFINE NERR_RplSrvrCallFailed  := (NERR_BASE+412)
DEFINE NERR_CantConnectRplSrvr := (NERR_BASE+413)
DEFINE NERR_CantOpenImageFile  := (NERR_BASE+414)
DEFINE NERR_CallingRplSrvr     := (NERR_BASE+415)
DEFINE NERR_StartingRplBoot    := (NERR_BASE+416)
DEFINE NERR_RplBootServiceTerm := (NERR_BASE+417)
DEFINE NERR_RplBootStartFailed := (NERR_BASE+418)
DEFINE NERR_RPL_CONNECTED      := (NERR_BASE+419)
DEFINE NERR_BrowserConfiguredToNotRun    :=  (NERR_BASE+450)
DEFINE NERR_RplNoAdaptersStarted         :=  (NERR_BASE+510)
DEFINE NERR_RplBadRegistry               :=  (NERR_BASE+511)
DEFINE NERR_RplBadDatabase               :=  (NERR_BASE+512)
DEFINE NERR_RplRplfilesShare             :=  (NERR_BASE+513)
DEFINE NERR_RplNotRplServer              :=  (NERR_BASE+514)
DEFINE NERR_RplCannotEnum                :=  (NERR_BASE+515)
DEFINE NERR_RplWkstaInfoCorrupted        :=  (NERR_BASE+516)
DEFINE NERR_RplWkstaNotFound             :=  (NERR_BASE+517)
DEFINE NERR_RplWkstaNameUnavailable      :=  (NERR_BASE+518)
DEFINE NERR_RplProfileInfoCorrupted      :=  (NERR_BASE+519)
DEFINE NERR_RplProfileNotFound           :=  (NERR_BASE+520)
DEFINE NERR_RplProfileNameUnavailable    :=  (NERR_BASE+521)
DEFINE NERR_RplProfileNotEmpty           :=  (NERR_BASE+522)
DEFINE NERR_RplConfigInfoCorrupted       :=  (NERR_BASE+523)
DEFINE NERR_RplConfigNotFound            :=  (NERR_BASE+524)
DEFINE NERR_RplAdapterInfoCorrupted      :=  (NERR_BASE+525)
DEFINE NERR_RplInternal                  :=  (NERR_BASE+526)
DEFINE NERR_RplVendorInfoCorrupted       :=  (NERR_BASE+527)
DEFINE NERR_RplBootInfoCorrupted         :=  (NERR_BASE+528)
DEFINE NERR_RplWkstaNeedsUserAcct        :=  (NERR_BASE+529)
DEFINE NERR_RplNeedsRPLUSERAcct          :=  (NERR_BASE+530)
DEFINE NERR_RplBootNotFound              :=  (NERR_BASE+531)
DEFINE NERR_RplIncompatibleProfile       :=  (NERR_BASE+532)
DEFINE NERR_RplAdapterNameUnavailable    :=  (NERR_BASE+533)
DEFINE NERR_RplConfigNotEmpty            :=  (NERR_BASE+534)
DEFINE NERR_RplBootInUse                 :=  (NERR_BASE+535)
DEFINE NERR_RplBackupDatabase            :=  (NERR_BASE+536)
DEFINE NERR_RplAdapterNotFound           :=  (NERR_BASE+537)
DEFINE NERR_RplVendorNotFound            :=  (NERR_BASE+538)
DEFINE NERR_RplVendorNameUnavailable     :=  (NERR_BASE+539)
DEFINE NERR_RplBootNameUnavailable       :=  (NERR_BASE+540)
DEFINE NERR_RplConfigNameUnavailable     :=  (NERR_BASE+541)
DEFINE NERR_DfsInternalCorruption       :=   (NERR_BASE+560)
DEFINE NERR_DfsVolumeDataCorrupt        :=   (NERR_BASE+561)
DEFINE NERR_DfsNoSuchVolume             :=   (NERR_BASE+562)
DEFINE NERR_DfsVolumeAlreadyExists      :=   (NERR_BASE+563)
DEFINE NERR_DfsAlreadyShared            :=   (NERR_BASE+564)
DEFINE NERR_DfsNoSuchShare              :=   (NERR_BASE+565)
DEFINE NERR_DfsNotALeafVolume           :=   (NERR_BASE+566)
DEFINE NERR_DfsLeafVolume               :=   (NERR_BASE+567)
DEFINE NERR_DfsVolumeHasMultipleServers :=   (NERR_BASE+568)
DEFINE NERR_DfsCantCreateJunctionPoint  :=   (NERR_BASE+569)
DEFINE NERR_DfsServerNotDfsAware        :=   (NERR_BASE+570)
DEFINE NERR_DfsBadRenamePath            :=   (NERR_BASE+571)
DEFINE NERR_DfsVolumeIsOffline          :=   (NERR_BASE+572)
DEFINE NERR_DfsInternalError            :=   (NERR_BASE+590)
DEFINE MAX_NERR             :=    (NERR_BASE+899)
#endregion
