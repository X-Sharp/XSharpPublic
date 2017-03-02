// 442. error XS0133: The expression being assigned to '_winRASNOUSER.szDomain' must be constant
/*
error appears randomly, compiler reports different error line and message after _EVERY_ attempted compilation.
some indicative error messages that may be reported:

C442.prg(1162,23): error XS0133: The expression being assigned to '_winRASNOUSER.szDomain' must be constant
C442.prg(1114,23): error XS0133: The expression being assigned to '_winRASCREDENTIALS.szDomain' must be constant
C442.prg(1255,28): error XS0133: The expression being assigned to '_winRAS_PORT_0.wszLogonDomain' must be constant
*/

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



DEFINE RAS_IPADDRESSLEN  := 15
DEFINE RAS_IPXADDRESSLEN := 22
DEFINE RAS_ATADDRESSLEN  := 32

DEFINE RASDOWNLEVEL       := 10
DEFINE RASADMIN_35        := 35
DEFINE RASADMIN_CURRENT   := 40


DEFINE CNLEN       := 1
DEFINE DNLEN       := CNLEN
DEFINE UNLEN       := 256
DEFINE PWLEN       := 256
DEFINE NETBIOS_NAME_LEN  := 16


FUNC RASADFUNC( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASADPARAMS, P4 AS DWORD PTR ) AS LOGIC
	RETURN FALSE

	/* A RAS phone book multilinked sub-entry.
	*/
_DLL FUNC RasDial( P1 AS _winRASDIALEXTENSIONS, P2 AS PSZ, P3 AS _winRASDIALPARAMS, P4 AS DWORD, P5 AS PTR,;
		P6 AS PTR ) AS DWORD STRICT:RASAPI32.RasDialA


_DLL FUNC RasEnumConnections( P1 AS _winRASCONN, P2 AS DWORD PTR, P3 AS DWORD PTR);
		AS DWORD STRICT:RASAPI32.RasEnumConnectionsA


_DLL FUNC  RasEnumEntries( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRYNAME, P4 AS DWORD  PTR, P5 AS DWORD PTR );
		AS DWORD STRICT:RASAPI32.RasEnumEntriesA


_DLL FUNC RasGetConnectStatus( P1 AS PTR, P2 AS _winRASCONNSTATUS ) AS DWORD STRICT:RASAPI32.RasGetConnectStatusA


_DLL FUNC RasGetErrorString( P1 AS DWORD, P2 AS PSZ, P3 AS DWORD ) AS DWORD STRICT:RASAPI32.RasGetErrorStringA


_DLL FUNC RasHangUp( P1 AS PTR) AS DWORD STRICT:RASAPI32.RasHangUpA


_DLL FUNC RasGetProjectionInfo(P1 AS PTR, P2 AS DWORD, P3 AS PTR, P4 AS DWORD PTR);
		AS DWORD STRICT:RASAPI32.RasGetProjectionInfoA


_DLL FUNC RasCreatePhonebookEntry( P1 AS PTR , P2 AS PSZ ) AS DWORD STRICT:RASAPI32.RasCreatePhonebookEntryA


_DLL FUNC RasEditPhonebookEntry( P1 AS PTR, P2 AS PSZ, P3 AS PSZ ) AS DWORD STRICT:RASAPI32.RasEditPhonebookEntryA


_DLL FUNC RasSetEntryDialParams(P1 AS PSZ, P2 AS _winRASDIALPARAMS, P3 AS LOGIC);
		AS DWORD STRICT:RASAPI32.RasSetEntryDialParamsA


_DLL FUNC RasGetEntryDialParams( P1 AS PSZ, P2 AS _winRASDIALPARAMS, P3 AS LOGIC PTR );
		AS DWORD STRICT:RASAPI32.RasGetEntryDialParamsA


_DLL FUNC RasEnumDevices( P1 AS _winRASDEVINFO, P2 AS DWORD PTR, P3 AS DWORD PTR );
		AS DWORD STRICT:RASAPI32.RasEnumDevicesA


_DLL FUNC RasGetCountryInfo( P1 AS _winRASCTRYINFO, P2 AS DWORD PTR) AS DWORD STRICT:RASAPI32.RasGetCountryInfoA


_DLL FUNC RasGetEntryProperties( P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRY, P4 AS DWORD PTR, P5 AS BYTE PTR, P6 AS DWORD PTR );
		AS DWORD STRICT:RASAPI32.RasGetEntryPropertiesA


_DLL FUNC RasSetEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASENTRY, P4 AS DWORD, P5 AS BYTE PTR, P6 AS DWORD );
		AS DWORD STRICT:RASAPI32.RasSetEntryPropertiesA

_DLL FUNC RasRenameEntry(P1 AS PSZ, P2 AS PSZ, P3 AS PSZ ) AS DWORD STRICT:RASAPI32.RasRenameEntryA


_DLL FUNC RasDeleteEntry(P1 AS PSZ, P2 AS PSZ ) AS DWORD STRICT:RASAPI32.RasDeleteEntry


_DLL FUNC RasValidateEntryName(P1 AS PSZ, P2 AS PSZ) AS DWORD STRICT:RASAPI32.RasValidateEntryNameA


_DLL FUNC RasGetSubEntryHandle(P1 AS PTR, P2 AS DWORD, P3 AS PTR ) AS DWORD STRICT:RASAPI32.RasGetSubEntryHandleA


_DLL FUNC RasGetCredentials(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASCREDENTIALS);
		AS DWORD STRICT:RASAPI32.RasGetCredentialsA


_DLL FUNC RasSetCredentials(P1 AS PSZ, P2 AS PSZ, P3 AS _winRASCREDENTIALS, P4 AS LOGIC );
		AS DWORD STRICT:RASAPI32.RasSetCredentialsA


_DLL FUNC RasConnectionNotification(P1 AS PTR, P2 AS PTR, P3 AS DWORD) AS DWORD STRICT:RASAPI.RasConnectionNotification


_DLL FUNC RasGetSubEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS DWORD,;
		P4 AS _winRASSUBENTRY, P5 AS DWORD PTR , P6 AS BYTE PTR, P7 AS DWORD PTR);
		AS DWORD STRICT:RASAPI32.RasGetSubEntryPropertiesA


_DLL FUNC RasSetSubEntryProperties(P1 AS PSZ, P2 AS PSZ, P3 AS DWORD,;
		P4 AS _winRASSUBENTRY, P5 AS DWORD, P6 AS BYTE PTR,;
		P7 AS DWORD ) AS DWORD STRICT:RASAPI32.RasSetSubEntryProperties


_DLL FUNC RasGetAutodialAddress(P1 AS PSZ, P2 AS DWORD PTR, P3 AS _winRASAUTODIALENTRY,;
		P4 AS DWORD PTR, P5 AS DWORD PTR );
		AS DWORD STRICT:RASAPI32.RasGetAutodialAddressA


_DLL FUNC RasSetAutodialAddress( P1 AS PSZ, P2 AS DWORD, P3 AS _winRASAUTODIALENTRY,;
		P4 AS DWORD, P5 AS DWORD );
		AS DWORD STRICT:RASAPI.RasSetAutodialAddressA


_DLL FUNC RasEnumAutodialAddresses( P1 AS PTR, P2 AS DWORD PTR, P3 AS DWORD PTR);
		AS DWORD STRICT:RASAPI32.RasEnumAutodialAddressesA


_DLL FUNC  RasGetAutodialEnable(P1 AS  DWORD, P2 AS LOGIC PTR ) AS DWORD STRICT:RASAPI32.RasGetAutodialEnableA


_DLL FUNC RasSetAutodialEnable(P1 AS  DWORD, P2 AS LOGIC ) AS DWORD STRICT:RASAPI32.RasSetAutodialEnableA


_DLL FUNC RasGetAutodialParam( P1 AS DWORD, P2 AS PTR, P3 AS DWORD PTR);
		AS DWORD STRICT:RASAPI32.RasGetAutodialParamA


_DLL FUNC RasSetAutodialParam(P1 AS DWORD, P2 AS PTR, P3 AS DWORD ) AS DWORD STRICT:RASAPI32.RasSetAutodialParamA


	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASERROR.PRG




FUNC RASSECURITYPROC() AS DWORD
	RETURN 0











_DLL FUNC RasSecurityDialogSend(HPort AS DWORD, pBuffer AS BYTE PTR, BufferLength AS WORD );
		AS DWORD STRICT:RASMAN.RasSecurityDialogSend


_DLL FUNC RasSecurityDialogReceive(hPort AS DWORD , pBuffer AS BYTE PTR,pBufferLength AS WORD PTR,;
		Timeout AS DWORD, hEvent AS PTR);
		AS DWORD STRICT:RASMAN.RasSecurityDialogReceive


_DLL FUNC RasSecurityDialogGetInfo(hPort AS DWORD, pBuffer AS _winRAS_SECURITY_INFO);
		AS DWORD STRICT:RASMAN.RasSecurityDialogGetInfo

	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASDLG.PRG









FUNC RASPBDLGFUNC(P1 AS  DWORD, P2 AS DWORD, P3 AS PSZ, P4 AS PTR ) AS VOID STRICT
	RETURN

_DLL FUNC RasPhonebookDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpInfo AS _winRASPBDLG );
		AS LOGIC STRICT:RASDLG.RasPhonebookDlgA


_DLL FUNC RasEntryDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpInfo AS _winRASENTRYDLG);
		AS LOGIC STRICT:RASDLG.RasEntryDlg


	// RvdH 050710 Next line was missing the 'A' for ANsi Version
_DLL FUNC RasDialDlg(lpszPhonebook AS PSZ, lpszEntry AS PSZ, lpszPhoneNumber AS PSZ,;
		lpInfo  AS _winRASDIALDLG) AS LOGIC STRICT:RASDLG.RasDialDlgA


_DLL FUNC RasMonitorDlg(lpszDeviceName AS PSZ, lpInfo AS _winRASMONITORDLG );
		AS LOGIC STRICT:RASDLG.RasMonitorDlg


	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\RASSAPI.PRG















_DLL FUNC RasAdminServerGetInfo(lpszServer AS PSZ, pRasServer0 AS _winRAS_SERVER_0);
		AS DWORD STRICT:RASSAPI.RasAdminServerGetInfo

_DLL FUNC RasAdminGetUserAccountServer(lpszDomain AS PSZ, lpszServer AS PSZ,;
		lpszUserAccountServer  AS PSZ  ) AS DWORD STRICT:RASSAPI.RasAdminGetUserAccountServer

_DLL FUNC RasAdminUserGetInfo(lpszUserAccountServer AS PSZ, lpszUser AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD STRICT:RASSAPI.RasAdminUserGetInfo

_DLL FUNC RasAdminUserSetInfo(lpszUserAccountServer AS PSZ, lpszUser AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD STRICT:RASSAPI.RasAdminUserSetInfo

_DLL FUNC RasAdminPortEnum( lpszServer AS PSZ, ppRasPort0 AS _winRAS_PORT_0, pcEntriesRead AS WORD PTR);
		AS DWORD STRICT:RASSAPI.RasAdminPortEnum

_DLL FUNC RasAdminPortGetInfo(lpszServer AS PSZ, lpszPort AS PSZ,  pRasPort1 AS _winRAS_PORT_1,;
		pRasStats AS _winRAS_PORT_STATISTICS, ppRasParams AS PTR);
		AS DWORD STRICT:RASSAPI.RasAdminPortGetInfo

_DLL FUNC RasAdminPortClearStatistics(lpszServer AS PSZ, lpszPort AS PSZ);
		AS DWORD STRICT:RASSAPI.RasAdminPortClearStatistics

_DLL FUNC RasAdminPortDisconnect(lpszServer AS PSZ, lpszPort AS PSZ);
		AS DWORD STRICT:RASSAPI.RasAdminPortDisconnect

_DLL FUNC RasAdminFreeBuffer(Pointer AS PTR) AS DWORD STRICT:RASSAPI.RasAdminFreeBuffer

_DLL FUNC RasAdminGetErrorString(ResourceId AS DWORD, lpszString AS PSZ, InBufSize AS DWORD);
		AS DWORD STRICT:RASSAPI.RasAdminGetErrorString






_DLL FUNC RasAdminGetUserParms(lpszParms AS PSZ, pRasUser0 AS _winRAS_USER_0);
		AS DWORD STRICT:RASSAPI.RasAdminGetUserParms
_DLL FUNC  RasAdminSetUserParms(lpszParms AS PSZ, cchNewParms AS DWORD, pRasUser0 AS _winRAS_USER_0);
		AS DWORD STRICT:RASSAPI.RasAdminSetUserParms


VOSTRUCT _WINRASCONN 
	MEMBER dwSize AS DWORD
	MEMBER hrasconn AS PTR
	MEMBER DIM szEntryName[ RAS_MaxEntryName + 1 ] AS BYTE
	MEMBER DIM szDeviceType[ RAS_MaxDeviceType + 1 ] AS BYTE
	MEMBER DIM szDeviceName[ RAS_MaxDeviceName + 1 ] AS BYTE
	MEMBER dwSubEntry AS DWORD 
	// RvdH 070411 added
	MEMBER dwFlags	AS DWORD


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
	//
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
	MEMBER DIM szPrerequisiteEntry[RAS_MaxEntryName + 1] AS BYTE
	MEMBER dwRedialCount AS DWORD
	MEMBER dwRedialPause  AS DWORD

VOSTRUCT _winRASADPARAMS // RvdH 070411 removed alignment  
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONG
	MEMBER yDlg AS LONG



	/* AutoDial DLL function parameter block 'dwFlags.'
	*/
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





VOSTRUCT _winRASPBDLG 		// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONG
	MEMBER yDlg AS LONG
	MEMBER dwCallbackId AS DWORD
	MEMBER pCallback AS PTR
	MEMBER dwError	AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD





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


VOSTRUCT _winRASDIALDLG 		// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER xDlg AS LONG
	MEMBER yDlg AS LONG
	MEMBER dwSubEntry AS DWORD
	MEMBER dwError AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD





VOSTRUCT _winRASMONITORDLG 	// RvdH 070411 removed alignment 
	MEMBER dwSize AS DWORD
	MEMBER hwndOwner AS PTR
	MEMBER dwFlags AS DWORD
	MEMBER dwStartPage AS DWORD
	MEMBER xDlg AS LONG
	MEMBER yDlg AS LONG
	MEMBER dwError AS DWORD
	MEMBER reserved AS DWORD
	MEMBER reserved2 AS DWORD




UNION u_winRAS_PARAMS_VALUE

	MEMBER	Number  AS DWORD


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

